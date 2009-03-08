-module(reflect_request_queue).

-export([handle/2]).

-define(ENABLE_META, true).

-record(poll_response, {requesting_client, formatted_request}).

%% A reversehttp request can be one of the following:
%%
%%  - a request for real content from the main vhost
%%  - a request to set up a virtual host
%%  - a poll for requests sent to a virtual host
%%  - a reply to a request sent to a virtual host
%%  - a tunnelled outbound request to be relayed
%%
%% First and foremost, if it's a request for a virtual host, we need
%% to pass it through unmolested. Otherwise, we get to examine the
%% request further to see which of the other four categories it falls
%% into.

handle(Req, ExceptionHosts) ->
    case Req:get_header_value(host) of
        undefined ->
            error(Req, 400, "Missing Host HTTP header");
        Host ->
            case lists:keysearch(Host, 1, ExceptionHosts) of
                {value, {_, AccessPoints}} ->
                    %% The request was for one of our configured
                    %% access-point hosts. Now, check the raw_path to
                    %% see if it matches an access-point path.
                    handle_exception_host(Req, AccessPoints);
                false ->
                    single_request(Req, Host)
            end
    end.

%%--------------------------------------------------------------------

handle_exception_host(Req, AccessPoints) ->
    {Path, QueryPart, _Fragment} = mochiweb_util:urlsplit_path(Req:get(raw_path)),
    case find_access_point(Path, AccessPoints) of
        {ok, AccessPoint, PathComponents} ->
            %% The request was for one of our access points.
            QueryFields = mochiweb_util:parse_qs(QueryPart),
            handle_reverse_http(Req,
                                Req:get(method),
                                AccessPoint,
                                PathComponents,
                                QueryFields);
        {error, not_found} ->
            %% It's a request for content from the main vhost(s). The
            %% special token exception_host is passed back to our
            %% caller to indicate that they should serve content as
            %% usual.
            exception_host
    end.

single_request(Req, Host) ->
    case request_host(Req) of
        {error, Reason} ->
            error(Req, 400, "Could not determine your IP address", Reason);
        {ok, RequestHost} ->
            FormattedRequest = format_req(Req, Req:recv_body()),
            Msg = #poll_response{requesting_client = RequestHost,
                                 formatted_request = FormattedRequest},
            case reflect_vhost_manager:request(Host, Msg) of
                {error, not_found} ->
                    error(Req, 404, "Virtual host not found", Host);
                {error, noproc} ->
                    error(Req, 503, "Virtual host manager crashed", Msg);
                {error, {timeout, poller}} ->
                    error(Req, 504, "No available servers");
                {error, {timeout, downstream}} ->
                    error(Req, 504, "Timeout waiting for response from downstream");
                {error, Reason} ->
                    error(Req, 500, "Internal contract error",
                          {reflect_vhost_manager, request, Msg, Reason});
                {ok, undefined} ->
                    error(Req, 502, "Missing response from downstream");
                {ok, Response} ->
                    case check_response(Response) of
                        {error, Reason} ->
                            error(Req, 502, "Bad response from downstream", Reason);
                        {ok, MochiResponse} ->
                            Req:respond(MochiResponse)
                    end
            end
    end.

handle_reverse_http(Req, 'POST', _AP, ["_relay", HostAndPort], _QueryFields) ->
    case extract_host_and_port(HostAndPort) of
        {error, _} ->
            error(Req, 400, "Bad host:port in relay request");
        {ok, Host, Port} ->
            case Req:get_header_value('content-type') of
                undefined ->
                    error(Req, 415, "Missing Content-type header");
                "message/http" ++ _ ->
                    case relay_http:relay(Host, list_to_integer(Port), Req:recv_body()) of
                        {ok, Response} ->
                            Req:respond({200, [{'Content-type', "message/http"}], Response});
                        {error, Code, Text} ->
                            error(Req, Code, Text)
                    end;
                Other ->
                    error(Req, 415, "Invalid Content-type header", Other)
            end
    end;

handle_reverse_http(Req, 'POST', AP, [], UrlQueryFields) ->
    BodyQueryFields = case Req:recv_body() of
                          undefined -> [];
                          V -> mochiweb_util:parse_qs(V)
                      end,
    QueryFields = BodyQueryFields ++ UrlQueryFields,
    case lists:keysearch("name", 1, QueryFields) of
        {value, {_, HostLabel}} ->
            Token = case lists:keysearch("token", 1, QueryFields) of
                        {value, {_, T}} -> T;
                        false -> random_id(HostLabel)
                    end,
            LeaseSecondsStr = case lists:keysearch("lease", 1, QueryFields) of
                                  {value, {_, L}} -> L;
                                  false -> "0"
                              end,
            case catch list_to_integer(LeaseSecondsStr) of
                {'EXIT', _} ->
                    error(Req, 400, "Invalid lease seconds setting", LeaseSecondsStr);
                LeaseSeconds ->
                    ReqName = random_id({HostLabel, Token}),
                    Headers = [link_header(format_request_url(Req,
                                                              AP, HostLabel, Token, ReqName),
                                           "first"),
                               link_header(format_ext_vhost_url(Req, HostLabel),
                                           "related"),
                               {"Location", format_int_vhost_url(Req, AP, HostLabel, Token)}],
                    Host = expand_host_label(Req, HostLabel),
                    case reflect_vhost_manager:configure(Host, Token, LeaseSeconds) of
                        {ok, existing, _Pid} ->
                            Req:respond({204, Headers, []});
                        {ok, new, _Pid} ->
                            Req:respond({201, Headers, []});
                        {error, bad_token} ->
                            error(Req, 403, "Bad token", HostLabel);
                        {error, Reason} ->
                            error(Req, 502, "Could not configure vhost manager", {Host, Reason})
                    end
            end;
        false ->
            error(Req, 400, "Missing label parameter", QueryFields)
    end;

handle_reverse_http(Req, 'GET', AP, [HostLabel, Token, ReqName], _QueryFields) ->
    NextReqName = random_id({HostLabel, Token}),
    Headers = [link_header(format_request_url(Req, AP, HostLabel, Token, NextReqName), "next")],
    Host = expand_host_label(Req, HostLabel),
    case reflect_vhost_manager:poll(
           Host, Token, ReqName,
           fun (#poll_response{requesting_client = RC, formatted_request = FR}) ->
                   Req:respond({200,
                                Headers ++ [{'Content-type', "message/http"},
                                            {'Requesting-Client', RC}],
                                FR}),
                   ok
           end) of
        {error, noproc} ->
            error(Req, 503, "Virtual host manager crashed", HostLabel);
        {error, timeout} ->
            Req:respond({204, Headers, []});
        {error, bad_token} ->
            error(Req, 403, "Bad token", HostLabel);
        {error, Reason} ->
            error(Req, 500, "Internal contract error", {reflect_vhost_manager, poll, Reason});
        ok ->
            ok
    end;

handle_reverse_http(Req, 'POST', _AP, [HostLabel, _Token, ReqName], _QueryFields) ->
    case reflect_vhost_manager:respond(
           ReqName,
           fun () ->
                   case Req:recv_body() of
                       undefined -> {error, missing_body};
                       Response -> {ok, Response}
                   end
           end) of
        {error, not_found} ->
            error(Req, 404, "Request key not found", {HostLabel, ReqName});
        {error, missing_body} ->
            error(Req, 400, "Missing response", {HostLabel, ReqName});
        ok ->
            Req:respond({202, [], []})
    end;

handle_reverse_http(Req, 'GET', AP, PathComponents, QueryFields) ->
    handle_meta(Req, AP, PathComponents, QueryFields);

handle_reverse_http(Req, Method, AP, PathComponents, QueryFields) ->
    error(Req, 400, "Bad Reverse-HTTP Request", {Method, AP, PathComponents, QueryFields}).

-ifdef(ENABLE_META).
handle_meta(Req, AP, PathComponents, QueryFields) ->
    reflect_meta:handle(Req, AP, PathComponents, QueryFields).
-else.
handle_meta(Req, AP, PathComponents, QueryFields) ->
    ok.
-endif.

%%--------------------------------------------------------------------

expand_host_label(Req, HostLabel) ->
    HostLabel ++ "." ++ Req:get_header_value(host).

format_ext_vhost_url(Req, HostLabel) ->
    "http://" ++ expand_host_label(Req, HostLabel) ++ "/".

format_int_vhost_url(Req, AccessPoint, HostLabel, Token) ->
    "http://" ++ Req:get_header_value(host) ++
        AccessPoint ++ "/" ++ HostLabel ++ "/" ++ Token.

format_request_url(Req, AccessPoint, HostLabel, Token, ReqName) ->
    "http://" ++ Req:get_header_value(host) ++
        AccessPoint ++ "/" ++ HostLabel ++ "/" ++ Token ++ "/" ++ ReqName.

link_header(Url, Rel) ->
    {'Link', "<" ++ Url ++ ">; rel=\"" ++ Rel ++ "\""}.

random_id(Term) ->
    mochihex:to_hex(erlang:md5(term_to_binary({Term, erlang:now()}))).

error(Req, StatusCode, ExplanatoryBodyText) ->
    error(Req, StatusCode, ExplanatoryBodyText, []).

error(Req, StatusCode, ExplanatoryBodyText, ExtraInfo) ->
    error_logger:warning_report({?MODULE, error,
                                 {client, request_host(Req)},
                                 {reply, StatusCode, ExplanatoryBodyText},
                                 ExtraInfo}),
    Req:respond({StatusCode, [], integer_to_list(StatusCode) ++ " " ++ ExplanatoryBodyText}).

find_access_point(_Path, []) ->
    {error, not_found};
find_access_point(Path, [AccessPoint | AccessPoints]) ->
    case lists:prefix(AccessPoint, Path) of
        true ->
            StrippedPath = string:substr(Path, length(AccessPoint) + 1),
            PathComponents = string:tokens(StrippedPath, "/"),
            {ok, AccessPoint, PathComponents};
        false ->
            find_access_point(Path, AccessPoints)
    end.

request_host(Req) ->
    Sock = Req:get(socket),
    case inet:peername(Sock) of
        {ok, {Addr, Port}} ->
            {ok, inet_parse:ntoa(Addr) ++ ":" ++ integer_to_list(Port)};
        {error, Reason} ->
            {error, Reason}
    end.

extract_host_and_port(HostAndPort) ->
    case string:tokens(HostAndPort, ":") of
        [H, P] -> {ok, H, P};
        [H] -> {ok, H, "80"};
        _ -> {error, invalid}
    end.

check_response(Bytes) ->
    case httpc_response:parse([Bytes, nolimit, true]) of
        {ok, {_HttpVersion, StatusCode, StatusText, Headers, Body}} ->
            {ok, {[integer_to_list(StatusCode), " ", StatusText],
                  http_response:header_list(Headers),
                  Body}};
        Other ->
            {error, Other}
    end.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

format_req(Req, Body) ->
    F = fun ({K, V}, Acc) -> [make_io(K), <<": ">>, V, <<"\r\n">> | Acc] end,
    Headers = lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Req:get(headers))),
    MethodLine = mochifmt:format("{0} {1} HTTP/{2.0}.{2.1}\r\n",
                                 [Req:get(method), Req:get(raw_path), Req:get(version)]),
    case Body of
        undefined ->
            [MethodLine, Headers];
        _ ->
            [MethodLine, Headers, Body]
    end.
