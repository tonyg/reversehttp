-module(reflect_meta).

-export([handle/4]).

-define(RPC_TIMEOUT, 10000).

handle(Req, AP, [], _QueryFields) ->
    reply_with(Req, "Access Point",
               gen_info(Req, AP) ++
               [{h2, <<"Registered Labels">>},
                {table, [{class, "meta-table"}],
                 [{tr, [], [{th, X} ||
                               X <- [<<"Label root">>, <<"Details">>]]}] ++
                 [format_vhost(AP, X) || X <- all_vhosts()]}]).

gen_info(Req, AP) ->
    [{h2, <<"General Info">>},
     {p, [<<"Access point info: ">>, hlink(apurl(Req, AP))]},
     {p, [<<"Site root: ">>, hlink("http://" ++ Req:get_header_value(host) ++ "/")]}].

apurl(Req, AP) ->
    "http://" ++ Req:get_header_value(host) ++ AP.

vhroot(VHost) ->
    "http://" ++ VHost ++ "/".

%% vhurl(AP, VHost) ->
%%     [Label | Labels] = string:tokens(VHost, "."),
%%     "http://" ++ string:join(Labels, ".") ++ AP ++ "/" ++ Label.

template(Title, BodyElts) ->
    {html, 
     [{head,
       [{title, [], [Title]},
        {link, [{rel, "stylesheet"},
                {href, "/style.css"},
                {type, "text/css"}], []}]},
      {body,
       [{'div', [{id, "outerContainer"}],
         [{'div', [{id, "innerContainer"}],
           [{h1, [], [Title]}] ++
           BodyElts}]}]}]}.

hlink(Href) ->
    hlink(Href, Href).

hlink(Href, Label) ->
    {a, [{href, Href}], [Label]}.

reply_with(Req, Title, BodyElts) ->
    reply_with(Req, template(Title, BodyElts)).

reply_with(Req, Html) ->
    Req:respond({200, [], mochiweb_html:to_html(Html)}).

all_vhosts() ->
    reflect_vhost_manager:lookup().

%% all_requests(HostLabel) ->
%%     [ {RN, global:whereis_name(N)} ||
%%      (N = {reflect_vhost_manager, request, H, RN}) <- global:registered_names(),
%%      H =:= HostLabel ].

keyget(K, L, Def) ->
    case lists:keysearch(K, 1, L) of
        {value, {_, V}} -> V;
        false -> Def
    end.

explain_status(infinity, PollerCount, _RequestCount) ->
    integer_to_list(PollerCount) ++
        case PollerCount of
            1 -> " poller";
            _ -> " pollers"
        end;
explain_status(ExpiryMs, _PollerCount, RequestCount) ->
    "will expire in " ++ integer_to_list(ExpiryMs) ++
        case RequestCount of
            0 -> "ms";
            _ -> "ms; " ++ integer_to_list(RequestCount) ++ " request(s) waiting"
        end.

format_vhost(_AP, VHost) ->
    Info = reflect_vhost_manager:info(VHost),
    {tr, [],
     [{td, [], [hlink(vhroot(VHost))]},
      {td, [], [explain_status(keyget(expiry_ms, Info, 0),
                               keyget(poller_count, Info, 0),
                               keyget(request_count, Info, 0))]}]}.
