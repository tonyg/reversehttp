-module(reflect_meta).

-export([handle/5]).

-define(RPC_TIMEOUT, 10000).

handle(Req, Config, AccessUrl, [], _QueryFields) ->
    reply_with(Req, "Access Point",
               gen_info(Req, Config, AccessUrl) ++
               [{h2, <<"Registered Delegations">>},
                {table, [{class, "meta-table"}],
                 [{tr, [], [{th, X} ||
                               X <- [<<"Root URL">>, <<"Details">>]]}] ++
                 [format_vhost(X) || X <- all_vhosts()]}]);
handle(Req, _Config, _AccessUrl, _PathComponents, _QueryFields) ->
    Req:not_found().

gen_info(Req, Config, AccessUrl) ->
    [{h2, <<"General Info">>},
     {p, [<<"Access point info: ">>, hlink(AccessUrl)]},
     {p, [<<"Site root: ">>, hlink("http://" ++ reversehttp:host(Req, Config) ++ "/")]}].

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
    Req:respond({200, [{'Content-type', "text/html"}], mochiweb_html:to_html(Html)}).

all_vhosts() ->
    reflect_vhost_manager:lookup().

%% all_requests(HostLabel) ->
%%     [ {RN, global:whereis_name(N)} ||
%%      (N = {reflect_vhost_manager, request, H, RN}) <- global:registered_names(),
%%      H =:= HostLabel ].

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

format_vhost(VHost) ->
    Info = reflect_vhost_manager:info(VHost),
    {tr, [],
     [{td, [], [hlink(VHost)]},
      {td, [], [explain_status(reversehttp:lookup(expiry_ms, Info, 0),
                               reversehttp:lookup(poller_count, Info, 0),
                               reversehttp:lookup(request_count, Info, 0))]}]}.
