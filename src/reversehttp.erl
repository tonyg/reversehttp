%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(reversehttp).
-author('author <author@example.com>').
-export([start/0, stop/0]).
-export([lookup/2, lookup/3, host/2, match_access_point/2]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the reversehttp server.
start() ->
    reversehttp_deps:ensure(),
    ensure_started(crypto),
    application:start(reversehttp).

%% @spec stop() -> ok
%% @doc Stop the reversehttp server.
stop() ->
    Res = application:stop(reversehttp),
    application:stop(crypto),
    Res.

lookup(Key, AssocList) ->
    case lists:keysearch(Key, 1, AssocList) of
        {value, {_, Value}} ->
            {ok, Value};
        false ->
            {error, not_found}
    end.

lookup(Key, AssocList, DefaultValue) ->
    case lists:keysearch(Key, 1, AssocList) of
        {value, {_, Value}} ->
            Value;
        false ->
            DefaultValue
    end.

host(Req, Config) ->
    case lookup(override_host, Config, undefined) of
        undefined ->
            case Req:get_header_value(host) of
                undefined -> lookup(default_host, Config, "localhost");
                V -> V
            end;
        V -> V
    end.

match_access_point(Req, Config) ->
    match_access_point1(mochiweb_util:urlsplit_path(Req:get(raw_path)),
                        host(Req, Config),
                        lookup(access_point_paths, Config, [])).

match_access_point1({Path, _QueryPart, _Fragment}, _Host, []) ->
    %% It's a request for content from the main vhost(s). We indicate
    %% to our caller that they should serve content as usual.
    "/" ++ StaticPath = Path,
    {normal, StaticPath};
match_access_point1(Pieces = {Path, QueryPart, _Fragment}, Host, [AccessPoint | AccessPoints]) ->
    case lists:prefix(AccessPoint, Path) of
        true ->
            %% The request was for one of our access points.
            StrippedPath = string:substr(Path, length(AccessPoint) + 1),
            PathComponents = string:tokens(StrippedPath, "/"),
            QueryFields = mochiweb_util:parse_qs(QueryPart),
            {access_point,
             "http://" ++ Host ++ AccessPoint,
             PathComponents,
             QueryFields};
        false ->
            match_access_point1(Pieces, Host, AccessPoints)
    end.
