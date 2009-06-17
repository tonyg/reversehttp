%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for reversehttp.

-module(reversehttp_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/3]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    ReflectorConfig = case application:get_env(reflector_config) of
                         undefined -> [{exception_hosts, [{"localhost", ["/reversehttp"]}]}];
                         {ok, V} -> V
                     end,
    error_logger:info_report({reversehttp_web, config, ReflectorConfig}),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot, ReflectorConfig)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot, ReflectorConfig) ->
    case catch reflect_request_queue:handle(Req, ReflectorConfig) of
        {'EXIT', normal} ->
            ok;
        {'EXIT', Reason} ->
            error_logger:error_report({?MODULE, ?LINE,
                                       {reflector_error, Reason},
                                       {request, Req:dump()}}),
            Req:respond({500, [], "Reflector error"}),
            ok;
        {normal, Path} ->
            static_content(Req, Path, DocRoot);
        ok ->
            ok
    end.

static_content(Req, Path, DocRoot) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
