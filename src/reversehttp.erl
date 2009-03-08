%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(reversehttp).
-author('author <author@example.com>').
-export([start/0, stop/0]).

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
