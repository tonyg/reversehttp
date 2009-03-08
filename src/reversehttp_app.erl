%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the reversehttp application.

-module(reversehttp_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for reversehttp.
start(_Type, _StartArgs) ->
    reversehttp_deps:ensure(),
    reversehttp_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for reversehttp.
stop(_State) ->
    ok.
