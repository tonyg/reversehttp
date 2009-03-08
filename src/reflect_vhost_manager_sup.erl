-module(reflect_vhost_manager_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ok = reflect_vhost_manager:setup_tables(),

    {ok, {{simple_one_for_one, 10, 10},
          [{reflect_vhost_manager, {reflect_vhost_manager, start_link, []},
            temporary, brutal_kill, worker, [reflect_vhost_manager]}]}}.
