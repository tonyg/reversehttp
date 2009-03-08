-module(reflect_vhost_manager).
-behaviour(gen_server).

-export([setup_tables/0, start_link/2, lookup/0]).
-export([request/2, configure/3, poll/4, respond/2]).
-export([info/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(POLL_TIMEOUT, 30000).
-define(REQUEST_TIMEOUT, 1000).
-define(ANSWER_TIMEOUT, 60000).

-record(reflect_vhost_manager, {name, pid}).
-record(reversehttp_request, {key, pid}).

setup_tables() ->
    ets:new(reflect_vhost_manager, [set, public, named_table,
                                    {keypos, #reflect_vhost_manager.name}]),
    ets:new(reversehttp_request, [set, public, named_table, {keypos, #reversehttp_request.key}]),
    ok.

start_link(VHostName, Token) ->
    gen_server:start_link(?MODULE,
                          [VHostName, Token],
                          []).

lookup() ->
    [K || [K] <- ets:match(reflect_vhost_manager, {reflect_vhost_manager, '$1', '_'})].

request(VHostName, Msg) ->
    case lookup(VHostName) of
        {ok, P} ->
            case with_monitor(P, fun (RequestRef) -> request1(P, RequestRef, Msg) end) of
                retry ->
                    request(VHostName, Msg);
                {error, timeout} ->
                    {error, {timeout, poller}};
                {error, Reason} ->
                    {error, Reason};
                {ok, ReplyKey} ->
                    receive
                        {reply, ReplyKey, Response} ->
                            {ok, Response}
                    after ?ANSWER_TIMEOUT ->
                            {error, {timeout, downstream}}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

request1(P, RequestRef, Msg) ->
    P ! {request, RequestRef, self()},
    case receive_reply(RequestRef, ?REQUEST_TIMEOUT, fun () -> unrequest(P) end) of
        {error, Reason} ->
            {error, Reason};
        {ok, PollerRef, PollerPid} ->
            with_monitor(PollerPid,
                         fun (PollerMonitorRef) ->
                                 PollerPid ! {PollerRef, {ok, Msg, self()}},
                                 receive
                                     {'DOWN', PollerMonitorRef, process, _, _} ->
                                         retry;
                                     {PollerRef, failed} ->
                                         retry;
                                     {PollerRef, {ok, ReplyKey}} ->
                                         {ok, ReplyKey}
                                 end
                         end)
    end.

configure(VHostName, Token, LeaseExtensionSeconds) ->
    configure_attempt(0, VHostName, Token, LeaseExtensionSeconds).

configure_attempt(N, _VHostName, _Token, _LeaseExtensionSeconds) when N >= 3 ->
    {error, too_many_attempts};
configure_attempt(N, VHostName, Token, LeaseExtensionSeconds) ->
    case ets:lookup(reflect_vhost_manager, VHostName) of
        [#reflect_vhost_manager{pid = HostPid}] ->
            configure1(HostPid, Token, LeaseExtensionSeconds, {ok, existing, HostPid});
        [] ->
            case supervisor:start_child(reflect_vhost_manager_sup, [VHostName, Token]) of
                {ok, ChildPid} ->
                    case ets:insert_new(reflect_vhost_manager,
                                        #reflect_vhost_manager{name = VHostName,
                                                               pid = ChildPid}) of
                        true ->
                            configure1(ChildPid, Token, LeaseExtensionSeconds, {ok, new, ChildPid});
                        false ->
                            timer:sleep(10),
                            configure_attempt(N + 1, VHostName, Token, LeaseExtensionSeconds)
                    end;
                {error, {already_started, _ChildPid}} ->
                    timer:sleep(10),
                    configure_attempt(N + 1, VHostName, Token, LeaseExtensionSeconds);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, too_many_vhost_records}
    end.

configure1(P, Token, LeaseExtensionSeconds, OkResult) ->
    case gen_server:call(P, {configure, Token, LeaseExtensionSeconds}) of
        ok ->
            OkResult;
        {error, Reason} ->
            {error, Reason}
    end.

poll(VHostName, Token, ReplyKey, F) ->
    case lookup(VHostName) of
        {ok, P} ->
            poll1(P, Token, ReplyKey, F);
        {error, not_found} ->
            case configure(VHostName, Token, 0) of
                {ok, _ExistingOrNew, P} ->
                    poll1(P, Token, ReplyKey, F);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

poll1(P, Token, ReplyKey, F) ->
    with_monitor(P,
                 fun (PollerRef) ->
                         P ! {poll, Token, PollerRef, self()},
                         case receive_reply(PollerRef, ?POLL_TIMEOUT, fun () -> unpoll(P) end) of
                             {ok, Msg, RequestPid} ->
                                 ok = register_request(ReplyKey, RequestPid),
                                 RequestPid ! {PollerRef, case catch F(Msg) of
                                                              ok ->
                                                                  {ok, ReplyKey};
                                                              _ ->
                                                                  ok = unregister_request(ReplyKey),
                                                                  failed
                                                          end},
                                 ok;
                             {error, Reason} ->
                                 {error, Reason}
                         end
                 end).

respond(ReplyKey, F) ->
    case lookup_request(ReplyKey) of
        not_found ->
            {error, not_found};
        {ok, #reversehttp_request{pid = RequestPid}} ->
            case F() of
                {ok, Response} ->
                    ok = unregister_request(ReplyKey),
                    RequestPid ! {reply, ReplyKey, Response},
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

info(VHostName) ->
    case lookup(VHostName) of
        {ok, Pid} ->
            gen_server:call(Pid, info);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------

lookup(VHostName) ->
    case ets:lookup(reflect_vhost_manager, VHostName) of
        [#reflect_vhost_manager{pid = P}] ->
            {ok, P};
        [] ->
            {error, not_found};
        _ ->
            {error, too_many_vhost_records}
    end.

register_request(ReplyKey, RequestPid) ->
    ets:insert(reversehttp_request, #reversehttp_request{key = ReplyKey, pid = RequestPid}),
    ok.

unregister_request(ReplyKey) ->
    ets:delete(reversehttp_request, ReplyKey),
    ok.

lookup_request(ReplyKey) ->
    case ets:lookup(reversehttp_request, ReplyKey) of
        [X] -> {ok, X};
        _ -> not_found
    end.

unrequest(P) -> ok = gen_server:call(P, {unrequest, self()}).
unpoll(P) -> ok = gen_server:call(P, {unpoll, self()}).

max(A, B) when A > B -> A;
max(_A, B) -> B.

sane_lease(Seconds) ->
    %% Don't let the idle-timeout lease drop below 30 seconds; no
    %% particular reason other than to avoid flapping. Must also be
    %% long enough to give a reasonable window for a subsequent
    %% initial configure to happen.
    max(30, Seconds).

with_monitor(Pid, F) ->
    Ref = erlang:monitor(process, Pid),
    Result = F(Ref),
    erlang:demonitor(Ref),
    Result.

receive_reply(Ref, TimeoutMs, CancelFun) ->
    receive
        {'DOWN', Ref, process, _Pid, _Reason} ->
            {error, noproc};
        {Ref, Msg} ->
            Msg
    after TimeoutMs ->
            ok = CancelFun(),
            receive
                {Ref, Msg} ->
                    Msg
            after 0 ->
                    {error, timeout}
            end
    end.

%%--------------------------------------------------------------------

-record(vhost_state, {name, token, lease_extension_seconds, lease_expiry_time, waiting}).

update_lease(State = #vhost_state{waiting = {pollers, QueueOfPollers}}) ->
    case queue:is_empty(QueueOfPollers) of
        true ->
            update_lease1(State);
        false ->
            State#vhost_state{lease_expiry_time = infinity}
    end;
update_lease(State) ->
    update_lease1(State).

update_lease1(State = #vhost_state{lease_expiry_time = infinity,
                                   lease_extension_seconds = LeaseExtensionSeconds}) ->
    State#vhost_state{lease_expiry_time = delay_time(erlang:now(), LeaseExtensionSeconds)};
update_lease1(State) ->
    State.

delay_time(T, DelaySeconds) ->
    ThenMicrosecs = timer:now_diff(T, {0, -DelaySeconds, 0}),
    A3 = ThenMicrosecs rem 1000000,
    A12 = ThenMicrosecs div 1000000,
    A2 = A12 rem 1000000,
    A1 = A12 div 1000000,
    {A1, A2, A3}.

timer_val(#vhost_state{lease_expiry_time = infinity}) ->
    infinity;
timer_val(#vhost_state{lease_expiry_time = CurrentExpiry}) ->
    timer:now_diff(CurrentExpiry, erlang:now()) div 1000.

noreply(S0 = #vhost_state{}) ->
    S = update_lease(S0),
    {noreply, S, timer_val(S)}.

reply(Reply, S0 = #vhost_state{}) ->
    S = update_lease(S0),
    {reply, Reply, S, timer_val(S)}.

enqueue_one(QueueOfWhat, Ref, Pid, Q, State = #vhost_state{})
  when is_reference(Ref), is_pid(Pid) ->
    noreply(State#vhost_state{waiting = {QueueOfWhat, queue:in({Ref, Pid}, Q)}}).

erase_one(QueueOfWhat, Pid, State = #vhost_state{waiting = {QueueOfWhat, Q}}) ->
    Q1 = queue:from_list(lists:keydelete(Pid, 2, queue:to_list(Q))),
    S = State#vhost_state{waiting = {QueueOfWhat, Q1}},
    reply(ok, S);
erase_one(_QueueOfWhat, _Pid, State) ->
    reply(ok, State).

%%--------------------------------------------------------------------

init([VHostName, Token]) ->
    process_flag(trap_exit, true),
    S = update_lease(#vhost_state{name = VHostName,
                                  token = Token,
                                  lease_extension_seconds = sane_lease(0),
                                  lease_expiry_time = infinity,
                                  waiting = {pollers, queue:new()}}),
    {ok, S, timer_val(S)}.

handle_call({configure, Token, LeaseExtensionSeconds}, _From, State) ->
    case State#vhost_state.token of
        Token ->
            reply(ok, State#vhost_state{lease_extension_seconds = sane_lease(LeaseExtensionSeconds),
                                        lease_expiry_time = infinity});
        _ ->
            reply({error, bad_token}, State)
    end;

handle_call({unrequest, RequestPid}, _From, State) ->
    erase_one(requests, RequestPid, State);

handle_call({unpoll, PollerPid}, _From, State) ->
    erase_one(pollers, PollerPid, State);

handle_call(info, _From, State = #vhost_state{name = Name, waiting = Waiting}) ->
    ExpiryMs = timer_val(State),
    reply([{name, Name}, {expiry_ms, ExpiryMs}] ++
          case Waiting of
              {pollers, Q} -> [{poller_count, queue:len(Q)}, {request_count, 0}];
              {requests, Q} -> [{poller_count, 0}, {request_count, queue:len(Q)}]
          end,
          State).

handle_cast(Msg, State) ->
    {stop, {unhandled_cast, ?MODULE, Msg, State}, State}.

handle_info({request, RequestRef, RequestPid}, State = #vhost_state{waiting = {pollers, Q}}) ->
    case queue:out(Q) of
        {{value, {PollerRef, PollerPid}}, Tail} ->
            RequestPid ! {RequestRef, {ok, PollerRef, PollerPid}},
            noreply(State#vhost_state{waiting = {pollers, Tail}});
        {empty, _} ->
            enqueue_one(requests, RequestRef, RequestPid, queue:new(), State)
    end;
handle_info({request, RequestRef, RequestPid}, State = #vhost_state{waiting = {requests, Q}}) ->
    enqueue_one(requests, RequestRef, RequestPid, Q, State);

handle_info({poll, Token, PollerRef, PollerPid}, State = #vhost_state{token = HostToken})
  when Token =/= requeue, Token =/= HostToken ->
    PollerPid ! {PollerRef, {error, bad_token}},
    noreply(State);
handle_info({poll, _Token, PollerRef, PollerPid}, State = #vhost_state{waiting = {pollers, Q}}) ->
    enqueue_one(pollers, PollerRef, PollerPid, Q, State);
handle_info({poll, _Token, PollerRef, PollerPid}, State = #vhost_state{waiting = {requests, Q}}) ->
    case queue:out(Q) of
        {{value, {RequestRef, RequestPid}}, Tail} ->
            RequestPid ! {RequestRef, {ok, PollerRef, PollerPid}},
            noreply(State#vhost_state{lease_expiry_time = infinity,
                                      waiting = {requests, Tail}});
        {empty, _} ->
            enqueue_one(pollers, PollerRef, PollerPid, queue:new(), State)
    end;

handle_info(timeout, State = #vhost_state{name = Name}) ->
    error_logger:info_report({?MODULE, Name, expired}),
    {stop, normal, State}.

terminate(_Reason, _State = #vhost_state{name = Name}) ->
    ets:delete_object(reflect_vhost_manager, #reflect_vhost_manager{name = Name, pid = self()}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
