-module(bad_performance).

-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3, start_link/0, write/1, run/0, stop/1, bench/1, pmap/2]).

-record(state, {fd}).

stop(Pid) ->
  gen_server:cast(Pid, stop).

write(Pid) ->
  gen_server:call(Pid, write, 60 * 1000 * 20).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_) ->
  {ok, Fd} = file:open("/tmp/output", [write, raw]),
  {ok, #state{fd = Fd}}.

terminate(_Reason, #state{fd = Fd}) ->
  io:format("terminate!~n"),
  file:close(Fd),
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

handle_call(write, _From, #state{fd = Fd} = State) ->
  file:write(Fd, <<"helloworld">>),
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_, State) ->
  {noreply, State}.

writeN(0, Pid) ->
  ok;
writeN(N, Pid) ->
  write(Pid),
  writeN(N - 1, Pid).

create_proc(F, X, Parent) ->
  Ref = make_ref(), spawn_link(fun() -> Parent ! {Ref, F(X)} end), Ref.

pmap(F, L) ->
      Parent = self(),
      [receive {Ref, Result} ->  Result end
        || Ref <- [ create_proc(F, X, Parent)|| X <- L]].

bench(Pid) ->
  pmap(fun(_) -> writeN(2, Pid) end, lists:seq(1, 50000)),
  ok.

run() ->
  {ok, Pid} = start_link(),
  Elapsed = timer:tc(?MODULE, bench, [Pid]),
  io:format("Elapsed ~p~n", [Elapsed]),

  stop(Pid).


