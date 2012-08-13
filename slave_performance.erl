-module(slave_performance).

-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3, start_link/0, write/1, run/0, stop/1, bench/1, pmap/2, slave_loop/1]).

-record(state, {fd, slave}).


stop(Pid) ->
  gen_server:cast(Pid, stop).

write(Pid) ->
  gen_server:call(Pid, write, 60 * 1000 * 10).

start_link() ->
  gen_server:start_link(?MODULE, [], []).


slave_loop(undefined) ->
  {ok, Fd} = file:open("/tmp/output", [write, raw]),
  slave_loop(Fd);
slave_loop(Fd) ->
     
     receive 
         {write, Ref, From} ->
           file:write(Fd, <<"helloworld">>),
           From ! {Ref, ok}
     end,
     slave_loop(Fd).

init(_) ->

  Slave = spawn_link(?MODULE, slave_loop, [undefined]),

  {ok, #state{slave = Slave}}.

terminate(_Reason, #state{fd = Fd}) ->
  io:format("terminate!~n"),
  file:close(Fd),
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

handle_call(write, _From, #state{slave = Slave} = State) ->
  Ref = make_ref(),
  Slave ! {write, Ref, self()},
  receive 
    {Ref, ok} -> ok
  end,
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_, State) ->
  {noreply, State}.

writeN(0, _Pid) ->
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


