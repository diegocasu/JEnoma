-module(worker).

%% API
-export([start/1, init/1]).

-record(state, {
  erlang_coordinator_process,
  erlang_coordinator_name,
  jar_file,
  java_worker_process,
  java_worker_name
}).


start(Args) ->
  State = #state{
    erlang_coordinator_process = lists:nth(2, Args),
    erlang_coordinator_name = lists:nth(3, Args),
    jar_file = lists:nth(4, Args),
    java_worker_process = lists:nth(5, Args),
    java_worker_name = lists:nth(6, Args)
  },
  register(lists:nth(1, Args), spawn(?MODULE, init, [State])).

init(State) ->
  JavaNodeCmd = io_lib:format("java -cp ~s it.unipi.jenoma.cluster.Worker ~s", [State#state.jar_file, node()]),
  spawn(fun() -> os:cmd(lists:flatten(JavaNodeCmd)) end),

  file:write_file("/home/osboxes/.jenoma/output", "ERLANG SPAWN"),

  receive
    heartbeat ->
      {State#state.erlang_coordinator_process, State#state.erlang_coordinator_name} ! heartbeat,
      file:write_file("/home/osboxes/.jenoma/output1", "JAVA SPAWN"),
      main_loop(State);
    stop ->
      stop_java_node(State),
      stop(stopped_by_coordinator)
  end.

main_loop(State) ->
  receive
    stop ->
      stop_java_node(State),
      stop(stopped_by_coordinator)
  end.

stop_java_node(State) ->
  {State#state.java_worker_process, State#state.java_worker_name} ! stop.

stop(Reason) ->
  init:stop(),
  exit(Reason).

