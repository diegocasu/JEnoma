-module(coordinator).

%% API
-export([start/1, init/1]).

-record(state, {
  java_coordinator_process,
  java_coordinator_name,
  workers,
  timeout
}).


% Args = [process_name, java_coordinator_process, java_coordinator_name]
start(Args) ->
  State = #state{java_coordinator_process = lists:nth(2, Args), java_coordinator_name = lists:nth(3, Args)},
  register(lists:nth(1, Args), spawn(?MODULE, init, [State])).

init(State) ->
  % Send a heartbeat to the Java coordinator.
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! heartbeat,

  receive
    {InitClusterCmd, Workers, Timeout} ->
      NewState = State#state{workers = Workers, timeout = Timeout},
      spawn(fun() -> os:cmd(InitClusterCmd) end),
      wait_for_cluster_setup(length(Workers), NewState),
      main_loop(NewState);
    stop ->
      stop(stopped_by_java_coordinator)
  end.

wait_for_cluster_setup(0, _) ->
  cluster_ready;

wait_for_cluster_setup(MissingWorkers, State) ->
  receive
    heartbeat ->
      wait_for_cluster_setup(MissingWorkers - 1, State)
  after State#state.timeout ->
    stop_cluster(State#state.workers),
    stop(cluster_setup_timeout)
  end.

main_loop(State) ->
  stop_cluster(State#state.workers),
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! end_computation,
  stop(finished).

stop_cluster(Workers) ->
  lists:foreach(fun(Worker) -> Worker ! stop end, Workers).

stop(Reason) ->
  init:stop(),
  exit(Reason).

