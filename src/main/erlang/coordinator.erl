-module(coordinator).

%% API
-export([start/1, main/1]).

-record(state, {
  java_coordinator_process,
  java_coordinator_name,
  workers,
  timeout_setup_cluster = infinity,
  timeout_worker = infinity
}).


% Args = [process_name, java_coordinator_process, java_coordinator_name]
start(Args) ->
  State = #state{
    java_coordinator_process = lists:nth(2, Args),
    java_coordinator_name = lists:nth(3, Args)
  },
  register(lists:nth(1, Args), spawn(?MODULE, main, [State])),
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! heartbeat.


main(State) ->
  receive
    {init_phase, stop} ->
      stop(stopped_by_java_coordinator);

    {cluster_setup_phase, stop} ->
      stop_cluster(State#state.workers),
      stop(cluster_setup_timeout);

    {cluster_setup_phase, InitClusterCmd, Workers, TimeoutSetupCluster, TimeoutWorker} ->
      NewState = State#state{
        workers = Workers,
        timeout_setup_cluster = TimeoutSetupCluster,
        timeout_worker = TimeoutWorker
      },
      spawn(fun() -> os:cmd(InitClusterCmd) end),
      wait_for_cluster_setup(length(Workers), NewState),
      main(NewState);

    {cluster_setup_phase, Workloads} ->
      send_workload(State#state.workers, Workloads),

      %TODO: change
      stop_cluster(State#state.workers),
      {State#state.java_coordinator_process, State#state.java_coordinator_name} ! end_computation,
      stop(finished)

  after State#state.timeout_worker ->
    stop_cluster(State#state.workers),
    stop(cluster_setup_timeout)
  end.


wait_for_cluster_setup(0, State) ->
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! cluster_ready;

wait_for_cluster_setup(MissingWorkers, State) ->
  receive
    heartbeat ->
      wait_for_cluster_setup(MissingWorkers - 1, State)
  after State#state.timeout_setup_cluster ->
    {State#state.java_coordinator_process, State#state.java_coordinator_name} ! cluster_timeout
  end.


send_workload([], []) ->
  workloads_sent;

send_workload([Worker|RemainingWorkers], [Workload|RemainingWorkloads]) ->
  Worker ! {cluster_setup_phase, Workload},
  send_workload(RemainingWorkers, RemainingWorkloads).


stop_cluster(Workers) ->
  lists:foreach(fun(Worker) -> Worker ! stop end, Workers).


stop(Reason) ->
  init:stop(),
  exit(Reason).

