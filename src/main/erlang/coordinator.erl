-module(coordinator).

%% API
-export([start/1, main/1]).

-record(state, {
  java_coordinator_process,
  java_coordinator_name,
  workers,
  timeout_setup_cluster = infinity,
  timeout_worker = infinity,
  elitism_number_individuals,
  elitism_elite_individuals = [],
  elitism_worst_individuals = [],
  elitism_workers_ready = 0,
  termination_conditions = [],
  population_chunks = []
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

    {cluster_setup_phase, InitCmd, Workers, TimeoutSetupCluster, TimeoutWorker, ElitismNumber} ->
      NewState = State#state{
        workers = Workers,
        timeout_setup_cluster = TimeoutSetupCluster,
        timeout_worker = TimeoutWorker,
        elitism_number_individuals = ElitismNumber
      },
      spawn(fun() -> os:cmd(InitCmd) end),
      wait_for_cluster_setup(length(Workers), NewState),
      main(NewState);

    {cluster_setup_phase, Workloads} ->
      send_workload(State#state.workers, Workloads),
      main(State);

    {elitism_phase, collection_completed} ->
      % Sort best individuals by descending order, worst individuals by ascending order.
      Elite = lists:sort(fun({_, FitnessA}, {_, FitnessB}) -> FitnessB =< FitnessA end, State#state.elitism_elite_individuals),
      Worst = lists:sort(fun({FitnessA, _, _}, {FitnessB, _, _}) -> FitnessA =< FitnessB end, State#state.elitism_worst_individuals),

      send_elite_individuals(Elite, Worst, State#state.elitism_number_individuals),
      lists:foreach(fun(Worker) -> Worker ! {elitism_phase, end_elitism} end, State#state.workers),

      NewState = State#state{
        elitism_elite_individuals = [],
        elitism_worst_individuals = [],
        elitism_workers_ready = 0
      },
      main(NewState);

    {elitism_phase, Elite, Worst} ->
      EliteList = lists:append(Elite, State#state.elitism_elite_individuals),
      WorstList = lists:append(Worst, State#state.elitism_worst_individuals),
      WorkersReady = State#state.elitism_workers_ready + 1,
      NewState = State#state{
        elitism_elite_individuals = EliteList,
        elitism_worst_individuals = WorstList,
        elitism_workers_ready = WorkersReady
      },
      if
        NewState#state.elitism_workers_ready == length(NewState#state.workers) ->
          self() ! {elitism_phase, collection_completed};
        true ->
          collection_not_completed
      end,
      main(NewState);

    {generation_end_phase, algorithm_continue} ->
      lists:foreach(fun(Worker) -> Worker ! {generation_end_phase, algorithm_continue} end, State#state.workers),
      main(State);

    {generation_end_phase, algorithm_end} ->
      lists:foreach(fun(Worker) -> Worker ! {generation_end_phase, algorithm_end} end, State#state.workers),
      main(State);

    {generation_end_phase, collection_completed} ->
      {State#state.java_coordinator_process, State#state.java_coordinator_name} !
        {termination_conditions, State#state.termination_conditions},
      NewState = State#state{termination_conditions = []},
      main(NewState);

    {generation_end_phase, TerminationCondition} ->
      ConditionsList = [TerminationCondition|State#state.termination_conditions],
      NewState = State#state{termination_conditions = ConditionsList},
      if
        length(NewState#state.termination_conditions) == length(NewState#state.workers) ->
          self() ! {generation_end_phase, collection_completed};
        true ->
          collection_not_completed
      end,
      main(NewState);

    {result_collection_phase, collection_completed} ->
      {State#state.java_coordinator_process, State#state.java_coordinator_name} !
        {final_population, State#state.population_chunks},
      NewState = State#state{population_chunks = []},
      main(NewState);

    {result_collection_phase, PopulationChunk} ->
      ChunksList = [PopulationChunk|State#state.population_chunks],
      NewState = State#state{population_chunks = ChunksList},
      if
        length(NewState#state.population_chunks) == length(NewState#state.workers) ->
          self() ! {result_collection_phase, collection_completed};
        true ->
          collection_not_completed
      end,
      main(NewState);

    {shutdown_phase, stop} ->
      stop_cluster(State#state.workers),
      stop(shutdown)

  after State#state.timeout_worker ->
    signal_computation_failure(State),
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


send_elite_individuals(_, _, 0) ->
  ok;

send_elite_individuals([], [], _) ->
  ok;

send_elite_individuals([{EliteIndividual, _}|Elite], [{_, WorstIndex, Worker}|Worst], Remaining) ->
  Worker ! {elitism_phase, {EliteIndividual, WorstIndex}},
  send_elite_individuals(Elite, Worst, Remaining - 1).


signal_computation_failure(State) ->
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! computation_failed.

stop_cluster(Workers) ->
  lists:foreach(fun(Worker) -> Worker ! stop end, Workers).


stop(Reason) ->
  init:stop(),
  exit(Reason).

