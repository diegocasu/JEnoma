%% @doc
%% Module representing the Erlang coordinator node instantiated on the coordinator machine.
%% It is responsible for: <br/>
%% 1) starting the Erlang worker nodes on the remote machines of the cluster; <br/>
%% 2) sending the workloads; <br/>
%% 3) performing the distributed elitism by collecting the elite and worst individuals
%%    of each worker, finding the global elite and worst ones and sending the updates to
%%    the involved nodes; <br/>
%% 4) collecting the partial termination conditions, sending them to the Java coordinator and
%%    propagating the end/continue decision to the workers; <br/>
%% 5) collecting the final results of each worker, sending them to the Java coordinator; <br/>
%% 6) stopping the cluster, either because a timeout has expired or upon a Java coordinator request.
-module(coordinator).

%% API
-export([start/1, main/1]).

%% State of the coordinator.
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


%% ====================================================================
%% Public API
%% ====================================================================

%% @spec start(Args :: list()) -> atom().
%% @doc
%% Starts the coordinator node by spawning a dedicated process running the main/1 function.
%% The list passed as argument must contain three atoms, representing the name of the process,
%% the name of the Java coordinator process and the name of the Java coordinator node, respectively.
start(Args) ->
  State = #state{
    java_coordinator_process = lists:nth(2, Args),
    java_coordinator_name = lists:nth(3, Args)
  },
  register(lists:nth(1, Args), spawn(?MODULE, main, [State])),
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! heartbeat.


%% @spec start(State :: record()) -> no_return().
%% @doc
%% Waits for messages sent either by the Java coordinator or by the Erlang workers,
%% executing the appropriate management actions. The function stays in an infinite receive
%% loop until the Java coordinator sends a stop signal or a timeout expires. The latter happens
%% when at least one worker does not send a message in time: this occurrence is interpreted
%% as a node failure, triggering the termination of the algorithm and the shutdown of the cluster.
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
      send_workloads(State#state.workers, Workloads),
      main(State);

    {elitism_phase, collection_completed} ->
      %% Sort the best individuals by descending order, the worst individuals by ascending order.
      %% If two individuals have the same fitness, they are sorted by node name, ensuring that
      %% the elitism returns the same results over multiple invocations, even if the sending operations
      %% made by distinct nodes are interleaved differently.
      Elite = lists:sort(
        fun({_, FitnessA, {_, NodeA}}, {_, FitnessB, {_, NodeB}}) -> {FitnessB, NodeB} =< {FitnessA, NodeA} end,
        State#state.elitism_elite_individuals),
      Worst = lists:sort(
        fun({FitnessA, _, {_, NodeA}}, {FitnessB, _, {_, NodeB}}) -> {FitnessA, NodeA} =< {FitnessB, NodeB} end,
        State#state.elitism_worst_individuals),

      send_elite_individuals(Elite, Worst, State#state.elitism_number_individuals),
      lists:foreach(fun(Worker) -> Worker ! {elitism_phase, end_elitism} end, State#state.workers),

      NewState = State#state{
        elitism_elite_individuals = [],
        elitism_worst_individuals = [],
        elitism_workers_ready = 0
      },
      main(NewState);

    {elitism_phase, Elite, Worst} ->
      NewState = State#state{
        elitism_elite_individuals = lists:append(Elite, State#state.elitism_elite_individuals),
        elitism_worst_individuals = lists:append(Worst, State#state.elitism_worst_individuals),
        elitism_workers_ready = State#state.elitism_workers_ready + 1
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
      NewState = State#state{termination_conditions = [TerminationCondition|State#state.termination_conditions]},
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
      NewState = State#state{population_chunks = [PopulationChunk|State#state.population_chunks]},
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


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Waits until all the workers in the cluster send a heartbeat.
%% If a timeout expires, the Java coordinator is notified with a cluster_timeout atom.
%% If the cluster setup is successful, the Java coordinator is notified with a cluster_ready atom.
wait_for_cluster_setup(0, State) ->
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! cluster_ready;

wait_for_cluster_setup(MissingWorkers, State) ->
  receive
    heartbeat ->
      wait_for_cluster_setup(MissingWorkers - 1, State)
  after State#state.timeout_setup_cluster ->
    {State#state.java_coordinator_process, State#state.java_coordinator_name} ! cluster_timeout
  end.


%% @doc Sends the workloads to the workers in the cluster.
send_workloads([], [], _) ->
  workloads_sent;

send_workloads([Worker|RemainingWorkers], [Workload|RemainingWorkloads], WorkersFullList) ->
  Worker ! {cluster_setup_phase, Workload, WorkersFullList},
  send_workloads(RemainingWorkers, RemainingWorkloads, WorkersFullList).

send_workloads(Workers, Workloads) ->
  send_workloads(Workers, Workloads, Workers).


%% @doc
%% Sends the results of the elitism phase to the workers. Each worker receives zero, one or more
%% tuples containing an elite individual and an index, where the latter points to the individual
%% of its population to be replaced.
send_elite_individuals(_, _, 0) ->
  ok;

send_elite_individuals([], [], _) ->
  ok;

send_elite_individuals([{EliteIndividual, _, _}|Elite], [{_, WorstIndex, Worker}|Worst], Remaining) ->
  Worker ! {elitism_phase, {EliteIndividual, WorstIndex}},
  send_elite_individuals(Elite, Worst, Remaining - 1).


%% @doc Alerts the Java coordinator that the computation has failed.
signal_computation_failure(State) ->
  {State#state.java_coordinator_process, State#state.java_coordinator_name} ! computation_failed.


%% @doc Sends a stop command to all the workers in the cluster.
stop_cluster(Workers) ->
  lists:foreach(fun(Worker) -> Worker ! stop end, Workers).


%% @doc Terminates the coordinator process, deallocating its resources.
stop(Reason) ->
  init:stop(),
  exit(Reason).
