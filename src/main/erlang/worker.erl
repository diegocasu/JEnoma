%% @doc
%% Module representing the Erlang worker node instantiated on a worker machine.
%% It is responsible for: <br/>
%% 1) starting the Java worker on its same machine; <br/>
%% 2) receiving the workloads; <br/>
%% 3) sending the elite and worst individuals, the partial termination condition and the final results
%%    computed by the Java worker to the coordinator; <br/>
%% 4) shuffling the population used by the Java worker by exchanging individuals
%%    with the other workers of the cluster; <br/>
%% 5) stopping itself and the Java worker when the coordinator sends a stop signal.
-module(worker).

%% API
-export([start/1, main/1]).

%% State of the worker.
-record(state, {
  this_process_name,
  erlang_coordinator_process,
  erlang_coordinator_name,
  jar_file,
  java_worker_process,
  java_worker_name,
  workers,
  elitism_individuals = [],
  shuffling_individuals = [],
  shuffling_workers_ready = 0
}).


%% ====================================================================
%% Public API
%% ====================================================================

%% @spec start(Args :: list()) -> atom().
%% @doc
%% Starts the Erlang worker node by spawning a dedicated process running the main/1 function and
%% executes the shell command to start the Java worker.
%% The list passed as argument must contain six atoms, representing the name of the process,
%% the name of the Erlang coordinator process, the name of the Erlang coordinator node,
%% the name of the jar containing the files of the project, the name of the Java worker process
%% and the name of the Java worker node, respectively.
start(Args) ->
  State = #state{
    this_process_name = lists:nth(1, Args),
    erlang_coordinator_process = lists:nth(2, Args),
    erlang_coordinator_name = lists:nth(3, Args),
    jar_file = lists:nth(4, Args),
    java_worker_process = lists:nth(5, Args),
    java_worker_name = lists:nth(6, Args)
  },

  ThisHost = lists:nth(2, string:tokens(atom_to_list(node()), "@")),
  LoggerCoordinatorHost = lists:nth(2, string:tokens(atom_to_list(State#state.erlang_coordinator_name), "@")),
  JavaNodeCmd = io_lib:format("java -cp ~s it.unipi.jenoma.cluster.Worker ~s ~s", [State#state.jar_file, ThisHost, LoggerCoordinatorHost]),
  spawn(fun() -> os:cmd(lists:flatten(JavaNodeCmd)) end),
  register(State#state.this_process_name, spawn(?MODULE, main, [State])).


%% @spec main(State :: record()) -> no_return().
%% @doc
%% Waits for messages sent by the Erlang coordinator or by the Java worker,
%% executing the appropriate management actions. The function stays in an infinite receive
%% loop until the coordinator sends a stop signal, triggering the shutdown of this node and
%% of the Java worker.
main(State) ->
  receive
    stop ->
      stop_java_node(State),
      stop(stopped_by_coordinator);

    heartbeat ->
      {State#state.erlang_coordinator_process, State#state.erlang_coordinator_name} ! heartbeat,
      main(State);

    {cluster_setup_phase, Workload, Workers} ->
      {State#state.java_worker_process, State#state.java_worker_name} ! Workload,
      NewState = State#state{workers = Workers},
      main(NewState);

    {elitism_phase, Elite, Worst} ->
      EliteWithSource = [{Individual, Fitness, {State#state.this_process_name, node()}} || {Individual, Fitness} <- Elite],
      WorstWithSource = [{Fitness, Index, {State#state.this_process_name, node()}} || {Fitness, Index} <- Worst],
      {State#state.erlang_coordinator_process, State#state.erlang_coordinator_name} !
        {elitism_phase, EliteWithSource, WorstWithSource},
      main(State);

    {elitism_phase, end_elitism} ->
      case State#state.elitism_individuals of
        [] ->
          {State#state.java_worker_process, State#state.java_worker_name} ! not_involved_elitism;
        _ ->
          {State#state.java_worker_process, State#state.java_worker_name} ! State#state.elitism_individuals
      end,
      NewState = State#state{elitism_individuals = []},
      main(NewState);

    {elitism_phase, NewIndividual} ->
      NewState = State#state{elitism_individuals = [NewIndividual | State#state.elitism_individuals]},
      main(NewState);

    {generation_end_phase, algorithm_continue} ->
      {State#state.java_worker_process, State#state.java_worker_name} ! algorithm_continue,
      main(State);

    {generation_end_phase, algorithm_end} ->
      {State#state.java_worker_process, State#state.java_worker_name} ! algorithm_end,
      main(State);

    {generation_end_phase, TerminationCondition} ->
      {State#state.erlang_coordinator_process, State#state.erlang_coordinator_name} !
        {generation_end_phase, TerminationCondition},
      main(State);

    {result_collection_phase, PopulationChunk} ->
      {State#state.erlang_coordinator_process, State#state.erlang_coordinator_name} !
        {result_collection_phase, PopulationChunk},
      main(State);

    {shuffle_phase, worker_ready} ->
      if
        State#state.shuffling_workers_ready == length(State#state.workers) - 1 ->
          %% Sort individuals by node name. If the population to be shuffled is the same,
          %% this ensures that the shuffle returns the same results over multiple invocations,
          %% even if the calls to broadcast() of distinct nodes are interleaved differently.
          IndividualsByNodeId = lists:sort(fun({NodeA, _}, {NodeB, _}) -> NodeA =< NodeB end, State#state.shuffling_individuals),
          {State#state.java_worker_process, State#state.java_worker_name} ! [Individual || {_, Individual} <- IndividualsByNodeId],
          NewState = State#state{
            shuffling_individuals = [],
            shuffling_workers_ready = 0
          },
          main(NewState);
        true ->
          NewState = State#state{shuffling_workers_ready = State#state.shuffling_workers_ready + 1},
          main(NewState)
      end;

    {shuffle_phase, {NodeId, Individual}} ->
      NewState = State#state{shuffling_individuals = [{NodeId, Individual} | State#state.shuffling_individuals]},
      main(NewState);

    {shuffle_phase, Population} ->
      broadcast(Population, State#state.workers, node()),
      main(State);

    _ ->
      main(State)
  end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Scatters the individuals of the given population to all
%% the workers in the cluster. The distribution is done sending one individual
%% to each worker (this node included) following a round robin strategy.
broadcast(Population, Peers, NodeId) ->
  broadcast(Population, Peers, NodeId, 0).

broadcast([], Peers, _, _) ->
  lists:foreach(fun(Peer) -> Peer ! {shuffle_phase, worker_ready} end, Peers);

broadcast([Individual|Population], Peers, NodeId, Index) ->
  lists:nth(Index + 1, Peers) ! {shuffle_phase, {NodeId, Individual}},
  broadcast(Population, Peers, NodeId, (Index + 1) rem length(Peers)).


%% @doc Sends a stop command to the Java worker.
stop_java_node(State) ->
  {State#state.java_worker_process, State#state.java_worker_name} ! stop.


%% @doc Terminates the worker process, deallocating its resources.
stop(Reason) ->
  init:stop(),
  exit(Reason).
