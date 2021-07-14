-module(worker).

%% API
-export([start/1, main/1]).

-record(state, {
  this_process_name,
  erlang_coordinator_process,
  erlang_coordinator_name,
  jar_file,
  java_worker_process,
  java_worker_name,
  workers,
  elitism_individuals = [],
  shuffling_node_id,
  shuffling_individuals = [],
  shuffling_workers_ready = 0
}).


start(Args) ->
  State = #state{
    this_process_name = lists:nth(1, Args),
    erlang_coordinator_process = lists:nth(2, Args),
    erlang_coordinator_name = lists:nth(3, Args),
    jar_file = lists:nth(4, Args),
    java_worker_process = lists:nth(5, Args),
    java_worker_name = lists:nth(6, Args)
  },
  register(State#state.this_process_name, spawn(?MODULE, main, [State])),

  ThisHost = lists:nth(2, string:tokens(atom_to_list(node()), "@")),
  LoggerCoordinatorHost = lists:nth(2, string:tokens(atom_to_list(State#state.erlang_coordinator_name), "@")),
  JavaNodeCmd = io_lib:format("java -cp ~s it.unipi.jenoma.cluster.Worker ~s ~s", [State#state.jar_file, ThisHost, LoggerCoordinatorHost]),
  spawn(fun() -> os:cmd(lists:flatten(JavaNodeCmd)) end).


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
      NewState = State#state{
        workers = Workers,
        shuffling_node_id = index_of({State#state.this_process_name, node()}, Workers)
      },
      main(NewState);

    {elitism_phase, Elite, Worst} ->
      WorstWithSource = [{Fitness, Index, {State#state.this_process_name, node()}} || {Fitness, Index} <- Worst],
      {State#state.erlang_coordinator_process, State#state.erlang_coordinator_name} !
        {elitism_phase, Elite, WorstWithSource},
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
      IndividualsList = [NewIndividual | State#state.elitism_individuals],
      NewState = State#state{elitism_individuals = IndividualsList},
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
          % Sort individuals by NodeId. If the population subjected to the shuffling is the same,
          % this ensures that the shuffling returns the same results over multiple invocations,
          % even if the calls to broadcast() of distinct nodes are interleaved differently.
          IndividualsByNodeId = lists:sort(fun({NodeIdA, _}, {NodeIdB, _}) -> NodeIdA =< NodeIdB end, State#state.shuffling_individuals),
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
      broadcast(Population, State#state.workers, State#state.shuffling_node_id),
      main(State);

    _ ->
      main(State)
  end.


broadcast(Population, Peers, NodeId) ->
  broadcast(Population, Peers, NodeId, 0).

broadcast([], Peers, _, _) ->
  lists:foreach(fun(Peer) -> Peer ! {shuffle_phase, worker_ready} end, Peers);

broadcast([Individual|Population], Peers, NodeId, Index) ->
  lists:nth(Index + 1, Peers) ! {shuffle_phase, {NodeId, Individual}},
  broadcast(Population, Peers, NodeId, (Index + 1) rem length(Peers)).


index_of(Item, List) ->
  index_of(Item, List, 1).

index_of(_, [], _) ->
  not_found;

index_of(Item, [Item|_], Index) ->
  Index;

index_of(Item, [_|T], Index) ->
  index_of(Item, T, Index + 1).


stop_java_node(State) ->
  {State#state.java_worker_process, State#state.java_worker_name} ! stop.


stop(Reason) ->
  init:stop(),
  exit(Reason).

