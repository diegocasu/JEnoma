-module(coordinator).

%% API
-export([start/1, init/2]).


% Args = [process_name, java_coordinator_process, java_coordinator_name]
start(Args) ->
  register(lists:nth(1, Args), spawn(?MODULE, init, [lists:nth(2, Args), lists:nth(3, Args)])).


init(JavaCoordinatorProcess, JavaCoordinatorName) ->
  % Send a heartbeat to the Java coordinator.
  {JavaCoordinatorProcess, JavaCoordinatorName} ! heartbeat,

  receive
    {InitClusterCmd, Workers} ->
      spawn(fun() -> os:cmd(InitClusterCmd) end)
  end.

