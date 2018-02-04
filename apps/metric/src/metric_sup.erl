%%%-------------------------------------------------------------------
%% @doc metric top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(metric_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    metric_storage:init(),
    {ok, Pools} = application:get_env(metric, pools),
    supervisor:start_link({local, ?SERVER}, ?MODULE, Pools).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Pools) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, metric_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    {ok, {SupFlags, PoolSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

