%%%-------------------------------------------------------------------
%% @doc metric public API
%% @end
%%%-------------------------------------------------------------------

-module(metric_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% Application API
-export([report/2, average/1]).

%%====================================================================
%% API
%%====================================================================

-spec report(MetricName :: binary(), MetricValue :: float()) -> ok.
report(MetricName, MetricValue) ->
    poolboy:transaction(metric_workers, fun(Worker) ->
        gen_server:cast(Worker, {report, MetricName, MetricValue})
    end).

-spec average(MetricName :: binary()) -> float().
average(MetricName) ->
    poolboy:transaction(metric_workers, fun(Worker) ->
        gen_server:call(Worker, {average, MetricName})
    end).

start(_StartType, _StartArgs) ->
    metric_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

