%%%-------------------------------------------------------------------
%% @doc metric_worker used with poolboy for concurrent calculation
%% @end
%%%-------------------------------------------------------------------

-module(metric_worker).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/1]).

%% Genserver callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, {}}.

handle_call({average, MetricName}, _From, State) ->
    MetricSlice = metric_storage:get(MetricName),
    Average     = metric_average(MetricSlice),
    {reply, Average, State}.

handle_cast({report, MetricName, MetricValue}, State) ->
    metric_storage:set(MetricName, MetricValue),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec metric_average(MetricSlice :: list()) -> float().
metric_average([]) -> 0;

metric_average(MetricSlice) ->
    Length = length(MetricSlice),
    Sum = lists:foldl(
            fun({_Timestamp, _MetricN, Value}, Sum) -> Value + Sum end,
            0,
            MetricSlice
           ),
    Sum / Length.

%%====================================================================
%% Eunit
%%====================================================================

-ifdef(TEST).

metric_average_test() ->
    ?assertEqual(0, metric_average([])),
    ?assertEqual(2.5, metric_average(
                        [
                         {1517692412974439, <<"test1">>, 1.5},
                         {1517692412974439, <<"test1">>, 2.5},
                         {1517692412974439, <<"test1">>, 3.5}
                        ])).

-endif.

