%%%-------------------------------------------------------------------
%% @doc metric_storage backend, simple example with ets.
%% We can also implement it with different storages -
%% dets, redis, memcached..
%% @end
%%%-------------------------------------------------------------------

-module(metric_storage).

-export([init/0, set/2, get/1, get/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(STORAGE_NAME, metrics).
-define(METRIC_PERIOD, 60 * 1000). % one minute in ms

%%====================================================================
%% API
%%====================================================================

%% @doc Init strorage, create ets
-spec init() -> ok.
init() ->
    ets:new(
      ?STORAGE_NAME,
      [duplicate_bag,
       public,
       named_table,
       {write_concurrency, true},
       {read_concurrency, true}]),
    ok.

%% @doc Store metric in the storage with timestamp
-spec set(MetricName :: binary(), MetricValue :: float()) -> true.
set(MetricName, MetricValue) ->
    ets:insert(?STORAGE_NAME, {get_timestamp(), MetricName, MetricValue}).

%% @doc Get metric list of values from timestamp. Default is one minute
-spec get(MetricNameSearch :: binary(), StartTimestamp :: integer()) -> list().
get(MetricNameSearch, StartTimestamp) ->
    MS = ets:fun2ms(
           fun({Timestamp, MetricName, MetricValue})
                 when Timestamp >= StartTimestamp andalso
                      MetricName =:= MetricNameSearch
                      -> {Timestamp, MetricName, MetricValue}
           end
          ),
    ets:select(?STORAGE_NAME, MS).

-spec get(MetricName :: binary()) -> list().
get(MetricNameSearch) ->
    StartTimestamp = get_timestamp(),
    get(MetricNameSearch, StartTimestamp - ?METRIC_PERIOD).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_timestamp() -> integer().
get_timestamp() ->
    erlang:system_time(millisecond).

