-module(metric_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([smoke_test/1]).

all() -> [smoke_test].

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(metric),
    Config.

smoke_test(_Config) ->
    metric_app:report(<<"test">>, 3),
    metric_app:report(<<"test">>, 3),
    metric_app:report(<<"test">>, 3),
    3.0 = metric_app:average(<<"test">>).

concurrent_write_test(_Config) ->
    [metric_app:report(MetricName, MetricValue) || MetricName <- [<<"test1">>, <<"test2">>, <<"test3">>], MetricValue <- lists:seq(1, 1000)].

end_per_testcase(_, _Config) ->
    ok.
