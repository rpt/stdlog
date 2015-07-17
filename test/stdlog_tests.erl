-module(stdlog_tests).

-include_lib("eunit/include/eunit.hrl").


throw_test() ->
    ok = error_logger:tty(false),
    ok = application:set_env(stdlog, logger, fun erlang:throw/1),
    Message = "test",
    try
        log:info(Message)
    catch
        throw:#{message := Thrown,
                severity := Severity} ->
            ?assertEqual(Message, Thrown),
            ?assertEqual(info, Severity)
    end.

throw_with_metadata_test() ->
    ok = error_logger:tty(false),
    ok = application:set_env(stdlog, logger, fun erlang:throw/1),
    Message = "test",
    try
        log:error(Message, #{field => value})
    catch
        throw:#{message := Thrown,
                severity := Severity,
                field := Field} ->
            ?assertEqual(Message, Thrown),
            ?assertEqual(error, Severity),
            ?assertEqual(value, Field)
    end.
