-module(lager_wrapper).
-behaviour(gen_logger).

-export([log/1]).

-define(TRUNCATE_SIZE, 1024).


log(#{message := Message,
      severity := Severity} = Data) ->
    Metadata = maps:without([message, severity, timestamp], Data),
    lager:dispatch_log(Severity, Metadata, iolist_to_binary(Message),
                       [], ?TRUNCATE_SIZE).
