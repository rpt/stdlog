-module(log).

-export([emergency/1]).
-export([emergency/2]).
-export([alert/1]).
-export([alert/2]).
-export([critical/1]).
-export([critical/2]).
-export([error/1]).
-export([error/2]).
-export([warning/1]).
-export([warning/2]).
-export([notice/1]).
-export([notice/2]).
-export([info/1]).
-export([info/2]).
-export([debug/1]).
-export([debug/2]).

-export_type([severity/0]).
-export_type([message/0]).
-export_type([metadata/0]).

-include("stdlog.hrl").

-define(DEFAULT_METADATA, #{}).


emergency(Message) -> emergency(Message, ?DEFAULT_METADATA).
emergency(Message, Metadata) -> log(emergency, Message, Metadata).
alert(Message) -> alert(Message, ?DEFAULT_METADATA).
alert(Message, Metadata) -> log(alert, Message, Metadata).
critical(Message) -> critical(Message, ?DEFAULT_METADATA).
critical(Message, Metadata) -> log(critical, Message, Metadata).
error(Message) -> ?MODULE:error(Message, ?DEFAULT_METADATA).
error(Message, Metadata) -> log(error, Message, Metadata).
warning(Message) -> warning(Message, ?DEFAULT_METADATA).
warning(Message, Metadata) -> log(warning, Message, Metadata).
notice(Message) -> notice(Message, ?DEFAULT_METADATA).
notice(Message, Metadata) -> log(notice, Message, Metadata).
info(Message) -> info(Message, ?DEFAULT_METADATA).
info(Message, Metadata) -> log(info, Message, Metadata).
debug(Message) -> debug(Message, ?DEFAULT_METADATA).
debug(Message, Metadata) -> log(debug, Message, Metadata).

-spec log(severity(), message(), metadata()) -> ok | no_return().
log(Severity, Message, Metadata) ->
    _ = application:start(?APP), %% Hmmm...
    case application:get_env(?APP, logger) of
        {ok, Logger} ->
            Data = Metadata#{message => Message,
                             severity => Severity,
                             timestamp => erlang:now()}, %% Hmmm...
            case Logger of
                Fun when is_function(Fun) ->
                    Fun(Data);
                Module when is_atom(Module) ->
                    Module:log(Data)
            end;
        undefined ->
            erlang:error(no_logger)
    end.
