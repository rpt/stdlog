-module(stdlog_pt).

-export([parse_transform/2]).


parse_transform(Forms, Options) ->
    _ = put(hardcode_logger, proplists:get_value(stdlog_hardcode_logger, Options, log)),
    case proplists:get_value(stdlog_replace_lager, Options, false) of
        true ->
            ok = path:add_callback(call, fun replace_lager/1);
        false ->
            ok
    end,
    ok = path:add_callback(attribute, fun set_file/1),
    ok = path:add_callback(attribute, fun set_module/1),
    ok = path:add_callback(function, fun set_function/1),
    ok = path:add_callback(call, fun add_metadata/1),
    path_utils:spy_walk(Forms).

replace_lager({call, L, {remote, _, {atom, _, lager}, {atom, _, Severity}}, Args}) ->
    {continue, {call, L, {remote, L, {atom, L, log},
                          {atom, L, Severity}}, lager_args(L, Args)}}.

lager_args(_, [Message]) ->
    [Message];
lager_args(L, [Format, Args]) ->
    [{call, L, {remote, L, {atom, L, io_lib}, {atom, L, format}}, [Format, Args]}].

set_file({attribute, _, file, {File, _}}) ->
    _ = put(file, File),
    continue.

set_module({attribute, _, module, Module}) ->
    _ = put(module, Module),
    continue.

set_function({function, _, Function, Arity, _}) ->
    _ = put(function, Function),
    _ = put(arity, Arity),
    continue.

add_metadata({call, L, {remote, _, {atom, _, log}, {atom, _, Severity}}, Args}) ->
    {stop, replace(L, Severity, Args)}.

replace(L, Severity, [Message]) ->
    log(L, data(L, Severity, Message));
replace(L, Severity, [Message, Metadata]) ->
    log(L, merge(L, data(L, Severity, Message), Metadata)).

log(L, Data) ->
    {call, L, {remote, L, {atom, L, get(hardcode_logger)}, {atom, L, log}}, [Data]}.

merge(L, M1, M2) ->
    {call, L, {remote, L, {atom, L, maps}, {atom, L, merge}}, [M1, M2]}.

data(L, Severity, Message) ->
    {map, L, [{map_field_assoc, L, {atom, L, severity}, {atom, L, Severity}},
              {map_field_assoc, L, {atom, L, message}, Message},
              {map_field_assoc, L, {atom, L, module}, {atom, L, get(module)}},
              {map_field_assoc, L, {atom, L, function}, {atom, L, get(function)}},
              {map_field_assoc, L, {atom, L, arity}, {atom, L, get(arity)}},
              {map_field_assoc, L, {atom, L, file}, {atom, L, get(file)}},
              {map_field_assoc, L, {atom, L, line}, {integer, L, L}},
              {map_field_assoc, L, {atom, L, pid}, self_pid(L)}]}.

self_pid(L) ->
    {call, L, {atom, L, pid_to_list}, [{call, L, {atom, L, self}, []}]}.
