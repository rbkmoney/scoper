-module(scoper_woody_event_handler).

%% Commented out to compile the module without woody
%% -behaviour(woody_event_handler).

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).


%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

%% client scoping
handle_event(Event = 'call service', RpcID, RawMeta, Opts) ->
    ok = scoper:add_scope(get_scope_name(client, Opts)),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'service result', RpcID, RawMeta, _Opts) ->
    _ = handle_event(Event, RpcID, RawMeta),
    scoper:remove_scope();

%% server scoping
handle_event(Event = 'server receive', RpcID, RawMeta, Opts) ->
    ok = scoper:add_scope(get_scope_name(server, Opts)),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'server send', RpcID, RawMeta, _Opts) ->
    _ = handle_event(Event, RpcID, RawMeta),
    scoper:remove_scope();

%% special cases
handle_event(Event = 'internal error', RpcID, RawMeta, _Opts) ->
    _ = handle_event(Event, RpcID, RawMeta),
    final_error_cleanup(RawMeta);
handle_event(Event = 'trace event', RpcID, RawMeta, Opts) ->
    scoper:scope(
        get_scope_name(server, Opts),
        fun() -> handle_event(Event, RpcID, RawMeta) end
    );

%% the rest
handle_event(Event, RpcID, RawMeta, _Opts) ->
    handle_event(Event, RpcID, RawMeta).


%%
%% Internal functions
%%
handle_event(Event, RpcID, RawMeta) ->
    {Level, {Format, Args}, Meta} = woody_event_handler:format_event_and_meta(
        Event,
        RawMeta,
        RpcID,
        [role, event, service, function, type, metadata, url]
    ),
    ok = scoper:add_meta(maps:merge(Meta, rpc_id_to_map(RpcID))),
    lager:log(Level, [{pid, self()}] ++ scoper:collect(), Format, Args).

get_scope_name(client, #{client_scope := ScopeName}) ->
    ScopeName;
get_scope_name(client, _) ->
    'woody.client';
get_scope_name(server, #{server_scope := ScopeName}) ->
    ScopeName;
get_scope_name(server, _) ->
    'woody.server'.

rpc_id_to_map(undefined) ->
    #{};
rpc_id_to_map(RpcID) ->
    RpcID.

final_error_cleanup(#{role := server, error := _, final := true}) ->
    scoper:remove_scope();
final_error_cleanup(_) ->
    ok.
