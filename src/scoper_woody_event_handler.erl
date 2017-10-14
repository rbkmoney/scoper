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

handle_event(Event, RpcID, RawMeta, Opts) ->
    {Level, {Format, Args}, Meta} = woody_event_handler:format_event_and_meta(
        Event,
        RawMeta,
        RpcID,
        [role, event, service, function, type, metadata, url]
    ),
    scoper:scope(
        get_scope(maps:get(role, Meta), Opts),
        fun() ->
            lager:log(
                Level,
                [{pid, self()}] ++ scoper:collect(),
                Format,
                Args
            )
        end,
        maps:merge(Meta, rpc_id_to_map(RpcID))
    ).


%%
%% Internal functions
%%
get_scope(client, #{client_scope := ScopeName}) ->
    ScopeName;
get_scope(client, _) ->
    'woody.client';
get_scope(server, #{server_scope := ScopeName}) ->
    ScopeName;
get_scope(server, _) ->
    'woody.server'.

rpc_id_to_map(undefined) ->
    #{};
rpc_id_to_map(RpcID) ->
    RpcID.
