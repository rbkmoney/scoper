-module(scoper_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).


%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event = ?EV_CALL_SERVICE, RpcID, RawMeta, Opts) ->
    ok = new_scope(client, Opts),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = ?EV_SERVICE_RESULT, RpcID, RawMeta, _Opts) ->
    ok = scoper:remove_scope(),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = ?EV_SERVER_RECEIVE, RpcID, RawMeta, Opts) ->
    ok = new_scope(server, Opts),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = ?EV_SERVER_SEND, RpcID, RawMeta, _Opts) ->
    ok = scoper:remove_scope(),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event, RpcID, RawMeta, _Opts) ->
    handle_event(Event, RpcID, RawMeta).


%%
%% Internal functions
%%
new_scope(client, #{client_scope := ScopeName}) ->
    scoper:add_scope(ScopeName);
new_scope(client, _) ->
    scoper:add_scope('woody.client');
new_scope(server, #{server_scope := ScopeName}) ->
    scoper:add_scope(ScopeName);
new_scope(server, _) ->
    scoper:add_scope('woody.server').

handle_event(Event, RpcID, RawMeta) ->
    {Level, {Format, Args}, Meta} = woody_event_handler:format_event_and_meta(
        Event,
        RawMeta,
        RpcID,
        [event, service, function, type, metadata, url]
    ),
    ok = scoper:set_meta(Meta),
    lager:log(Level, [{pid, self()}] ++ maps:to_list(RpcID) ++ scoper:dump_meta(), Format, Args).
