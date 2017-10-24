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
handle_event(Event = 'call service', RpcID, RawMeta, _Opts) ->
    ok = scoper:add_scope(get_scope_name(client)),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'service result', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    scoper:remove_scope();

%% server scoping
handle_event(Event = 'server receive', RpcID, RawMeta, _Opts) ->
    ok = add_server_meta(RpcID),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'server send', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    remove_server_meta();

%% special cases
handle_event(Event = 'internal error', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    final_error_cleanup(RawMeta);
handle_event(Event = 'trace event', RpcID, RawMeta = #{role := Role}, _Opts) ->
    scoper:scope(
        get_scope_name(Role),
        fun() -> handle_event(Event, RpcID, RawMeta) end
    );

%% the rest
handle_event(Event, RpcID, RawMeta, _Opts) ->
    handle_event(Event, RpcID, RawMeta).


%%
%% Internal functions
%%
handle_event(Event, RpcID, RawMeta = #{role := Role}) ->
    {Level, {Format, Args}, Meta} = woody_event_handler:format_event_and_meta(
        Event,
        RawMeta,
        RpcID,
        [event, service, function, type, metadata, url]
    ),
    ok = scoper:add_meta(Meta),
    lager:log(Level, collect_md(Role, RpcID), Format, Args).

%% Log metadata should contain rpc ID properties (trace_id, span_id and parent_id)
%% _on the top level_ according to the requirements.
%% In order to add rpc ID to log messages from woody handler, it is stored
%% in lager:md() in case of woody server. Since woody client can be invoked during
%% processing of parent request by a woody server handler, rpc ID of the child request
%% is added directly to the log meta before logging. It is _not stored_ in lager:md()
%% in that case, so child rpc ID does not override parent rpc ID
%% for the server handler processing context.
collect_md(client, RpcID) ->
    collect_md(add_rpc_id(RpcID, lager:md()));
collect_md(server, _RpcID) ->
    collect_md(lager:md()).

collect_md(MD) ->
    [{pid, self()}] ++ MD.

get_scope_name(client) ->
    'rpc.client';
get_scope_name(server) ->
    'rpc.server'.

final_error_cleanup(#{role := server, error := _, final := true}) ->
    remove_server_meta();
final_error_cleanup(_) ->
    ok.

add_server_meta(RpcID) ->
    ok = scoper:add_scope(get_scope_name(server)),
    lager:md(add_rpc_id(RpcID, lager:md())).

remove_server_meta() ->
    ok = scoper:remove_scope(),
    lager:md(remove_rpc_id(lager:md())).

add_rpc_id(undefined, MD) ->
    MD;
add_rpc_id(RpcID, MD) ->
    maps:fold(
        fun(K, V, Acc) -> lists:keystore(K, 1, Acc, {K, V}) end,
        MD,
        RpcID
    ).

remove_rpc_id(MD) ->
    lists:filtermap(
        fun({Key, _}) ->
            Key =/= span_id andalso Key =/= trace_id andalso Key =/= parent_id;
           (_) ->
            true
        end,
        MD
    ).
