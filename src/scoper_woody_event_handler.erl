-module(scoper_woody_event_handler).

%% Commented out to compile the module without woody
%% -behaviour(woody_event_handler).

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

-type options() :: #{
    event_handler_opts => woody_event_handler:options()
}.

-export_type([options/0]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta :: woody_event_handler:event_meta(),
    Opts :: options().
%% client scoping
handle_event(Event = 'client begin', RpcID, RawMeta, Opts) ->
    ok = scoper:add_scope(get_scope_name(client)),
    do_handle_event(Event, RpcID, RawMeta, Opts);
handle_event(Event = 'client cache begin', RpcID, RawMeta, Opts) ->
    ok = scoper:add_scope(get_scope_name(caching_client)),
    do_handle_event(Event, RpcID, RawMeta, Opts);
handle_event(Event = 'client end', RpcID, RawMeta, Opts) ->
    ok = do_handle_event(Event, RpcID, RawMeta, Opts),
    scoper:remove_scope();
handle_event(Event = 'client cache end', RpcID, RawMeta, Opts) ->
    ok = do_handle_event(Event, RpcID, RawMeta, Opts),
    scoper:remove_scope();
%% server scoping
handle_event(Event = 'server receive', RpcID, RawMeta, Opts) ->
    ok = add_server_meta(RpcID),
    do_handle_event(Event, RpcID, RawMeta, Opts);
handle_event(Event = 'server send', RpcID, RawMeta, Opts) ->
    ok = do_handle_event(Event, RpcID, RawMeta, Opts),
    remove_server_meta();
%% special cases
handle_event(Event = 'internal error', RpcID, RawMeta, Opts) ->
    ok = do_handle_event(Event, RpcID, RawMeta, Opts),
    final_error_cleanup(RawMeta);
handle_event(Event = 'trace event', RpcID, RawMeta = #{role := Role}, Opts) ->
    case lists:member(get_scope_name(Role), scoper:get_scope_names()) of
        true ->
            do_handle_event(Event, RpcID, RawMeta, Opts);
        false ->
            scoper:scope(
                get_scope_name(Role),
                fun() -> do_handle_event(Event, RpcID, RawMeta, Opts) end
            )
    end;
%% the rest
handle_event(Event, RpcID, RawMeta, Opts) ->
    do_handle_event(Event, RpcID, RawMeta, Opts).

%%
%% Internal functions
%%
do_handle_event(Event, _RpcID, _RawMeta, _Opts) when
    Event =:= 'client begin' orelse
        Event =:= 'client end' orelse
        Event =:= 'client cache begin' orelse
        Event =:= 'client cache end'
->
    ok;
do_handle_event(Event, RpcID, RawMeta = #{role := Role}, Opts) ->
    {Level, {Format, Args}, Meta} = woody_event_handler:format_event_and_meta(
        Event,
        RawMeta,
        RpcID,
        [event, service, function, type, metadata, url, deadline, execution_duration_ms],
        get_event_handler_options(Opts)
    ),
    ok = scoper:add_meta(Meta),
    logger:log(Level, Format, Args, collect_md(Role, RpcID));
do_handle_event(_Event, _RpcID, _RawMeta, _Opts) ->
    ok.

%% Log metadata should contain rpc ID properties (trace_id, span_id and parent_id)
%% _on the top level_ according to the requirements.
%% In order to add rpc ID to log messages from woody handler, it is stored
%% in lager:md() in case of woody server. Since woody client can be invoked during
%% processing of parent request by a woody server handler, rpc ID of the child request
%% is added directly to the log meta before logging. It is _not stored_ in lager:md()
%% in that case, so child rpc ID does not override parent rpc ID
%% for the server handler processing context.
collect_md(client, RpcID) ->
    collect_md(add_rpc_id(RpcID, scoper:collect()));
collect_md(server, _RpcID) ->
    collect_md(scoper:collect()).

collect_md(MD) ->
    MD#{pid => self()}.

get_scope_name(client) ->
    'rpc.client';
get_scope_name(caching_client) ->
    'rpc.caching_client';
get_scope_name(server) ->
    'rpc.server'.

final_error_cleanup(#{role := server, error := _, final := true}) ->
    remove_server_meta();
final_error_cleanup(_) ->
    ok.

add_server_meta(RpcID) ->
    ok = scoper:add_scope(get_scope_name(server)),
    logger:set_process_metadata(add_rpc_id(RpcID, scoper:collect())).

remove_server_meta() ->
    _ =
        case scoper:get_current_scope() of
            'rpc.server' ->
                ok;
            _ ->
                logger:warning(
                    "Scoper woody event handler: removing uncleaned scopes on the server: ~p",
                    [scoper:get_scope_names()]
                )
        end,
    ok = scoper:clear().

add_rpc_id(undefined, MD) ->
    MD;
add_rpc_id(RpcID, MD) ->
    maps:merge(MD, RpcID).

%% Pass event_handler_opts only
get_event_handler_options(#{event_handler_opts := EventHandlerOptions}) ->
    EventHandlerOptions;
get_event_handler_options(_Opts) ->
    #{}.
