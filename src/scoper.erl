-module(scoper).

%% API
-export([scope/2]).
-export([scope/3]).
-export([add_scope/1]).
-export([add_scope/2]).
-export([remove_scope/0]).
-export([remove_scope/1]).
-export([add_meta/1]).
-export([remove_meta/1]).
-export([get_current_scope/0]).
-export([collect/0]).
-export([clear/0]).
-export([get_scope_names/0]).
-export([get_process_meta/0]).

%% Types
-type scope() :: scoper_storage:scope().
-type meta()  :: scoper_storage:meta().
-type key()   :: scoper_storage:key().
-type data()  :: scoper_storage:data().

-export_type([key/0, meta/0, scope/0, data/0]).

-define(TAG, scoper).


%%
%% API
%%
-spec scope(scope(), fun()) ->
    _.
scope(Name, Fun) ->
    scope(Name, #{}, Fun).

-spec scope(scope(), meta(), fun()) ->
    _.
scope(Name, Meta, Fun) ->
    try
        add_scope(Name, Meta),
        Fun()
    after
        remove_scope(Name)
    end.

-spec add_scope(scope()) ->
    ok.
add_scope(Name) ->
    add_scope(Name, #{}).

-spec add_scope(scope(), meta()) ->
    ok.
add_scope(Name, Meta) ->
    Scopes = get_scope_names(),
    case lists:member(Name, [?TAG | Scopes]) of
        true ->
            ok = error_logger:warning_msg("Scoper: attempt to add taken scope ~p; scopes: ~p", [Name, Scopes]);
        false ->
            set_scope_names([Name | Scopes]),
            store(Name, Meta)
    end.

-spec remove_scope() ->
    ok.
remove_scope() ->
    case get_scope_names() of
        [] ->
            remove_scope('$remove_scope/0', []);
        Scopes = [Current | _] ->
            remove_scope(Current, Scopes)
    end.

-spec remove_scope(scope()) ->
    ok.
remove_scope(Name) ->
    remove_scope(Name, get_scope_names()).

-spec add_meta(meta()) ->
    ok.
add_meta(Meta) when map_size(Meta) =:= 0 ->
    ok;
add_meta(Meta) ->
    try
        ScopeName = get_current_scope(),
        store(ScopeName, maps:merge(find(ScopeName), Meta))
    catch
        throw:{scoper, no_scopes} ->
            ok = logger:warning("Scoper: attempt to add meta: ~p when no scopes set", [Meta])
    end.

-spec remove_meta([key()]) ->
    ok.
remove_meta(Keys) ->
    try
        ScopeName = get_current_scope(),
        store(ScopeName, maps:without(Keys, find(ScopeName)))
    catch
        throw:{scoper, no_scopes} ->
            ok = logger:warning("Scoper: attempt to remove meta keys ~p when no scopes set", [Keys])
    end.

-spec get_current_scope() ->
    scope() | no_return().
get_current_scope() ->
    case get_scope_names() of
        [] ->
            erlang:throw({scoper, no_scopes});
        Scopes ->
            hd(Scopes)
    end.

-spec collect() ->
    data().
collect() ->
    scoper_storage:collect().

-spec clear() ->
    ok.
clear() ->
    scoper_storage:delete().

-spec get_scope_names() ->
    [scope()].
get_scope_names() ->
    case find(?TAG) of
        undefined ->
            [];
        Scopes ->
            Scopes
    end.


%%
%% Internal functions
%%
-spec set_scope_names([scope()]) ->
    ok.
set_scope_names(Names) ->
    store(?TAG, Names).

-spec remove_scope(scope(), [scope()]) ->
     ok.
remove_scope(Name, Scopes = []) ->
    ok = error_logger:warning_msg("Scoper: attempt to remove scope ~p from the scopes stack: ~p", [Name, Scopes]);
remove_scope(Name, [Name | Rest]) ->
    ok = delete(Name),
    ok = set_scope_names(Rest);
remove_scope(Name, Scopes) ->
    ok = error_logger:warning_msg("Scoper: attempt to remove scope ~p from the scopes stack: ~p", [Name, Scopes]).

-spec store(scope(), scoper_storage:payload()) ->
    ok.
store(Key, Value) ->
    scoper_storage:store(Key, Value).

-spec find(scope()) ->
    scoper_storage:payload() | undefined.
find(Key) ->
    scoper_storage:find(Key).

-spec delete(scope()) ->
    ok.
delete(Key) ->
    scoper_storage:delete(Key).

-spec get_process_meta() ->
    logger:metadata().
get_process_meta() ->
    case logger:get_process_metadata() of
        undefined -> #{};
        Metadata -> Metadata
    end.
