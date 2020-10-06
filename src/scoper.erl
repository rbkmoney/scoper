-module(scoper).

%% API
-export([scope/2]).
-export([scope/4]).
-export([add_scope/1]).
-export([add_scope/3]).
-export([remove_scope/1]).
-export([remove_scope/2]).
-export([add_meta/2]).
-export([remove_meta/2]).
-export([get_current_scope/1]).
-export([collect/1]).
-export([clear/1]).
-export([get_scope_names/1]).

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
    scope(ok, Name, #{}, Fun).

-spec scope(_, scope(), meta(), fun()) ->
    _.
scope(Key, Name, Meta, Fun) ->
    try
        add_scope(Key, Name, Meta),
        Fun()
    after
        remove_scope(Name)
    end.

-spec add_scope(scope()) ->
    ok.
add_scope(Name) ->
    add_scope(ok, Name, #{}).

-spec add_scope(_, scope(), meta()) ->
    ok.
add_scope(Key, Name, Meta) ->
    Scopes = get_scope_names(Key),
    case lists:member(Name, [?TAG | Scopes]) of
        true ->
            ok = error_logger:warning_msg("Scoper: attempt to add taken scope ~p; scopes: ~p", [Name, Scopes]);
        false ->
            set_scope_names(Key, [Name | Scopes]),
            store(Name, Meta)
    end.

-spec remove_scope(_) ->
    ok.
remove_scope(Key) ->
    case get_scope_names(Key) of
        [] ->
            remove_scope(Key, '$remove_scope/0', []);
        Scopes = [Current | _] ->
            remove_scope(Key, Current, Scopes)
    end.

-spec remove_scope(_, scope()) ->
    ok.
remove_scope(Key, Name) ->
    remove_scope(Key, Name, get_scope_names(Key)).

-spec add_meta(_, meta()) ->
    ok.
add_meta(_, Meta) when map_size(Meta) =:= 0 ->
    ok;
add_meta(Key, Meta) ->
    try
        ScopeName = get_current_scope(Key),
        store(ScopeName, maps:merge(find(ScopeName), Meta))
    catch
        throw:{scoper, no_scopes} ->
            ok = logger:warning("Scoper: attempt to add meta: ~p when no scopes set", [Meta])
    end.

-spec remove_meta(_, [key()]) ->
    ok.
remove_meta(Key, Keys) ->
    try
        ScopeName = get_current_scope(Key),
        store(ScopeName, maps:without(Keys, find(ScopeName)))
    catch
        throw:{scoper, no_scopes} ->
            ok = logger:warning("Scoper: attempt to remove meta keys ~p when no scopes set", [Keys])
    end.

-spec get_current_scope(_) ->
    scope() | no_return().
get_current_scope(Key) ->
    case get_scope_names(Key) of
        [] ->
            erlang:throw({scoper, no_scopes});
        Scopes ->
            hd(Scopes)
    end.

-spec collect(_) ->
    data().
collect(Key) ->
    scoper_storage:collect(Key).

-spec clear(_) ->
    ok.
clear(Key) ->
    scoper_storage:delete(Key).

-spec get_scope_names(_) ->
    [scope()].
get_scope_names(Key) ->
    case find(Key, ?TAG) of
        undefined ->
            [];
        Scopes ->
            Scopes
    end.


%%
%% Internal functions
%%
-spec set_scope_names(_, [scope()]) ->
    ok.
set_scope_names(Key, Names) ->
    store(Key, ?TAG, Names).


-spec remove_scope(_, scope(), [scope()]) ->
     ok.
remove_scope(_, Name, Scopes = []) ->
    ok = error_logger:warning_msg("Scoper: attempt to remove scope ~p from the scopes stack: ~p", [Name, Scopes]);
remove_scope(Key, Name, [Name | Rest]) ->
    ok = delete(Key, Name),
    ok = set_scope_names(Key, Rest);
remove_scope(_, Name, Scopes) ->
    ok = error_logger:warning_msg("Scoper: attempt to remove scope ~p from the scopes stack: ~p", [Name, Scopes]).

-spec store(scope(), scoper_storage:payload()) ->
    ok.
store(Scope, Value) ->
    store(ok, Scope, Value).

store(Key, Scope, Value) ->
    scoper_storage:store(Key, Scope, Value).

-spec find(scope()) ->
    scoper_storage:payload() | undefined.
find(Scope) ->
    find(ok, Scope).

-spec find(_, scope()) ->
    scoper_storage:payload() | undefined.
find(Key, Scope) ->
    scoper_storage:find(Key, Scope).

% -spec delete(scope()) ->
%     ok.
% delete(Scope) ->
%    delete(ok, Scope).

-spec delete(_, scope()) ->
    ok.
delete(Key, Scope) ->
    scoper_storage:delete(Key, Scope).
