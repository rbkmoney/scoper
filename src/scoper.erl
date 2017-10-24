-module(scoper).

%% API
-export([scope/2]).
-export([scope/3]).
-export([add_scope/1]).
-export([remove_scope/0]).
-export([add_meta/1]).
-export([remove_meta/1]).
-export([get_current_scope/0]).
-export([collect/0]).

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
        add_scope(Name),
        add_meta(Meta),
        Fun()
    after
        remove_scope()
    end.

-spec add_scope(scope()) ->
    ok.
add_scope(Name) ->
    set_scope_names([Name | get_scope_names()]),
    store(Name, #{}).

-spec remove_scope() ->
    ok.
remove_scope() ->
    case get_scope_names() of
        [] ->
            ok;
        [Current | Rest] ->
            ok = delete(Current),
            ok = set_scope_names(Rest)
    end.

-spec add_meta(meta()) ->
    ok.
add_meta(Meta) when map_size(Meta) =:= 0 ->
    ok;
add_meta(Meta) ->
    ScopeName = get_current_scope(),
    case find(ScopeName) of
        undefined ->
            erlang:error(badarg, [Meta, ScopeName]);
        ScopeMeta ->
            store(ScopeName, maps:merge(ScopeMeta, Meta))
    end.

-spec remove_meta([key()]) ->
    ok.
remove_meta(Keys) ->
    ScopeName = get_current_scope(),
    case find(ScopeName) of
        undefined ->
            ok;
        ScopeMeta ->
            store(ScopeName, maps:without(Keys, ScopeMeta))
    end.

-spec get_current_scope() ->
    scope().
get_current_scope() ->
    hd(get_scope_names()).

-spec collect() ->
    data().
collect() ->
    scoper_storage:collect().


%%
%% Internal functions
%%
-spec get_scope_names() ->
    [scope()].
get_scope_names() ->
    case find(?TAG) of
        undefined ->
            [];
        Scopes ->
            Scopes
    end.

-spec set_scope_names([scope()]) ->
    ok.
set_scope_names(Names) ->
    store(?TAG, Names).

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
