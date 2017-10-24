-module(scoper_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).


-export([
    mirror_scoper_state_in_meta/1,
    play_with_meta/1
]).


-type test_name() :: atom().
-type group_name() :: atom().
-type config() :: [{atom(), _}].

-define(TAG, scoper).


%%
%% Tests descriptions
%%
-spec all() ->
    [test_name()].
all() ->
    [
        {group, procdict},
        {group, lager}
    ].

-spec groups() ->
    [{group_name(), [parallel | sequence], [test_name()]}].

groups() ->
    [
        {procdict, [sequence], [
            mirror_scoper_state_in_meta,
            play_with_meta
        ]},
        {lager, [sequence], [
            mirror_scoper_state_in_meta,
            play_with_meta
        ]}
    ].


%%
%% Starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(scoper),
    [{apps, Apps}|C].

-spec end_per_suite(config()) ->
    any().
end_per_suite(C) ->
    [application:stop(App) || App <- proplists:get_value(apps, C)].

-spec init_per_group(group_name(), config()) ->
    config().

init_per_group(procdict, C) ->
    init_group(scoper_storage_procdict, C);
init_per_group(lager, C) ->
    init_group(scoper_storage_lager, C).

init_group(Logger, C) ->
    ok = application:set_env(scoper, storage, Logger),
    [{scoper_storage, Logger} | C].

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_, C) ->
    application:unset_env(scoper, storage),
    lists:keydelete(scoper_storage, 1, C).


%%
%% Tests
%%
-spec mirror_scoper_state_in_meta(config()) ->
    ok.
mirror_scoper_state_in_meta(_C) ->
    mirror_scoper_state_in_meta([scope1, scope2, scope3], []).

mirror_scoper_state_in_meta([], State) ->
    ok = 'match_scope_meta_and_scope_state'(State);
mirror_scoper_state_in_meta([Scope | T], State) ->
    ok = 'match_scope_meta_and_scope_state'(State),
    ScopeState = [Scope | State],
    scoper:scope(
        Scope,
        #{scopes => ScopeState},
        fun() -> mirror_scoper_state_in_meta(T, ScopeState) end
    ),
    ok = 'match_scope_meta_and_scope_state'(State).

-spec play_with_meta(config()) ->
    ok.
play_with_meta(_C) ->

    %% Try to operate on non initilized scopes
    try scoper:add_meta(#{dummy => dummy})
    catch
        error:badarg -> ok
    end,
    try scoper:remove_meta(dummy)
    catch
        error:badarg -> ok
    end,

    %% Create scope1 and add key1
    ok               = scoper:add_scope(scope1),
    #{}              = find(scope1),
    ok               = scoper:add_meta(#{key1 => dummy}),
    #{key1 := dummy} = find(scope1),

    %% Create scope2 and add key2
    ok               = scoper:add_scope(scope2),
    #{}              = find(scope2),
    ok               = scoper:add_meta(#{key2 => dummy}),
    #{key2 := dummy} = find(scope2),

    %% Update key2 in scope2
    ok                      = scoper:add_meta(#{key2 => not_so_dummy}),
    #{key2 := not_so_dummy} = find(scope2),

    %% No key1 in scope2
    undefined = maps:get(key1, find(scope2), undefined),

    %% Add key1 to scope2
    ok = scoper:add_meta(#{key1 => smart}),
    #{key1 := dummy} = find(scope1),
    #{key1 := smart} = find(scope2),

    %% Remove key1 from scope2
    ok               = scoper:remove_meta([key1]),
    #{key1 := dummy} = find(scope1),
    undefined        = maps:get(key1, find(scope2), undefined),

    %% Remove scope2, now in scope1
    scoper:remove_scope(),
    undefined = find(scope2),
    #{key1 := dummy}                = find(scope1),

    %% Add key2 to scope1
    ok                              = scoper:add_meta(#{key2 => dummy}),
    #{key1 := dummy, key2 := dummy} = find(scope1),

    %% Remove scope1, no scopes left
    scoper:remove_scope(),
    undefined = find(scope1),

    %% Try to operate when no scopes are there
    try scoper:add_meta(#{dummy => dummy})
    catch
        error:badarg -> ok
    end,

    try scoper:remove_meta([dummy])
    catch
        error:badarg -> ok
    end.

%%
%% Internal functions
%%
'match_scope_meta_and_scope_state'(State) ->
    'match_scope_meta_and_scope_state'(State, scoper:collect()).

'match_scope_meta_and_scope_state'([], #{}) ->
    ok;
'match_scope_meta_and_scope_state'(State, Data = #{?TAG := State}) ->
    lists:foldr(
        fun(Scope, Acc) ->
            'scope_meta=:=scoper_state'(Scope, [Scope | Acc], Data)
        end,
        [],
        State
    ),
    ok.

'scope_meta=:=scoper_state'(Scope, State, Meta) ->
    #{scopes := State} = maps:get(Scope, Meta),
    State.

find(Key) ->
    maps:get(Key, scoper:collect(), undefined).
