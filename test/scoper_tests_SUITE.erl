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
-spec all() -> [test_name()].
all() ->
    [
        {group, procdict},
        {group, lager},
        {group, logger}
    ].

-spec groups() -> [{group_name(), [parallel | sequence], [test_name()]}].
groups() ->
    [
        {procdict, [sequence], [
            mirror_scoper_state_in_meta,
            play_with_meta
        ]},
        {lager, [sequence], [
            mirror_scoper_state_in_meta,
            play_with_meta
        ]},
        {logger, [sequence], [
            mirror_scoper_state_in_meta,
            play_with_meta
        ]}
    ].

%%
%% Starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(scoper),
    [{apps, Apps} | C].

-spec end_per_suite(config()) -> any().
end_per_suite(C) ->
    [application:stop(App) || App <- proplists:get_value(apps, C)].

-spec init_per_group(group_name(), config()) -> config().
init_per_group(procdict, C) ->
    init_group(scoper_storage_procdict, C);
init_per_group(lager, C) ->
    init_group(scoper_storage_lager, C);
init_per_group(logger, C) ->
    init_group(scoper_storage_logger, C).

init_group(Logger, C) ->
    ok = application:set_env(scoper, storage, Logger),
    [{scoper_storage, Logger} | C].

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_, C) ->
    application:unset_env(scoper, storage),
    lists:keydelete(scoper_storage, 1, C).

%%
%% Tests
%%
-spec mirror_scoper_state_in_meta(config()) -> ok.
mirror_scoper_state_in_meta(_C) ->
    mirror_scoper_state_in_meta([scope1, scope2, scope3], []).

mirror_scoper_state_in_meta([], State) ->
    ok = match_scoper_state_and_meta(State);
mirror_scoper_state_in_meta([Scope | NextScopes], State) ->
    ok = match_scoper_state_and_meta(State),
    NextState = [Scope | State],
    scoper:scope(
        Scope,
        #{scopes => NextState},
        fun() -> mirror_scoper_state_in_meta(NextScopes, NextState) end
    ),
    ok = match_scoper_state_and_meta(State).

-spec play_with_meta(config()) -> ok.
play_with_meta(_C) ->
    %% Try to operate on non initilized scopes
    ok = scoper:add_meta(#{dummy => dummy}),
    #{} = scoper:collect(),
    ok = scoper:remove_meta(dummy),
    #{} = scoper:collect(),

    %% Create scope1 and add key1
    ok = scoper:add_scope(scope1),
    #{} = find(scope1),
    ok = scoper:add_meta(#{key1 => dummy}),
    #{key1 := dummy} = find(scope1),

    %% Create scope2 and add key2
    ok = scoper:add_scope(scope2, #{key2 => dummy}),
    #{} = find(scope2),
    #{key2 := dummy} = find(scope2),

    %% Update key2 in scope2
    ok = scoper:add_meta(#{key2 => not_so_dummy}),
    #{key2 := not_so_dummy} = find(scope2),

    %% No key1 in scope2
    undefined = maps:get(key1, find(scope2), undefined),

    %% Add key1 to scope2
    ok = scoper:add_meta(#{key1 => smart}),
    #{key1 := dummy} = find(scope1),
    #{key1 := smart} = find(scope2),

    %% Remove key1 from scope2
    ok = scoper:remove_meta([key1]),
    #{key1 := dummy} = find(scope1),
    undefined = maps:get(key1, find(scope2), undefined),

    %% Try to create scopes: scope1 and scope2 again, and also reserved 'scoper'
    Data = scoper:collect(),
    ok = scoper:add_scope(scope1),
    Data = scoper:collect(),
    ok = scoper:add_scope(scope1),
    Data = scoper:collect(),
    ok = scoper:add_scope(scope2),
    Data = scoper:collect(),
    ok = scoper:add_scope(scoper),
    Data = scoper:collect(),

    %% Try to remove scope1 (still in scope2 now)
    ok = scoper:remove_scope(scope1),
    Data = scoper:collect(),

    %% Try to remove unexisting scope5 (still in scope2 now)
    ok = scoper:remove_scope(scope5),
    Data = scoper:collect(),

    %% Remove scope2, now in scope1
    ok = scoper:remove_scope(scope2),
    undefined = find(scope2),
    #{key1 := dummy} = find(scope1),

    %% Add key2 to scope1
    ok = scoper:add_meta(#{key2 => dummy}),
    #{key1 := dummy, key2 := dummy} = find(scope1),

    %% Remove scope1, no scopes left
    ok = scoper:remove_scope(),
    undefined = find(scope1),
    #{} = scoper:collect(),

    %% Try to operate when no scopes are there
    ok = scoper:add_meta(#{dummy => dummy}),
    #{} = scoper:collect(),
    ok = scoper:remove_meta(dummy),
    #{} = scoper:collect(),

    %% Add scopes and clear them all
    ok = scoper:add_scope(scope1),
    ok = scoper:add_scope(scope2),
    #{} = find(scope1),
    #{} = find(scope2),
    ok = scoper:clear(),
    #{} = scoper:collect().

%%
%% Internal functions
%%
match_scoper_state_and_meta(State) ->
    match_scoper_state_and_meta(State, scoper:collect()).

match_scoper_state_and_meta([], #{}) ->
    ok;
match_scoper_state_and_meta(State, Data = #{?TAG := State}) ->
    lists:foldr(
        fun(Scope, Acc) ->
            do_match_scoper_state_and_meta(Scope, [Scope | Acc], Data)
        end,
        [],
        State
    ),
    ok.

do_match_scoper_state_and_meta(Scope, State, Meta) ->
    #{scopes := State} = maps:get(Scope, Meta),
    State.

find(Key) ->
    maps:get(Key, scoper:collect(), undefined).
