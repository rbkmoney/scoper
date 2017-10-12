-module(scoper_tests_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).


-export([
    scope_ok/1,
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
            scope_ok,
            play_with_meta
        ]},
        {lager, [sequence], [
            scope_ok,
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
    [application_stop(App) || App <- proplists:get_value(apps, C)].

application_stop(App=sasl) ->
    %% hack for preventing sasl deadlock
    %% http://erlang.org/pipermail/erlang-questions/2014-May/079012.html
    error_logger:delete_report_handler(cth_log_redirect),
    application:stop(App),
    error_logger:add_report_handler(cth_log_redirect),
    ok;
application_stop(App) ->
    application:stop(App).

-spec init_per_group(group_name(), config()) ->
    config().

init_per_group(procdict, C) ->
    init_group(scoper_logger_procdict, C);
init_per_group(lager, C) ->
    init_group(scoper_logger_lager, C).

init_group(Logger, C) ->
    ok = application:set_env(scoper, logger, Logger),
    [{scoper_logger, Logger} | C].

-spec end_per_group(group_name(), config()) ->
    config().
end_per_group(_, C) ->
    application:unset_env(scoper, logger),
    lists:keydelete(scoper_logger, 1, C).


%%
%% Tests
%%
-spec scope_ok(config()) ->
    ok.
scope_ok(C) ->
    scope_ok_test(?config(scoper_logger, C)).

scope_ok_test(Logger) ->
    scope_ok_test([scope1, scope2, scope3], [], Logger).

scope_ok_test([], State, Logger) ->
    ok = validate_scopes(State, Logger);
scope_ok_test([Scope | T], State, Logger) ->
    ok = validate_scopes(State, Logger),
    NewState = [Scope | State],
    scoper:scope(
        Scope,
        fun() -> scope_ok_test(T, NewState, Logger) end,
        #{scopes => NewState}
    ),
    ok = validate_scopes(State, Logger).

-spec play_with_meta(config()) ->
    ok.
play_with_meta(_C) ->
    try scoper:set_meta(#{dummy => dummy}, scope1)
    catch
        error:badarg -> ok
    end,

    try scoper:remove_meta(dummy, scope1)
    catch
        error:badarg -> ok
    end,

    ok = scoper:add_scope(scope1),
    {_, #{}} = scoper_logger:keyfind(scope1),
    ok = scoper:set_meta(#{key1 => dummy}),
    {_, #{key1 := dummy}} = scoper_logger:keyfind(scope1),

    ok = scoper:add_scope(scope2),
    {_, #{}} = scoper_logger:keyfind(scope2),
    ok = scoper:set_meta(#{key2 => dummy}),
    {_, #{key2 := dummy}} = scoper_logger:keyfind(scope2),

    ok = scoper:set_meta(#{key3 => dummy}, scope1),
    {_, #{key1 := dummy, key3 := dummy}} = scoper_logger:keyfind(scope1),
    {_, #{key2 := dummy}} = scoper_logger:keyfind(scope2),

    ok = scoper:set_meta(#{key2 => not_so_dummy}),
    {_, #{key2 := not_so_dummy}} = scoper_logger:keyfind(scope2),

    ok = scoper:remove_meta([key2]),
    {_, Meta} = scoper_logger:keyfind(scope2),
    undefined = maps:get(get, Meta, undefined),

    ok = scoper:remove_meta([key3], scope1),
    {_, Meta1} = scoper_logger:keyfind(scope1),
    undefined = maps:get(get, Meta1, undefined),

    scoper:remove_scope(),
    false = scoper_logger:keyfind(scope2),
    {_, #{key1 := dummy}} = scoper_logger:keyfind(scope1),

    ok = scoper:set_meta(#{key2 => dummy}),
    {_, #{key1 := dummy, key2 := dummy}} = scoper_logger:keyfind(scope1),

    try scoper:set_meta(#{key4 => dummy}, scope2)
    catch
        error:badarg -> ok
    end,

    scoper:remove_scope(),
    false = scoper_logger:keyfind(scope1),

    try scoper:set_meta(#{dummy => dummy})
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
validate_scopes(State, Logger) ->
    do_validate_scopes(State, get_scopes_data(Logger)).

do_validate_scopes([], []) ->
    ok;
do_validate_scopes(State, RawData) ->
    {value, {?TAG, State}, RawMeta} = lists:keytake(?TAG, 1, RawData),
    ok = validate_scope_meta(State, RawMeta).

validate_scope_meta(State, Metadata) ->
    lists:foldr(
        fun(Scope, Acc) ->
            Acc1 = [Scope | Acc],
            {Scope, #{scopes := Acc1}} = lists:keyfind(Scope, 1, Metadata),
            Acc1
        end,
        [],
        State
    ),
    ok.

get_scopes_data(Logger) ->
    Logger:get_data().
