-module(scoper).

%% API
-export([scope/2]).
-export([scope/3]).
-export([add_scope/1]).
-export([remove_scope/0]).
-export([add_meta/1]).
-export([add_meta/2]).
-export([remove_meta/1]).
-export([remove_meta/2]).
-export([get_current_scope/0]).
-export([collect/0]).

%% Types
-type key()        :: atom().
-type value()      :: any().
-type meta()       :: #{key() => value()}.
-type scope_name() :: key().
-type payload()    :: meta() | [scope_name()].
-type data()       :: [{scoper:scope_name(), scoper:payload()}].

-export_type([key/0, value/0, meta/0, scope_name/0, payload/0, data/0]).

-define(TAG, scoper).


%%
%% API
%%
-spec scope(scope_name(), fun()) ->
    _.
scope(Name, Fun) ->
    scope(Name, Fun, #{}).

-spec scope(scope_name(), fun(), meta()) ->
    _.
scope(Name, Fun, Meta) ->
    try
        add_scope(Name),
        add_meta(Meta),
        Fun()
    after
        remove_scope()
    end.

-spec add_scope(scope_name()) ->
    ok.
add_scope(Name) ->
    set_scope_names([Name | get_scope_names()]),
    keystore(Name, #{}).

-spec remove_scope() ->
    ok.
remove_scope() ->
    case get_scope_names() of
        [] ->
            ok;
        [Current | Rest] ->
            ok = keydelete(Current),
            ok = set_scope_names(Rest)
    end.

-spec add_meta(meta()) ->
    ok.
add_meta(Meta) ->
    add_meta(Meta, get_current_scope()).

-spec add_meta(meta(), scope_name()) ->
    ok.
add_meta(Meta, _) when map_size(Meta) =:= 0 ->
    ok;
add_meta(Meta, ScopeName) ->
    case keyfind(ScopeName) of
        {ScopeName, ScopeMeta} ->
            keystore(ScopeName, maps:merge(ScopeMeta, Meta));
        false ->
            erlang:error(badarg, [Meta, ScopeName])
    end.

-spec remove_meta([key()]) ->
    ok.
remove_meta(Keys) ->
    remove_meta(Keys, get_current_scope()).

-spec remove_meta([key()], scope_name()) ->
    ok.
remove_meta(Keys, ScopeName) ->
    case keyfind(ScopeName) of
        {ScopeName, ScopeMeta} ->
            keystore(ScopeName, maps:filter(
                fun(K, _V) ->
                    not lists:member(K, Keys)
                end,
                ScopeMeta
            ));
        false ->
            ok
    end.

-spec get_current_scope() ->
    scope_name().
get_current_scope() ->
    hd(get_scope_names()).

-spec collect() ->
    data().
collect() ->
    scoper_logger:collect().


%%
%% Internal functions
%%
-spec get_scope_names() ->
    [scope_name()].
get_scope_names() ->
    case keyfind(?TAG) of
        {?TAG, Scopes} ->
            Scopes;
        false ->
            keystore(?TAG, []),
            []
    end.

-spec set_scope_names([scope_name()]) ->
    ok.
set_scope_names(Names) ->
    keystore(?TAG, Names).

-spec keystore(scope_name(), payload()) ->
    ok.
keystore(Key, Value) ->
    scoper_logger:keystore(Key, Value).

-spec keyfind(scope_name()) ->
    {scope_name(), payload()} |
    false.
keyfind(Key) ->
    scoper_logger:keyfind(Key).

-spec keydelete(scope_name()) ->
    ok.
keydelete(Key) ->
    scoper_logger:keydelete(Key).
