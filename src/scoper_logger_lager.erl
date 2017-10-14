-module(scoper_logger_lager).

-behaviour(scoper_logger).

%% scoper_logger behaviour callbacks
-export([keystore/2]).
-export([keyfind/1]).
-export([keydelete/1]).
-export([collect/0]).


%%
%% scoper_logger behaviour callbacks
%%
-spec keystore(scoper:scope_name(), scoper:payload()) ->
    ok.
keystore(ScopeName, Payload) ->
    lager:md(lists:keystore(ScopeName, 1, lager:md(), {ScopeName, Payload})).

-spec keyfind(scoper:scope_name()) ->
    {scoper:scope_name(), scoper:payload()} |
    false.
keyfind(ScopeName) ->
    lists:keyfind(ScopeName, 1, lager:md()).

-spec keydelete(scoper:scope_name()) ->
    ok.
keydelete(ScopeName) ->
    lager:md(lists:keydelete(ScopeName, 1, lager:md())).

-spec collect() ->
    scoper:data().
collect() ->
    lager:md().
