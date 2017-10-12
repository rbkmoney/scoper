-module(scoper_logger_lager).

-behaviour(scoper_logger).

%% API
-export([get_data/0]).


%% scoper_logger behaviour callbacks
-export([keystore/2]).
-export([keyfind/1]).
-export([keydelete/1]).


%% Types
-type data() :: [{scoper:scope_name(), scoper:payload()}].
-export_type([data/0]).


%%
%% API
%%
-spec get_data() ->
    data().
get_data() ->
    lager:md().


%%
%% scoper_logger behaviour callbacks
%%
-spec keystore(scoper:key(), scoper:payload()) ->
    ok.
keystore(Key, Value) ->
    lager:md(lists:keystore(Key, 1, lager:md(), {Key, Value})).

-spec keyfind(scoper:key()) ->
    {scoper:key(), scoper:payload()} |
    false.
keyfind(Key) ->
    lists:keyfind(Key, 1, lager:md()).

-spec keydelete(scoper:key()) ->
    ok.
keydelete(Key) ->
    lager:md(lists:keydelete(Key, 1, lager:md())).
