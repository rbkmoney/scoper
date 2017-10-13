-module(scoper_logger).

%% API
-export([keystore/2]).
-export([keyfind/1]).
-export([keydelete/1]).
-export([get_data/0]).


%% Types
-type data() :: [{scoper:scope_name(), scoper:payload()}].
-export_type([data/0]).


%%
%% Behaviour definition
%%
-callback keystore(scoper:key(), scoper:payload()) ->
    ok.

-callback keyfind(scoper:key()) ->
    {scoper:key(), scoper:payload()} |
    false.

-callback keydelete(scoper:key()) ->
    ok.

-callback get_data() ->
    data().


%%
%% API
%%
-spec keystore(scoper:key(), scoper:payload()) ->
    ok.
keystore(Key, Value) ->
    (logger()):keystore(Key, Value).

-spec keyfind(scoper:key()) ->
    {scoper:key(), scoper:payload()} |
    false.
keyfind(Key) ->
    (logger()):keyfind(Key).

-spec keydelete(scoper:key()) ->
    ok.
keydelete(Key) ->
    (logger()):keydelete(Key).

-spec get_data() ->
    data().
get_data() ->
    (logger()):get_data().


%%
%% Internal functions
%%
logger() ->
    {ok, Logger} = application:get_env(scoper, logger),
    Logger.
