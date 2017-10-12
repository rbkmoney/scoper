-module(scoper_logger).

%% API
-export([keystore/2]).
-export([keyfind/1]).
-export([keydelete/1]).


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


%%
%% Internal functions
%%
logger() ->
    {ok, Logger} = application:get_env(scoper, logger),
    Logger.
