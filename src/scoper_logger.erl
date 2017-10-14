-module(scoper_logger).

%% API
-export([keystore/2]).
-export([keyfind/1]).
-export([keydelete/1]).
-export([collect/0]).


%%
%% Behaviour definition
%%
-callback keystore(scoper:scope_name(), scoper:payload()) ->
    ok.

-callback keyfind(scoper:scope_name()) ->
    {scoper:scope_name(), scoper:payload()} |
    false.

-callback keydelete(scoper:scope_name()) ->
    ok.

-callback collect() ->
    scoper:data().


%%
%% API
%%
-spec keystore(scoper:scope_name(), scoper:payload()) ->
    ok.
keystore(ScopeName, Payload) ->
    (logger()):keystore(ScopeName, Payload).

-spec keyfind(scoper:scope_name()) ->
    {scoper:scope_name(), scoper:payload()} |
    false.
keyfind(ScopeName) ->
    (logger()):keyfind(ScopeName).

-spec keydelete(scoper:scope_name()) ->
    ok.
keydelete(ScopeName) ->
    (logger()):keydelete(ScopeName).

-spec collect() ->
    scoper:data().
collect() ->
    (logger()):collect().


%%
%% Internal functions
%%
logger() ->
    application:get_env(scoper, logger, scoper_logger_procdict).
