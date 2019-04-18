-module(scoper_storage_logger).

-behaviour(scoper_storage).

%% scoper_storage behaviour callbacks
-export([store/2]).
-export([find/1]).
-export([delete/0]).
-export([delete/1]).
-export([collect/0]).


%%
%% scoper_storage behaviour callbacks
%%
-spec store(scoper_storage:scope(), scoper_storage:payload()) ->
    ok.
store(ScopeName, Payload) ->
    CurrScope = scoper:get_process_meta(),
    logger:set_process_metadata(CurrScope#{ScopeName => Payload}). % update perhaps?

-spec find(scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(ScopeName) ->
    maps:get(ScopeName, scoper:get_process_meta(), undefined).

-spec delete() ->
    ok.
delete() ->
    logger:set_process_metadata(#{}).

-spec delete(scoper_storage:scope()) ->
    ok.
delete(ScopeName) ->
    CurrScope = scoper:get_process_meta(),
    logger:set_process_metadata(maps:remove(ScopeName, CurrScope)).

-spec collect() ->
    scoper_storage:data().
collect() ->
    scoper:get_process_meta().

