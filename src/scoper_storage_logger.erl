-module(scoper_storage_logger).

-behaviour(scoper_storage).

%% scoper_storage behaviour callbacks
-export([store/3]).
-export([find/2]).
-export([delete/1]).
-export([delete/2]).
-export([collect/1]).


%%
%% scoper_storage behaviour callbacks
%%
-spec store(_, scoper_storage:scope(), scoper_storage:payload()) ->
    ok.
store(_, ScopeName, Payload) ->
    CurrScope = collect(ok),
    logger:update_process_metadata(CurrScope#{ScopeName => Payload}).

-spec find(_, scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(_, ScopeName) ->
    maps:get(ScopeName, collect(ok), undefined).

-spec delete(_) ->
    ok.
delete(_) ->
    logger:set_process_metadata(#{}).

-spec delete(_, scoper_storage:scope()) ->
    ok.
delete(_, ScopeName) ->
    CurrScope = collect(ok),
    logger:set_process_metadata(maps:remove(ScopeName, CurrScope)).

-spec collect(_) ->
    scoper_storage:data().
collect(_) ->
    case logger:get_process_metadata() of
        undefined -> #{};
        Metadata -> Metadata
    end.
