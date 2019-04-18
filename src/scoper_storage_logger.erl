-module(scoper_storage_logger).

-behaviour(scoper_storage).

%% scoper_storage behaviour callbacks
-export([store/2]).
-export([find/1]).
-export([delete/0]).
-export([delete/1]).
-export([collect/0]).
-export([get_process_meta/0]).


%%
%% scoper_storage behaviour callbacks
%%
-spec store(scoper_storage:scope(), scoper_storage:payload()) ->
    ok.
store(ScopeName, Payload) ->
    CurrScope = get_process_meta(),
    logger:update_process_metadata(CurrScope#{ScopeName => Payload}).

-spec find(scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(ScopeName) ->
    maps:get(ScopeName, get_process_meta(), undefined).

-spec delete() ->
    ok.
delete() ->
    logger:set_process_metadata(#{}).

-spec delete(scoper_storage:scope()) ->
    ok.
delete(ScopeName) ->
    CurrScope = get_process_meta(),
    logger:set_process_metadata(maps:remove(ScopeName, CurrScope)).

-spec collect() ->
    scoper_storage:data().
collect() ->
    get_process_meta().

-spec get_process_meta() ->
    scoper_storage:data().
get_process_meta() ->
    case logger:get_process_metadata() of
        undefined -> #{};
        Metadata -> Metadata
    end.
