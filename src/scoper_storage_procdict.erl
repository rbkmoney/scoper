-module(scoper_storage_procdict).

-behaviour(scoper_storage).

%% scoper_storage behaviour callbacks.
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
    put_data(maps:merge(get_data(), #{ScopeName => Payload})).

-spec find(_, scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(_, ScopeName) ->
    maps:get(ScopeName, get_data(), undefined).

-spec delete(_) ->
    ok.
delete(_) ->
    put_data(#{}).

-spec delete(_, scoper_storage:scope()) ->
    ok.
delete(_, ScopeName) ->
    put_data(maps:without([ScopeName], get_data())).

-spec collect(_) ->
    scoper_storage:data().
collect(_) ->
    get_data().


%%
%% Internal functions
%%
-spec put_data(scoper_storage:data()) ->
    ok.
put_data(Data) ->
    _ = erlang:put(?MODULE, Data),
    ok.

-spec get_data() ->
   scoper_storage:data().
get_data() ->
    case erlang:get(?MODULE) of
        undefined -> #{};
        Data      -> Data
    end.
