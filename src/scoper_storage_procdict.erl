-module(scoper_storage_procdict).

-behaviour(scoper_storage).

%% scoper_storage behaviour callbacks.
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
    put_data(maps:merge(get_data(), #{ScopeName => Payload})).

-spec find(scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(ScopeName) ->
    maps:get(ScopeName, get_data(), undefined).

-spec delete() ->
    ok.
delete() ->
    put_data(#{}).

-spec delete(scoper_storage:scope()) ->
    ok.
delete(ScopeName) ->
    put_data(maps:without([ScopeName], get_data())).

-spec collect() ->
    scoper_storage:data().
collect() ->
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
