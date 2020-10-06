-module(scoper_storage_lager).

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
    lager:md(lists:keystore(ScopeName, 1, lager:md(), {ScopeName, Payload})).

-spec find(_, scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(_, ScopeName) ->
    case lists:keyfind(ScopeName, 1, lager:md()) of
        {ScopeName, Payload} ->
            Payload;
        false ->
            undefined
    end.

-spec delete(_) ->
    ok.
delete(_) ->
    %% Below is a trick to avoid dialyzer warining for the lager:md/1 spec:
    %% -spec md([{atom(), any()},...]) -> ok.
    lager:md(lists:nthtail(1, [{what, ever}])).

-spec delete(_, scoper_storage:scope()) ->
    ok.
delete(_, ScopeName) ->
    lager:md(lists:keydelete(ScopeName, 1, lager:md())).

-spec collect(_) ->
    scoper_storage:data().
collect(_) ->
    maps:from_list(lager:md()).
