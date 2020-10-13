-module(scoper_storage_lager).

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
-spec store(scoper_storage:scope(), scoper_storage:payload()) -> ok.
store(ScopeName, Payload) ->
    lager:md(lists:keystore(ScopeName, 1, lager:md(), {ScopeName, Payload})).

-spec find(scoper_storage:scope()) -> scoper_storage:payload() | undefined.
find(ScopeName) ->
    case lists:keyfind(ScopeName, 1, lager:md()) of
        {ScopeName, Payload} ->
            Payload;
        false ->
            undefined
    end.

-spec delete() -> ok.
delete() ->
    %% Below is a trick to avoid dialyzer warining for the lager:md/1 spec:
    %% -spec md([{atom(), any()},...]) -> ok.
    lager:md(lists:nthtail(1, [{what, ever}])).

-spec delete(scoper_storage:scope()) -> ok.
delete(ScopeName) ->
    lager:md(lists:keydelete(ScopeName, 1, lager:md())).

-spec collect() -> scoper_storage:data().
collect() ->
    maps:from_list(lager:md()).
