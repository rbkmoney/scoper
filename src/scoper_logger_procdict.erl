-module(scoper_logger_procdict).

-behaviour(scoper_logger).

%% scoper_logger behaviour callbacks.
-export([keystore/2]).
-export([keyfind/1]).
-export([keydelete/1]).
-export([collect/0]).


%%
%% scoper_logger behaviour callbacks
%%
-spec keystore(scoper:key(), scoper:payload()) ->
    ok.
keystore(Key, Value) ->
    put_data(lists:keystore(Key, 1, collect(), {Key, Value})).

-spec keyfind(scoper:key()) ->
    {scoper:key(), scoper:payload()} |
    false.
keyfind(Key) ->
    lists:keyfind(Key, 1, collect()).

-spec keydelete(scoper:key()) ->
    ok.
keydelete(Key) ->
    put_data(lists:keydelete(Key, 1, collect())).

-spec collect() ->
    scoper:data().
collect() ->
    case erlang:get(?MODULE) of
        undefined -> [];
        Data      -> Data
    end.


%%
%% Internal functions
%%
-spec put_data(scoper:data()) ->
    ok.
put_data(Data) ->
    _ = erlang:put(?MODULE, Data),
    ok.
