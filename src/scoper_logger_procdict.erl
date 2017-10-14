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
-spec keystore(scoper:scope_name(), scoper:payload()) ->
    ok.
keystore(ScopeName, Payload) ->
    put_data(lists:keystore(ScopeName, 1, collect(), {ScopeName, Payload})).

-spec keyfind(scoper:scope_name()) ->
    {scoper:scope_name(), scoper:payload()} |
    false.
keyfind(ScopeName) ->
    lists:keyfind(ScopeName, 1, collect()).

-spec keydelete(scoper:scope_name()) ->
    ok.
keydelete(ScopeName) ->
    put_data(lists:keydelete(ScopeName, 1, collect())).

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
