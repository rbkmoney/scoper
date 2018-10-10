-module(scoper_storage).

%% API
-export([store/2]).
-export([find/1]).
-export([delete/0]).
-export([delete/1]).
-export([collect/0]).

%% Types
-type key()     :: atom().
-type value()   :: any().
-type meta()    :: #{key() => value()}.
-type scope()   :: key().
-type payload() :: meta() | [scope()].
-type data()    :: #{scope() => payload()}.

-export_type([key/0, value/0, meta/0, scope/0, payload/0, data/0]).

%%
%% Behaviour definition
%%
-callback store(scope(), payload()) ->
    ok.

-callback find(scope()) ->
    payload() | undefined.

-callback delete() ->
    ok.

-callback delete(scope()) ->
    ok.

-callback collect() ->
    data().


%%
%% API
%%
-spec store(scope(), payload()) ->
    ok.
store(ScopeName, Payload) ->
    (logger()):store(ScopeName, Payload).

-spec find(scope()) ->
    payload() | undefined.
find(ScopeName) ->
    (logger()):find(ScopeName).

-spec delete() ->
    ok.
delete() ->
    (logger()):delete().

-spec delete(scope()) ->
    ok.
delete(ScopeName) ->
    (logger()):delete(ScopeName).

-spec collect() ->
    data().
collect() ->
    (logger()):collect().


%%
%% Internal functions
%%
logger() ->
    application:get_env(scoper, storage, scoper_storage_procdict).
