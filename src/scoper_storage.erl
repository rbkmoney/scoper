-module(scoper_storage).

%% API
-export([store/3]).
-export([find/2]).
-export([delete/1]).
-export([delete/2]).
-export([collect/1]).

%% Types
-type storage_key() :: any().
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
-callback store(storage_key(), scope(), payload()) ->
    ok.

-callback find(storage_key(), scope()) ->
    payload() | undefined.

-callback delete(storage_key()) ->
    ok.

-callback delete(storage_key(), scope()) ->
    ok.

-callback collect(storage_key()) ->
    data().


%%
%% API
%%
-spec store(storage_key(), scope(), payload()) ->
    ok.
store(StorageKey, ScopeName, Payload) ->
    (storage()):store(StorageKey, ScopeName, Payload).

-spec find(storage_key(), scope()) ->
    payload() | undefined.
find(StorageKey, ScopeName) ->
    (storage()):find(StorageKey, ScopeName).

-spec delete(storage_key()) ->
    ok.
delete(StorageKey) ->
    (storage()):delete(StorageKey).

-spec delete(storage_key(), scope()) ->
    ok.
delete(StorageKey, ScopeName) ->
    (storage()):delete(StorageKey, ScopeName).

-spec collect(storage_key()) ->
    data().
collect(StorageKey) ->
    (storage()):collect(StorageKey).


%%
%% Internal functions
%%
storage() ->
    application:get_env(scoper, storage, scoper_storage_procdict).
