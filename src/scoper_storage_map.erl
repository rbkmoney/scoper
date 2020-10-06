-module(scoper_storage_map).
-behaviour(scoper_storage).

%% scoper_storage behaviour callbacks
-export([store/3]).
-export([find/2]).
-export([delete/1]).
-export([delete/2]).
-export([collect/1]).

%% gen_server

-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).

-spec store(scoper_storage:storage_key(), scoper_storage:scope(), scoper_storage:payload()) ->
    ok.
store(Key, ScopeName, Payload) ->
    ok = gen_server:call(?MODULE, {store, Key, ScopeName, Payload}).

-spec find(scoper_storage:storage_key(), scoper_storage:scope()) ->
    scoper_storage:payload() | undefined.
find(Key, ScopeName) ->
    {ok, Meta} = gen_server:call(?MODULE, {find, Key, ScopeName}),
    Meta.

-spec delete(scoper_storage:storage_key()) -> ok.
delete(Key) ->
    ok = gen_server:call(?MODULE, {delete, Key}).

-spec delete(scoper_storage:storage_key(), scoper_storage:scope()) -> ok.
delete(Key, ScopeName) ->
    ok = gen_server:call(?MODULE, {delete, Key, ScopeName}).

-spec collect(scoper_storage:storage_key()) ->
    scoper_storage:payload().
collect(Key) ->
    {ok, Meta} = gen_server:call(?MODULE, {collect, Key}),
    Meta.

-spec start_link() ->
    _.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(_) ->
    {ok, #{}}.
init(_) ->
    {ok, #{}}.

-spec handle_call(_, _, _) -> ok | {ok, scoper_storage:payload()}.
handle_call({store, Key, Scope, Meta}, _, State) ->
    NewState = State#{Key => #{Scope => Meta}},
    {reply, ok, NewState};

handle_call({find, Key, Scope}, _, State) ->
    Meta = maps:get(Scope, maps:get(Key, State), undefined),
    {reply, {ok, Meta}, State};

handle_call({delete, Key}, _, State) ->
    {reply, ok, maps:remove(Key, State)};

handle_call({delete, Key, Scope}, _, State) ->
    Meta = maps:get(Scope, maps:get(Key, State)),
    NewMeta = maps:remove(Scope, Meta),
    {reply, ok, State#{Key => NewMeta}};

handle_call({collect, Key}, _, State) ->
    {reply, maps:get(Key, #{}), State}.

-spec handle_cast(_, map()) ->
    {noreply, map()}.
handle_cast(_, State) ->
    {noreply, State}.

