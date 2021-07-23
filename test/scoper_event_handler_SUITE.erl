-module(scoper_event_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([handler_logs_events/1]).

%% woody_server callbacks
-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%% logger callbacks
-export([log/2]).
-export([adding_handler/1]).
-export([removing_handler/1]).

%%

-type test_name() :: atom().
-type config() :: [{atom(), _}].

-spec all() -> [test_name()].
all() ->
    [
        handler_logs_events
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    {ok, Apps1} = application:ensure_all_started(scoper),
    {ok, Apps2} = application:ensure_all_started(woody),
    ok = logger:set_primary_config(level, debug),
    [{apps, Apps1 ++ Apps2} | C].

-spec end_per_suite(config()) -> any().
end_per_suite(C) ->
    ok = logger:set_primary_config(level, notice),
    [application:stop(App) || App <- ?config(apps, C)].

%%

-include("scp_test_thrift.hrl").

-define(LOGEVENT(Level, Msg, Meta),
    {log, #{
        level := Level,
        msg := {Msg, _},
        meta := #{trace_id := <<_/binary>>, parent_id := <<_/binary>>, span_id := <<_/binary>>},
        meta := Meta
    }}
).

-spec handler_logs_events(config()) -> _.
handler_logs_events(_C) ->
    %% NOTE
    %% Test that scoper properly handles woody events.
    %% The purpose is to catch regressions early against woody master.
    ServerId = ?FUNCTION_NAME,
    HandlerId = ?FUNCTION_NAME,
    {ok, Pid, Url} = start_woody_server(ServerId),
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        level => debug,
        config => #{forward => self()},
        %% NOTE
        %% Filter out everything spit out by OTP facilities: supervisor and progress reports, etc.
        filters => [{domain, {fun logger_filters:domain/2, {stop, sub, [otp]}}}]
    }),
    {ok, #'GeneratedID'{}} = call_woody_service('GenerateID', {{snowflake, #'SnowflakeSchema'{}}}, Url),
    ok = stop_woody_server(Pid),
    ok = logger:remove_handler(HandlerId),
    ?assertMatch(
        [
            {relay, HandlerId, started},
            {relay, HandlerId,
                ?LOGEVENT(info, "[client] calling" ++ _, #{
                    'rpc.client' := #{
                        event := 'call service',
                        function := 'GenerateID',
                        service := 'Generator'
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(debug, "[client] sending request" ++ _, #{
                    'rpc.client' := #{
                        event := 'client send',
                        type := call,
                        url := Url,
                        function := 'GenerateID',
                        service := 'Generator'
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(debug, "[server] request" ++ _, #{
                    'rpc.server' := #{
                        event := 'server receive',
                        url := Url
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(info, "[server] handling" ++ _, #{
                    'rpc.server' := #{
                        event := 'invoke service handler',
                        url := Url,
                        type := call,
                        function := 'GenerateID',
                        service := 'Generator'
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(info, "[server] handling result" ++ _, #{
                    'rpc.server' := #{
                        event := 'service handler result',
                        url := Url,
                        type := call,
                        function := 'GenerateID',
                        service := 'Generator'
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(debug, "[server] response sent" ++ _, #{
                    'rpc.server' := #{
                        event := 'server send',
                        url := Url,
                        type := call,
                        function := 'GenerateID',
                        service := 'Generator',
                        execution_duration_ms := _
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(debug, "[client] received response" ++ _, #{
                    'rpc.client' := #{
                        event := 'client receive',
                        url := Url,
                        type := call,
                        function := 'GenerateID',
                        service := 'Generator',
                        execution_duration_ms := _
                    }
                })},
            {relay, HandlerId,
                ?LOGEVENT(info, "[client] request handled" ++ _, #{
                    'rpc.client' := #{
                        event := 'service result',
                        url := Url,
                        type := call,
                        function := 'GenerateID',
                        service := 'Generator',
                        execution_duration_ms := _
                    }
                })},
            {relay, HandlerId, terminated}
        ],
        flush_msg_queue(1000)
    ).

flush_msg_queue(Timeout) ->
    receive
        M -> [M | flush_msg_queue(Timeout)]
    after Timeout -> []
    end.

%%

-define(SERVICE, {scp_test_thrift, 'Generator'}).

start_woody_server(Id) ->
    Path = "/gen",
    ServerOpts = #{
        handlers => [{Path, {?SERVICE, ?MODULE}}],
        event_handler => scoper_woody_event_handler,
        ip => {127, 0, 0, 1},
        port => 0
    },
    {ok, SupPid} = genlib_adhoc_supervisor:start_link(#{}, [woody_server:child_spec(Id, ServerOpts)]),
    {IP, Port} = woody_server:get_addr(Id, ServerOpts),
    Url = genlib:format("http://~s:~p~s", [inet:ntoa(IP), Port, Path]),
    {ok, SupPid, Url}.

stop_woody_server(SupPid) ->
    true = erlang:unlink(SupPid),
    proc_lib:stop(SupPid, shutdown, 1000).

call_woody_service(Function, Args, Url) ->
    Request = {?SERVICE, Function, Args},
    woody_client:call(Request, #{url => Url, event_handler => scoper_woody_event_handler}).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('GenerateID', {Schema}, _WoodyCtx, _Opts) ->
    {ID, IntegerID} = generate_id(Schema),
    {ok, #'GeneratedID'{id = ID, integer_id = IntegerID}}.

generate_id({snowflake, _}) ->
    <<ID:64>> = snowflake:new(),
    {genlib_format:format_int_base(ID, 62), ID};
generate_id({sequence, _}) ->
    ID = erlang:unique_integer([positive, monotonic]),
    {integer_to_binary(ID), ID}.

%% NOTE
%% Logger handler which forwards every log message to the configured process.

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(LogEvent, #{id := Name, module := ?MODULE, config := Config}) ->
    Pid = maps:get(forward, Config),
    Pid ! {relay, Name, {log, LogEvent}},
    ok.

-spec adding_handler(logger:handler_config()) -> {ok, logger:handler_config()}.
adding_handler(HConfig = #{id := Name, module := ?MODULE, config := Config}) ->
    Pid = maps:get(forward, Config),
    Pid ! {relay, Name, started},
    {ok, HConfig}.

-spec removing_handler(logger:handler_config()) -> _.
removing_handler(#{id := Name, module := ?MODULE, config := Config}) ->
    Pid = maps:get(forward, Config),
    Pid ! {relay, Name, terminated}.
