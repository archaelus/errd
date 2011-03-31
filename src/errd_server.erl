%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Manages an rrdtool process.
%% @end
%%%-------------------------------------------------------------------
-module(errd_server).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("errd_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0
         ,start/0
         ,stop/1
         ,cd/2
         ,raw/2
         ,info/2
         ,format_raw/3
         ,command/2
        ]).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------
-record(state, {rrd_port}).

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(RRD_COMMAND_TIMEOUT, 5 * 1000).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start() -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop(Pid::pid()) -> ok
%% @end
%%--------------------------------------------------------------------
stop(Server) ->
    gen_server:call(Server, stop).

cd(Server, Directory) ->
    gen_server:call(Server, {cd, Directory}).

info(Server, Filename) ->
    case gen_server:call(Server, {info, Filename}) of
        {ok, Data} ->
            errd_info:parse(Data);
        Other ->
            Other
    end.

raw(Server, Str) ->
    gen_server:call(Server, {raw, Str}).

format_raw(Server, Fmt, Args) ->
    gen_server:call(Server, {raw, Fmt, Args}).

command(Server, Cmd) ->
    raw(Server, errd_command:format(Cmd)).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    case open_port({spawn, "rrdtool -"},
                   [use_stdio, exit_status, {line, 16000}]) of
        Port when is_port(Port) ->
            {ok, #state{rrd_port=Port}};
        Else ->
            {stop, {no_rrdtool, Else}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%handle_call({create, Spec = #rrd_create{}}, _From, State=#state{rrd_port=Port}) ->
%    Fmt = ok, Args = ok,
%    {reply, rrd_command(Port, Fmt, Args), State};
handle_call({raw, Fmt, Args}, _From, State=#state{rrd_port=Port}) ->
    {reply, rrd_command(Port, Fmt, Args), State};
handle_call({raw, Cmd}, _From, State=#state{rrd_port=Port}) ->
    {reply, rrd_command(Port, Cmd), State};
handle_call({cd, Directory}, _From, State=#state{rrd_port=Port}) ->
    Result = rrd_command(Port, "cd ~s~n", [Directory]),
    {reply, Result, State};
handle_call({info, Filename}, _From, State=#state{rrd_port=Port}) ->
    Result = rrd_command(Port, "info ~s~n", [Filename]),
    {reply, Result, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    ?WARN("Unexpected call: ~p", [Request]),
    {reply, {error, unknown_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARN("Unexpected info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, #state{rrd_port=P}) when is_port(P) ->
    port_close(P),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================

rrd_command(Port, Fmt, Args) ->
    rrd_command(Port, lists:flatten(io_lib:format(Fmt, Args))).

rrd_command(Port, Command) ->
    true = port_command(Port, Command),
    wait_rrd_command(Port, Command, [], []).

wait_rrd_command(Port, Cmd, Lines, SoFar) ->
    receive
        {P, {data, {eol, "OK " ++ PerfData}}} when P == Port, SoFar == [] ->
            parse_rrd_response(Cmd, {ok, PerfData}, lists:reverse(Lines));
        {P, {data, {eol, "ERROR: " ++ Error}}} when P == Port, SoFar == [] ->
            parse_rrd_response(Cmd, {error, Error}, lists:reverse(Lines));
        {P, {data, {eol, Data}}} when P == Port ->
            wait_rrd_command(Port, Cmd, [Data|Lines], SoFar);
        {P, {data, {noeol, Data}}} when P == Port ->
            wait_rrd_command(Port, Cmd, Lines, SoFar ++ Data)
    after ?RRD_COMMAND_TIMEOUT ->
            ?WARN("rrdtool didn't respond to [~s] within ~pms.~nPartial data: ~p~n.",
                  [Cmd, ?RRD_COMMAND_TIMEOUT, SoFar]),
            {error, rrd_timeout}
    end.

parse_rrd_response(_Cmd, {ok, _PerfData}, Lines) ->
    %?INFO("Command [~s] completed: ~s~n~s~n", [Cmd, PerfData, Lines]),
    {ok, Lines};
parse_rrd_response(Cmd, {error, Error}, Lines) ->
    ?WARN("Command [~s] failed: ~s~n~s~n", [Cmd, Error, Lines]),
    {error, Error}.

rrd_cmd_test() ->
    {ok, Pid} = ?MODULE:start_link(),
    ?assertMatch({ok, []}, gen_server:call(Pid, {cd, "/"})),
    ?MODULE:stop(Pid).

% vim: set ts=4 sw=4 expandtab:
