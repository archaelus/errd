%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Library to turn rrd datastructures into rrd command strings.
%% @end
%%%-------------------------------------------------------------------
-module(errd_command).

-include_lib("errd_internal.hrl").

%% API
-export([format/1,
         to_list/1,
         create/3,
         steps/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (RrdRecord) -> RrdCommand::string()
%%   RrdRecord = #rrd_create{} | #rrd_ds{} | #rrd_rra{}
%% @doc Converts the data structure describing the rrd command to
%%  a string that can be executed by rrdtool.
%% @end 
format(#rrd_create{file=File,start_time=undefined,
                   step=Step,ds_defs=DSs,rra_defs=RRAs}) when is_integer(Step) ->
    Dstr = lists:flatten(string:join(lists:map(fun (D) -> format(D) end, DSs), " ")),
    RRAstr = lists:flatten(string:join(lists:map(fun (D) -> format(D) end, RRAs), " ")),
    lists:flatten(io_lib:format("create ~s --step ~p ~s ~s~n", [File, Step, Dstr, RRAstr]));

format(#rrd_ds{name=Name,type=Type,args=Args}) when is_atom(Type) ->
    io_lib:format("DS:~s:~s:~s", [Name, to_list(Type), Args]);

format(#rrd_rra{cf=CF,args=Args}) when is_atom(CF) ->
    io_lib:format("RRA:~s:~s", [to_list(CF), Args]);

format(#rrd_update{file=File, time=Time, updates=Updates}) when is_list(File) ->
    TimeFmt = case Time of
        now ->
            "N";
        _ ->
            Time
    end,
    {Template, Update} = format(Updates),
    lists:flatten(io_lib:format("update ~s -t ~s ~s:~s~n", [File, Template, TimeFmt, Update]));

format([#rrd_ds_update{} | _Tail] = List) ->
    format_updates(List, [], []).


%% @spec (File::string(), DSName::string(), Type) -> #rrd_create{}
%%   Type = guage | counter | derive | absolute
%% @doc Creates the #rrd_create{} command data structure for a data
%% source called DSName, in a file named File. The data source Type
%% determines how rrdtool will treat updates (see
%% http://oss.oetiker.ch/rrdtool/ for more information).
%% @end
create(File,DSName,Type) when Type == gauge; Type == counter;
                              Type == derive; Type == absolute ->
    #rrd_create{file=File,
                ds_defs=[#rrd_ds{name=DSName,type=Type,
                                 args="900:0:U"}],
                rra_defs=[#rrd_rra{cf=average,
                                   args="0.5:1:288"}, % 1 day of 5min averages
                          #rrd_rra{cf=average,
                                   args="0.5:12:168"}, % 7 days of 1hr averages
                          #rrd_rra{cf=average,
                                   args="0.5:288:365"}, % 1 year of daily average
                          #rrd_rra{cf=min,
                                   args="0.5:1:288"}, % 1 day of 5min averages
                          #rrd_rra{cf=min,
                                   args="0.5:12:168"}, % 7 days of 1hr averages
                          #rrd_rra{cf=min,
                                   args="0.5:288:365"}, % 1 year of daily average
                          #rrd_rra{cf=max,
                                   args="0.5:1:288"}, % 1 day of 5min averages
                          #rrd_rra{cf=max,
                                   args="0.5:12:168"}, % 7 days of 1hr averages
                          #rrd_rra{cf=max,
                                   args="0.5:288:365"}, % 1 year of daily average
                          #rrd_rra{cf=hwpredict,
                                   args="2016:0.1:0.0035:288"} % 1 week of forecast
                          ]}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec steps(Unit::atom(), Step::integer()) -> Steps::integer()
%%   Unit = day | week
%% @doc Calculates the number seconds per Step given the number of Steps in a time period.
%%
%% @end
steps(day, 1) ->
    86400;
steps(day, Step) ->
    round(steps(day, 1) / Step);
steps(week, 1) ->
    7 * steps(day, 1);
steps(week, Step) ->
    round(steps(week, 1) / Step).

one_day_test()->
    ?assert(steps(day, 300) == 288).

one_week_test() ->
    ?assert(steps(week, 300) == 2016).

%% @spec to_list(atom()) -> string()
%% @doc Converts the given atom to an upper case string.
%% @end
to_list(S) when is_atom(S) ->
    string:to_upper(atom_to_list(S)).

format_updates([], Template, Update) ->
    {string:join(lists:reverse(Template), ":"), string:join(lists:reverse(Update), ":")};
format_updates([#rrd_ds_update{name=Name, value=Value} | Tail], Template, Update) ->
    format_updates(Tail, [Name | Template], [value_to_list(Value) | Update]).

value_to_list(unknown) ->
    "U";
value_to_list(Value) when is_list(Value) ->
    Value;
value_to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
value_to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
value_to_list(Value) when is_float(Value) ->
    float_to_list(Value);
value_to_list(Value) when is_binary(Value) ->
    binary_to_list(Value).

join_test() ->
    ?assert(string:join(["This", "is", "a", "test."], " ") == "This is a test."),
    ?assert(string:join(["test."], " ") == "test.").

% vim: set ts=4 sw=4 expandtab:
