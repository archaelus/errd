%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc RRDtool Info Parser.
%% @end
%%%-------------------------------------------------------------------
-module(errd_info).

-include_lib("errd_internal.hrl").

%% API
-export([parse/1]).

%%====================================================================
%% API
%%====================================================================

parse(Lines) ->
    parse_filename(#rrd{}, [string:strip(Line,right, $\n) || Line<-Lines]).

parse_filename(Rrd, [Line | Lines]) ->
    parse_version(Rrd#rrd{file=re_find(Line, "filename = \"(.*)\"")},
                  Lines).

parse_version(Rrd, [Line | Lines]) ->
    parse_step(Rrd#rrd{version=re_find(Line, "rrd_version = \"(.*)\"")},
               Lines).

parse_step(Rrd, [Line | Lines]) ->
    Step = re_find(Line, "step = (.*)"),
    parse_last_update(Rrd#rrd{step=list_to_integer(Step)}, Lines).

parse_last_update(Rrd, [Line | Lines]) ->
    LU = re_find(Line, "last_update = (.*)"),
    parse_other(Rrd#rrd{last_update=list_to_integer(LU)}, Lines).

parse_other(Rrd, []) ->
    Rrd;
parse_other(Rrd, ["ds"++_|_] = Lines) ->
    parse_ds(Rrd, Lines);
parse_other(Rrd, ["rra"++_|_] = Lines) ->
    parse_rra(Rrd, Lines).

parse_ds(Rrd, Lines) ->
    parse_ds_name(Rrd, #rrd_ds{}, Lines).

parse_rra(Rrd, _) ->
    Rrd. %% Not implemented.

parse_ds_name(Rrd, Ds, [Line|Lines]) ->
    {match, [Name, Type]} = re:run(Line, "ds\\[(.*)\\].type = \"(.*)\"", [{capture, all_but_first, list}]),
    parse_ds_min_heartbeat(Rrd, Ds#rrd_ds{name=Name,type=Type},
                           Lines).

parse_ds_min_heartbeat(Rrd, Ds, [Line|Lines]) ->
    MinH = re_find(Line,"minimal_heartbeat = (.*)$"),
    parse_ds_min(Rrd, Ds#rrd_ds{heartbeat=list_to_integer(MinH)}, Lines).

parse_ds_min(Rrd, Ds, [Line|Lines]) ->
    Min = re_find_float(Line,"min = (.*)$"),
    parse_ds_max(Rrd, Ds#rrd_ds{min=Min}, Lines).

parse_ds_max(Rrd, Ds, [Line|Lines]) ->
    Max = re_find_float(Line,"max = (.*)$"),
    parse_ds_last(Rrd, Ds#rrd_ds{max=Max}, Lines).

parse_ds_last(Rrd, Ds, [Line|Lines]) ->
    Last = case re_find(Line,"last_ds = \"(.*)\"$") of
               "UNKN" -> unknown;
               
               Val -> Val
           end,
    parse_ds_value(Rrd, Ds#rrd_ds{args=[{last_ds,Last}|Ds#rrd_ds.args]}, Lines).

parse_ds_value(Rrd, Ds, [Line|Lines]) ->
    Val = re_find_float(Line,"value = (.*)$"),
    parse_ds_unknown_sec(Rrd, Ds#rrd_ds{args=[{value,Val}|Ds#rrd_ds.args]}, Lines).

parse_ds_unknown_sec(Rrd, Ds, [Line|Lines]) ->
    Sec = re_find(Line,"unknown_sec = (.*)$"),
    parse_other(Rrd#rrd{ds=Rrd#rrd.ds ++
                        [Ds#rrd_ds{args=[{unknown_sec,list_to_integer(Sec)}|Ds#rrd_ds.args]}]},
                Lines).
    

re_find(String, Re) ->
    {match, [Match]} = re:run(String, Re, [{capture, all_but_first, list}]),
    Match.

re_find_float(String, Re) ->
    Match = re_find(String, Re),
    case Match of
        "NaN" -> undefined;
        Float -> list_to_float(Float)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

% vim: set ts=4 sw=4 expandtab:
