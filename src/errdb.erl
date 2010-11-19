%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ERRD DB API
%% @end
%%%-------------------------------------------------------------------
-module(errdb).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([create/1,
         create/2,
         read_config/1,
         write_config/2,
         config_name/1,
         rrd_name/1,
         read/1,
         delete/1]).

%% Format:
%%   errd_db_name/
%%     data.rrd -- rrdtool file
%%     config.erl -- config parameters in consult format.

%%====================================================================
%% API
%%====================================================================

create(PathName) ->
    create(PathName, []).

create(PathName, Options) when is_list(Options) ->
    ok = file:make_dir(PathName),
    write_config(PathName, Options).   

write_config(PathName, Terms) when is_list(Terms) ->
    file:write_file(config_name(PathName),
                    iolist_to_binary([io_lib:format("~p.~n", [Term])
                                      ||Term<-Terms])).

read_config(PathName) ->
    file:consult(config_name(PathName)).

config_name(PathName) ->
    filename:join([PathName, "config.erl"]).

rrd_name(PathName) ->
    filename:join([PathName, "data.rrd"]).

read(PathName) ->
    true = filelib:is_dir(PathName),
    {ok, Config} = read_config(PathName),
    {ok, {rrd_name(PathName),
          Config}}.

delete(PathName) ->
    file:delete(config_name(PathName)),
    file:delete(rrd_name(PathName)),
    file:del_dir(PathName).

%%====================================================================
%% Internal functions
%%====================================================================

errdb_test() ->
    ?assertMatch(false, filelib:is_dir("../priv/test")),
    ?assertMatch(ok, create("../priv/test", [test])),
    ?assertMatch({ok, {"../priv/test/data.rrd",
                       [test]}},
                 read("../priv/test")),
    ?assertMatch(ok, delete("../priv/test")).

% vim: set ts=4 sw=4 expandtab:
