
-include_lib("eunit/include/eunit.hrl").
-include_lib("errd.hrl").

-define(INFO(Format, Args),
        error_logger:info_report([{where, lists:flatten(io_lib:format("(~p:~p ~p)", [?MODULE, ?LINE, self()]))},
                                  lists:flatten(io_lib:format(Format, Args))])).
-define(WARN(Format, Args),
        error_logger:warning_report([{where, lists:flatten(io_lib:format("(~p:~p ~p)", [?MODULE, ?LINE, self()]))},
                                     lists:flatten(io_lib:format(Format, Args))])).
-define(ERR(Format, Args),
        error_logger:error_report([{where, lists:flatten(io_lib:format("(~p:~p ~p)", [?MODULE, ?LINE, self()]))},
                                   lists:flatten(io_lib:format(Format, Args))])).

% vim: set ts=4 sw=4 expandtab:
