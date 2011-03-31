
-record(rrd_ds_update, {name :: string(),
                        value = unknown :: any()}).

-record(rrd_ds, {name :: string(),
                 type :: 'gauge' | 'counter' | 'derive' | 'absolute',
                 heartbeat :: integer(),
                 min :: integer(),
                 max :: integer(),
                 args = [] :: string()}).

-record(rrd_rra, {cf :: 'average' | 'min' | 'max' | 'last',
                  args :: string()}).

-record(rrd_create, {file :: string(),
                     start_time :: any(), %% TODO
                     step = 300 :: pos_integer(),
                     ds_defs = [] :: [#rrd_ds{}],
                     rra_defs = [] :: [#rrd_rra{}]}).

-record(rrd_update, {file :: string(),
                     time = now :: 'now' | string(),
                     updates :: [#rrd_ds_update{}]}).

-record(rrd, {file :: string(),
              ds = [] :: [#rrd_ds{}],
              rra = [] :: [any()],
              version :: string(),
              step :: integer(),
              last_update :: integer()}).

% vim: set ts=4 sw=4 expandtab:
