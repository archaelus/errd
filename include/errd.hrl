
-record(rrd_create, {file,
                     start_time,
                     step = 300,
                     ds_defs = [],
                     rra_defs = []}).

-record(rrd_update, {file,
                     time = now,
                     updates}).

-record(rrd_ds_update, {name,
                        value = unknown}).

-record(rrd_ds, {name,
                 type,
                 heartbeat,
                 min,
                 max,
                 args = []}).

-record(rrd_rra, {cf,
                  args}).

-record(rrd, {file,
              ds = [],
              rra = [],
              version,
              step,
              last_update}).

% vim: set ts=4 sw=4 expandtab:
