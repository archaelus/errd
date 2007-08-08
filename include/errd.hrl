
-record(rrd_create, {file,
                     start_time,
                     step = 300,
                     ds_defs = [],
                     rra_defs = []}).

-record(rrd_update, {file,
                     updates}).

-record(rrd_ds_update, {name,
                        time = now,
                        value = unknown}).

-record(rrd_ds, {name,
                 type,
                 args}).

-record(rrd_rra, {cf,
                  args}).
