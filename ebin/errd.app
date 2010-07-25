{application, errd,
 [{description, "Erlang Round Robin Databases"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib]},
  {mod, {errd_app, []}},
  {modules, [cl_calendar, errdb, errd_app, errd_info, errd_sup, errd_command, errd_server, errd_server_sup]},
  {registered, [errd_sup, errd_server_sup]}
 ]}.
