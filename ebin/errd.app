{application, errd,
 [{description, "Erlang Round Robin Databases"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib]},
  {mod, {errd_app, []}},
  {modules, [errd_app, errd_sup, errd_command, errd_server]},
  {registered, [errd_sup]}
 ]}.
