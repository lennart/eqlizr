{application, eqlizr,
 [{description, "eqlizr"},
  {vsn, "0.01"},
  {modules, [
    eqlizr,
    eqlizr_app,
    eqlizr_sup,
    eqlizr_web,
    eqlizr_deps
  ]},
  {registered, []},
  {mod, {eqlizr_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
