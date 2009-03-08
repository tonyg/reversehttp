{application, reversehttp,
 [{description, "reversehttp"},
  {vsn, "0.01"},
  {modules, [
    reversehttp,
    reversehttp_app,
    reversehttp_sup,
    reversehttp_web,
    reversehttp_deps
  ]},
  {registered, []},
  {mod, {reversehttp_app, []}},
  {env, [{exception_hosts, [{"localhost", ["/reversehttp"]},
                            {"localhost:8000", ["/reversehttp"]},
                            {"localhost.lshift.net", ["/reversehttp"]},
                            {"localhost.lshift.net:8000", ["/reversehttp"]}]},
         {port, 8000}]},
  {applications, [kernel, stdlib, crypto]}]}.
