{application, adb_jsoniq,
[{description, "A JSON query language interface to use AngraDB."},
 {vsn, "0.01"},
 {modules, [jsoniq_app, jsoniq_sup, jsoniq_server]},
 {registered, [jsoniq_sup]},
 {applications, [kernel, stdlib, lager]},
 {mod, { jsoniq_app, 
    % {server_addr, server_port}
    [{localhost, 1234}]}}
]}.
