{application, erlywatcher,
[{description, "videostream motion detection plugin"},
 {vsn, "0.1"},
 {modules, [
    erlywatcher,
    erlywatcher_app,
    erlywatcher_event,
    erlywatcher_server,
    erlywatcher_sup
  ]},
 {registered, [erlywatcher]},
 {applications, [kernel, stdlib]},
 {mod, {erlywatcher_app, []}}
]}.

