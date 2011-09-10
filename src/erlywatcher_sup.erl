-module(erlywatcher_sup).
-author('Max Khardin <max.khardin@gmail.com>').
-version(1.0).
-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Supervisors = [
	{   erlywatcher_event,                      % Id       = internal id
		{erlywatcher_event, start_link, []},    % StartFun = {M, F, A}
		permanent,                               % Restart  = permanent | transient | temporary
		2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
		worker,                                  % Type     = worker | supervisor
		[erlywatcher_event]                     % Modules  = [Module] | dynamic
	},
	{   erlywatcher_server,                     % Id       = internal id
		{erlywatcher_server, start_link, []},   % StartFun = {M, F, A}
		permanent,                               % Restart  = permanent | transient | temporary
		2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
		worker,                                  % Type     = worker | supervisor
		[erlywatcher_server]                    % Modules  = [Module] | dynamic
	}
  ],
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
