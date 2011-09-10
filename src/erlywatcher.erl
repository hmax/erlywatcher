-module(erlywatcher).
-author('Max Khardin <max.khardin@gmail.com>').

% PLUGIN API
-export([start/0, stop/0]).

start() ->
  application:start(erlywatcher).
  
stop() -> 
  application:stop(erlywatcher),
  application:unload(erlywatcher).

