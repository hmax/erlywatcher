-module(erlywatcher_app).
-author('Max Khardin <max.khardin@gmail.com>').
-behaviour(application).
-version(0.1).

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  io:format("start erlywatcher_app~n"),
  erlywatcher_sup:start_link().
  
stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.
