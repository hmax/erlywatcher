-module(erlywatcher_event).
-author('Max Khardin <max.khardin@gmail.com>').
-behaviour(gen_event).
-include_lib("include/erlyvideo.hrl").

%% External API
-export([start_link/0, listen/0]).

%% gen_event callbacks
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	Pid = spawn_link(?MODULE, listen, []),
	{ok, Pid}.
  
listen() ->
	ems_event:add_sup_handler(?MODULE, []),
	receive
	Msg ->
		io:format("erlywatcher_event:listen ~p~n", [Msg])
	end.
  
init([]) ->
  {ok, state}.

handle_event(#erlyvideo_event{event = stream_started, host = Host,  stream_name = StreamName, stream = Stream, options = Options}, State) ->
    gen_server:cast(erlywatcher_server, {stream_started, Host, StreamName, Stream, Options}),
 	{ok, State};

handle_event(#erlyvideo_event{event = stream_stopped, host = Host, stream_name = StreamName, stream = Stream}, State) ->
    gen_server:cast(erlywatcher_server, {stream_stopped, Host, StreamName, Stream}),
	{ok, State};

handle_event(Msg, State) ->
	io:format("handle_event ~p~n", [Msg]),
	{ok, State}.

handle_call(Request, State) ->
	{ok, Request, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
