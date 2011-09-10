-module(erlywatcher_server).
-author('Max Khardin <max.khardin@gmail.com>').
-behaviour(gen_server).
-include_lib("include/video_frame.hrl").

%% external API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% private API

-export([logger/0]).

-record(online_streams, {streams, streamloggers, next_stream_id}).

logger() ->
    receive 
        #video_frame{} = Frame -> io:format("logged via ~p: ~p ~n", [self(), Frame#video_frame.content]), logger();
        stop -> io:format("Stopping logger with pid: ~p ~n", self());
        _ -> io:format("Unknown message arrived", []), logger()
    end.
    

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
	{ok, #online_streams{streams = dict:new(), streamloggers = dict:new(), next_stream_id = 1}}.


handle_call(Request, _From, State) ->
	io:format("server handle call ~p ~p ~n", [Request, _From]),
	% {stop, {unknown_call, Request}, State}.
    {ok, State}.

%%We need to store stream_id, because we have to dispatch frames to right loggers
handle_cast({stream_started, _Host, _StreamName, Stream, _Options}, #online_streams{streams = Streams, streamloggers = StreamLoggers, next_stream_id = StreamId }) ->

    LoggerPid = spawn(?MODULE, logger, []),

    ems_media:play(Stream, [{stream_id, StreamId}]),
	NewStreamLoggers = dict:store(StreamId, LoggerPid, StreamLoggers),
    NewStreams = dict:store(Stream, StreamId, Streams),
	io:format("stream started ~p ~p ~n", [Stream, dict:to_list(NewStreams)]),
    {noreply, #online_streams{streams = NewStreams, streamloggers = NewStreamLoggers, next_stream_id = StreamId + 1 }};

handle_cast({stream_stopped, _Host, _StreamName, Stream}, #online_streams{streams = Streams, streamloggers = StreamLoggers, next_stream_id = NextStreamId}) ->

    StreamId = dict:find(Stream, Streams),

	NewStreams = dict:erase(Stream, Streams),
    NewStreamLoggers = dict:erase(StreamId, StreamLoggers),
	io:format("stream stopped ~p ~p ~n", [Stream, dict:to_list(NewStreams)]),
	{noreply, #online_streams{streams = NewStreams, streamloggers = NewStreamLoggers, next_stream_id = NextStreamId}};


handle_cast(Msg, State) ->
	io:format("server handle cast ~p ~n", [Msg]),
	{noreply, State}.

handle_info(#video_frame{} = Frame, Streams) ->
    case dict:find(Frame#video_frame.stream_id, Streams#online_streams.streamloggers) of 
        {ok, LoggerPid} when is_pid(LoggerPid) -> LoggerPid ! Frame;
        error -> io:format("erlywatcher_server: non-existent stream_id <-> logger process mapping")
    end,
    %io:format("erlywatcher_server:frame from stream number ~p~n", [Frame]),
    {noreply, Streams};

handle_info(_Info, State) ->
	%io:format("erlywatcher_server:handle_info ~p~n", [Info]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
