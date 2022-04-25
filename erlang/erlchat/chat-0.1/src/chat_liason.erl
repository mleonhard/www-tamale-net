%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_liason).
-behavior(gen_server).
-created_by('Michael Leonhard http://tamale.net/').

%% intermodule exports
-export([send_message/2]).% called by dispatcher to deliver a message
-export([start/1]).       % called by acceptor
-export([start_link/1]).  % called by supervisor

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% internal exports
-export([reader_start/3]).

-include_lib("kernel/include/inet.hrl"). % needed for hostent structure
get_client_name(Socket) ->
	case inet:peername(Socket) of
	{error, _Reason} -> "?.?.?.?:?";
	{ok, {{A,B,C,D}, Port}} ->
		case inet:gethostbyaddr({A,B,C,D}) of
		{error, _Reason} ->
			io_lib:fwrite("~B.~B.~B.~B:~B", [A,B,C,D, Port]);
		{ok, Hostent} ->
			io_lib:fwrite("~s:~B", [Hostent#hostent.h_name, Port])
		end
	end.

%% returns a string representation of the number of seconds since Epoch
string_timestamp() ->
	{MegaSecs,Secs,_Microsecs} = now(),
	integer_to_list(MegaSecs) ++ integer_to_list(Secs).

%% makes a notification of joining, leaving, etc.
format_notification(Name, String) ->
	string_timestamp() ++ " " ++ Name ++" "
		++ lists:filter(fun(X) -> (X /= $\r)and(X /= $\n)end, String).

%% formats a received message that the user typed
format_message(Name, B) ->
	string_timestamp() ++ " <" ++ Name ++ "> "
		++ lists:filter(fun(X)->(X /= $\r)and(X /= $\n)end, binary_to_list(B)).

%% saves a log entry: <client address> <message>
log_message(Name, Format, Args) ->
	chat_logger:fmsg(Name ++ " " ++ Format, Args).

log_message(Name, String) ->
	chat_logger:msg(Name ++ " " ++ String).

reader_start(Socket, Name, LiasonPid) ->
	reader_loop(Socket, Name, LiasonPid).

reader_loop(Socket, Name, LiasonPid) ->
	case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
		chat_dispatcher:string(format_message(Name, B)),
		reader_loop(Socket, Name, LiasonPid);
	{error, closed} ->
		unlink(LiasonPid),
		gen_server:cast(LiasonPid, socket_closed)
	end.

deliver_message(Name, Socket, String) ->
	log_message(Name, "==> \"~s\\r\\n\"", [String]),
	gen_tcp:send(Socket, String ++ "\r\n").

server_name() ->
	case application:get_env(name) of
	{ok, Name} -> Name;
	undefined -> 
		chat_logger:msg("liason: server Name unspecified in app environment. Using default."),
		"Unnamed Chat Server" % default name
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intermodule exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% called by dispatcher
send_message(String, Pid) ->
	%%%chat_logger:fmsg("pid=~w called liason:send_message(\"~s\", ~w)", [self(), String, Pid]),
	gen_server:cast(Pid, {message, String}). % ok

% called by acceptor
start(Socket) ->
	ExtraArgs = [{socket, Socket}],
	supervisor:start_child(chat_liason_sup, ExtraArgs). % simple_one_for_one

%% called by supervisor to start the liason gen_server
start_link(Args) ->
	Options = [],
	gen_server:start_link(chat_liason, Args, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, From, State) ->
	{_Socket, Name} = State,
	case Request of
	Other ->
		log_message(Name, "unknown call, pid=~w, request=~w", [From, Other]),
		{noreply, State}
	end.

handle_cast(Request, State) ->
	{Socket, Name} = State,
	case Request of
	{message, String} ->
		deliver_message(Name, Socket, String),
		{noreply,State};
	socket_closed ->
		log_message(Name, "socket closed"),
		{stop,normal,State};
	Other ->
		log_message(Name, "unknown cast, request=~w", [Other]),
		{noreply,State}
	end.

handle_info(Info, State) ->
	{_Socket, Name} = State,
	log_message(Name, "got unknown message, info=~w", [Info]),
	{noreply, State}.

init(Args) ->
	{socket, Socket} = Args,
	%inet:setopts(Socket, [binary, {packet, line}, {active, false}]),
	Name = get_client_name(Socket),
	log_message(Name, "connected, pid=~w", [self()]),
	chat_dispatcher:connected(),
	deliver_message(Name, Socket, "Welcome to "++server_name()), % greet client
	chat_dispatcher:string(format_notification(Name,"connected.")), % inform
	spawn_link(chat_liason, reader_start, [Socket, Name, self()]),
	State = {Socket, Name},
	{ok, State}.

%% called when handle_cast returns stop.
%% when a shutdown occurs, all liasons are brutally killed by chat_liason_sup
terminate(Reason, State) ->
	{Socket, Name} = State,
	inet:close(Socket),
	log_message(Name, "terminating, pid=~w, reason=~w", [self(), Reason]),
	chat_dispatcher:string(format_notification(Name,"disconnected")), % inform
	chat_dispatcher:disconnected(),
	ok.
