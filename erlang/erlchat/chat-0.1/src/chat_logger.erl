%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_logger).
-behavior(gen_server).
-created_by('Michael Leonhard http://tamale.net/').

%% intermodule exports
-export([start_link/0]).  % called by supervisor
-export([msg/1]).         % called by chat modules
-export([fmsg/2]).
-export([print_msg/1]).
-export([print_fmsg/2]).

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% API
print_msg(String) ->
	io:fwrite("~s ~s~n", [string_timestamp(), String]).

msg(String) ->
	FlatString = lists:flatten([string_timestamp(), " ", String]),
	gen_server:cast(chat_logger, {string, FlatString}). % ok

print_fmsg(Format, Args) ->
	io:fwrite(string_timestamp() ++ " " ++ Format ++ "~n", Args).

fmsg(Format, Args) ->
	String = io_lib:fwrite(string_timestamp() ++ " " ++ Format, Args),
	gen_server:cast(chat_logger, {string, String}). %ok

%% used to start the gen_server
start_link() ->
	ServerName = {local, chat_logger}, % name gets register()ed
	Module = chat_logger,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, From, State) ->
	{_LogFileDevice} = State,
	fmsg("logger: unknown call ~w from ~w", [Request, From]),
	{noreply,State}.

add_newline(String) ->
	case lists:last(String) of
	$\n -> String;
	_Other -> String ++ "\n"
	end.

handle_cast(Request, State) ->
	{LogFileDevice} = State,
	case Request of
	{string, String} ->
		% TODO: escape nonprintables
		ok = file:write(LogFileDevice, add_newline(String)),
		io:fwrite("~s~n", [String]),
		% TODO: send message to OpDispatcher
		{noreply,State};
	Other ->
		fmsg("logger: unknown cast ~w", [Other]),
		{noreply,State}
	end.

handle_info(Info, State) -> 
	fmsg("logger: unknown message ~w", [Info]),
	{noreply, State}.

string_timestamp() ->
	{MegaSecs,Secs,_Microsecs} = now(),
	integer_to_list(MegaSecs) ++ integer_to_list(Secs).

%% retrieve filename from application environment, or use default
logfilename_env() ->
	case application:get_env(logfilename) of
	{ok, LogFileName} -> LogFileName;
	undefined ->
		print_msg("logger: LogFileName unspecified in app environment."
			"Using default."),
		"chat.log" % default
	end.

logfilename() ->
	[code:priv_dir(chat),"/", logfilename_env()].

remove_handler(Module) ->
	case error_logger:delete_report_handler(Module) of
	{error, _What} -> ok;
	_Other -> remove_handler(Module)
	end.

init(_Args) ->
	process_flag(trap_exit, true),
	LogfileName = logfilename(),
	{ok, LogFileDevice} = file:open(LogfileName, [append, raw]),
	OpenMessage = io_lib:fwrite("~s logger: started, pid=~w, file=~s~n"
		, [string_timestamp(), self(), LogfileName]),
	ok = file:write(LogFileDevice, OpenMessage), % write to logfile
	io:fwrite("~s", [OpenMessage]), % print on screen
	remove_handler(chat_report_handler),
	ok = error_logger:add_report_handler(chat_report_handler),
	State = {LogFileDevice},
	{ok, State}.

terminate(Reason, State) ->
	{LogFileDevice} = State,
	remove_handler(chat_report_handler),
	CloseMessage = io_lib:fwrite("~s logger: terminating, reason=~w~n" 
		, [string_timestamp(), Reason]),
	ok = file:write(LogFileDevice, [CloseMessage, "\n"]), % write to logfile
	ok = file:close(LogFileDevice),
	io:fwrite("~s", [CloseMessage]), % print on screen
	ok.
