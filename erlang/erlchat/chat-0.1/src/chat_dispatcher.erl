%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_dispatcher).
-behavior(gen_server).
-created_by('Michael Leonhard http://tamale.net/').

%% intermodule exports
-export([start_link/0]).  % called by supervisor
-export([connected/0]).   % called by liasons
-export([disconnected/0]).%  "
-export([string/1]).    %  "

%% gen_server callbacks
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% API used by Liasons
connected() -> gen_server:call(chat_dispatcher, connected).
disconnected() -> gen_server:call(chat_dispatcher, disconnected).
string(String) ->
	FlatString = lists:flatten(String),
	%%%chat_logger:fmsg("pid=~w calls dispatcher:string(\"~s\")", [self(), FlatString]),
	gen_server:cast(chat_dispatcher, {string, FlatString}). % ok

%% used to start the dispatcher gen_server
start_link() ->
	ServerName = {local, chat_dispatcher}, % name gets register()ed
	Module = chat_dispatcher,
	Options = [],
	gen_server:start_link(ServerName, Module, noargs, Options).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
	NewState = State,
	{ok, NewState}.

handle_call(Request, From, State) ->
	RecipientList = State,
	{Pid,_Tag} = From,
	case Request of
	connected ->
		%false = lists:member(Pid, RecipientList),
		chat_logger:fmsg("dispatcher: new recipient, pid=~w", [Pid]),
		RecipientList2 = RecipientList ++ [Pid],
		{reply,ok,RecipientList2};
	disconnected ->
		%true = lists:member(Pid, RecipientList),
		chat_logger:fmsg("dispatcher: deleting recipient, pid=~w", [Pid]),
		RecipientList2 = RecipientList -- [Pid],
		{reply,ok,RecipientList2};
	Other ->
		chat_logger:fmsg("dispatcher: unknown call, from=~w, request=~w", [From, Other]),
		{noreply,State}
	end.

handle_cast(Request, State) ->
	RecipientList = State,
	case Request of
	{string, String} ->
		chat_logger:fmsg("dispatcher: \"~s\"", [String]),
		lists:foreach( fun(Pid) -> chat_liason:send_message(String,Pid) end
			, RecipientList),
		{noreply,State};
	Other ->
		chat_logger:fmsg("dispatcher: unknown cast, request=~w", [Other]),
		{noreply,State}
	end.

handle_info(Info, State) -> 
	chat_logger:fmsg("dispatcher: unknown message, info=~w", [Info]),
	{noreply, State}.

init(_Args) ->
	State = [],
	{ok, State}.

% terminate is called if a handle_* call returns stop
% dispatcher is brutally killed by chat_supervisor on shutdown
terminate(Reason, _State) ->
	chat_logger:fmsg("dispatcher: terminating, reason=~w", [Reason]),
	ok.
