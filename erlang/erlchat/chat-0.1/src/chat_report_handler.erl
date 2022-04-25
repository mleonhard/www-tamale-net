%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_report_handler).
-behavior(gen_event).
-created_by('Michael Leonhard http://tamale.net/').

%% gen_event callbacks
-export([code_change/3]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([init/1]).
-export([terminate/2]).

%% gen_server callbacks
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(Request, State) ->
	chat_logger:fmsg("report_handler: unknown call, request=~w", [Request]),
	{ok, noreply, State}.

handle_event(Event, State) ->
	case Event of
	{error_report, _Gleader, _Data} -> chat_logger:fmsg("*** ~w ***", [Event]);
	%%%{error, _Gleader, _Data} -> chat_logger:fmsg("*** ~w ***", [Event]);
	{info_report, _Gleader, _Data} -> chat_logger:fmsg("*** ~w ***", [Event]);
	%%%{info_msg, _Gleader, _Data} -> chat_logger:fmsg("*** ~w ***", [Event]);
	%%%{info, _Gleader, _Data} -> chat_logger:fmsg("*** ~w ***", [Event]);
	_Other -> ok
	end,
	{ok, State}.

handle_info(Info, State) ->
	chat_logger:fmsg("report_handler: unknown message, info=~w", [Info]),
	{noreply, State}.

init(_Args) ->
	chat_logger:msg("report_handler: initializing"),
	State = {},
	{ok, State}.

terminate(Reason, _State) ->
	chat_logger:fmsg("report_handler: terminating, reason=~w", [Reason]),
	ok.
