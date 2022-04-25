%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_acceptor_sup).
-behavior(supervisor_bridge).
-created_by('Michael Leonhard http://tamale.net/').

%% intermodule exports
-export([start_link/0]).

%% supervisor_bridge callbacks
-export([terminate/2]).
-export([init/1]).

start_link() -> supervisor_bridge:start_link({local, chat_acceptor_sup}
	, chat_acceptor_sup, []).

init(_Args) ->
	chat_logger:fmsg("acceptor_sup: initializing, pid=~w", [self()]),
	{ok, AcceptorPid} = chat_acceptor:start(),
	{ok, AcceptorPid, AcceptorPid}.

terminate(Reason, State) ->
	AcceptorPid = State,
	chat_logger:fmsg("acceptor_sup: terminating, reason=~w", [Reason]),
	exit(AcceptorPid, Reason).

