%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_acceptor).
-created_by('Michael Leonhard http://tamale.net/').

%% intermodule interface
-export([start/0]).

%% internal exports
-export([init/1]).

port_number() ->
	case application:get_env(port) of
	{ok, Port} ->
		chat_logger:fmsg("acceptor: Port specified in app environment: ~w", [Port]),
		Port;
	undefined ->
		chat_logger:msg("acceptor: Port unspecified in app environment. Using default."),
		5679 % default port
	end.

start() ->
	Port = port_number(),
	DefaultOpts = [binary, {packet, line}, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, DefaultOpts) of
	{ok, LSock} ->
		chat_logger:fmsg("acceptor: listening on port ~B", [Port]),
		Pid = spawn_link(chat_acceptor, init, [LSock]),
		{ok, Pid};
	{error, Reason} -> {error, Reason}
	end.

init(LSock) ->
	chat_logger:fmsg("acceptor: loop initializing, pid=~w, socket=~w"
		, [self(), LSock]),
	loop(LSock).

loop(LSock) ->
	case gen_tcp:accept(LSock) of
	{ok, Socket} ->
		chat_logger:fmsg("acceptor: accepted connection, socket=~w", [Socket]), 
		chat_liason:start(Socket),
		loop(LSock);
	{error, Reason} ->
		chat_logger:fmsg("acceptor: stopping, reason=~w", [Reason]),
		exit(Reason)
	end.
