%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat_liason_sup).
-behavior(supervisor).
-created_by('Michael Leonhard http://tamale.net/').

%% user interface
-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).

start_link() ->
	chat_logger:msg("liason_sup:start_link()"),
	supervisor:start_link({local, chat_liason_sup}, chat_liason_sup, []).

init(Args) ->
	chat_logger:fmsg("liason_sup:init(~w)", [Args]),
	LiasonSpec = {chat_liason, {chat_liason, start_link, []}
		, temporary, brutal_kill, worker, [chat_liason]},
	StartSpecs = {{simple_one_for_one, 0, 1}, [LiasonSpec]},
	chat_logger:msg("liason_sup:init() returning"),
	{ok, StartSpecs}.
