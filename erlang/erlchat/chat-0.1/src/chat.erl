%%% Copyright 2005 Michael Leonhard
%%% All rights reserved.
-module(chat).
-behaviour(application).
-revision('Revision: 0.1 ').
-created('Date: 2005/06/09 17:35:00 ').
-created_by('Michael Leonhard http://tamale.net/').

% application exports
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	io:fwrite("Chat Server~n"
		"Copyright 2005 Michael Leonhard~n"
		"All rights reserved.~n"),
	chat_supervisor:start_link().

stop(_State) ->
	ok.
