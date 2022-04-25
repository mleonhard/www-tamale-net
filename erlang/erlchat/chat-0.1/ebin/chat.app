{application, chat, [
	{description, "Chat Server"},
	{vsn, "0.1"},
	{modules, [chat, chat_acceptor, chat_acceptor_sup, chat_dispatcher
		, chat_liason, chat_liason_sup, chat_supervisor]},
	{registered, [chat_acceptor_sup, chat_dispatcher, chat_liason_sup
		, chat_supervisor]},
	{applications, [kernel, stdlib]},
	{mod, {chat, []}},
	{env, [{name, "Unnamed Chat Server"}
		, {logfilename, "chat.log"}
		, {port, 5679}]}
	]}.
