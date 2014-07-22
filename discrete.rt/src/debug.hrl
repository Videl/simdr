%% Comment out line below to disable debug mode
%% -define(DEBUG,void).
-import(tc51debug, [wait/0]).

-ifdef(DEBUG). %% B_tc51_debug is a 'reserved variable name'
-define(DEBUG_TABLE, test_debug). 
-define(CREATE_DEBUG_TABLE,	
		whereis(tc51debug) =/= undefined orelse register(tc51debug, spawn(tc51debug,wait, [])), 
		B_tc51_debug = ets:info(?DEBUG_TABLE) =/= undefined, 
		B_tc51_debug orelse ets:new(?DEBUG_TABLE, [ordered_set, public, named_table]),
		B_tc51_debug orelse ets:give_away(?DEBUG_TABLE, whereis(tc51debug), void)).
-define(DLOG(X), ets:insert(?DEBUG_TABLE, {erlang:now(), ?MODULE, X})).

-else.
-define(CREATE_DEBUG_TABLE, void).
-define(DLOG(X), void).
-endif.


