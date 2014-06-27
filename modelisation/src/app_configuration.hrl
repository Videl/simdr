
-ifndef(TEST).
-define(TEST, true).
-endif.

% config record
-include("config.hrl").


-define(DEBUG, true).

-ifdef(DEBUG). %% B_tc51_debug is a 'reserved variable name'
-define(DEBUG_TABLE, test_debug). 
-define(CREATE_DEBUG_TABLE,	
		whereis(tc51debug) =/= undefined orelse register(tc51debug, spawn(fun() -> receive die -> void end end)), 
		B_tc51_debug = ets:info(?DEBUG_TABLE) =/= undefined, 
		B_tc51_debug orelse ets:new(?DEBUG_TABLE, [ordered_set, public, named_table]),
		B_tc51_debug orelse ets:give_away(?DEBUG_TABLE, whereis(tc51debug), void)).
-define(DLOG(X), ets:insert(?DEBUG_TABLE, {erlang:localtime(), ?MODULE, X})).
-define(DLOG(X, Y), ets:insert(?DEBUG_TABLE, {erlang:localtime(), X, Y})).

-else.
-define(CREATE_DEBUG_TABLE, void).
-define(DLOG(X), void).
-endif.


