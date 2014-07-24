
-ifndef(TEST).
-define(TEST, true).
-endif.

%%% config record
-include("recordactor.hrl").

%%% supervisor record
-include("recordsupervisor.hrl").

%-define(DEBUG, true). %% Comment out if you want to see all debug messages.
-define(MFORMAT(Z, X), 
	simdr_actor_contract:get_mode(Z) =/= rt orelse io:format(X)).
-define(MFORMAT(Z, X, Y), 
	{_Year, {H, M, S}} = simdr_timemachine:get_time(),
	ToWrite = lists:concat(["(~wh~wm~ws) ", X]),
	Param = [H, M, S] ++ Y,
	simdr_actor_contract:get_mode(Z) =/= rt orelse io:format(ToWrite, Param)).

-ifdef(DEBUG). %% B_tc51_debug is a 'reserved variable name'
-define(DEBUG_TABLE, test_debug). 
-define(CREATE_DEBUG_TABLE,	
		whereis(tc51debug) =/= undefined orelse register(tc51debug, spawn(fun() -> receive die -> void end end)), 
		B_tc51_debug = ets:info(?DEBUG_TABLE) =/= undefined, 
		B_tc51_debug orelse ets:new(?DEBUG_TABLE, [ordered_set, public, named_table]),
		B_tc51_debug orelse ets:give_away(?DEBUG_TABLE, whereis(tc51debug), void)).
-define(DLOG(X), ets:insert(?DEBUG_TABLE, {erlang:localtime(), ?MODULE, X})).
-define(DLOG(X, Y), ets:insert(?DEBUG_TABLE, {erlang:localtime(), X, Y})).
-define(DFORMAT(X), io:format(X)).
-define(DFORMAT(X, Y), io:format(X, Y)).

-else.
-define(DEBUG_TABLE, test_debug). 
-define(CREATE_DEBUG_TABLE, void).
-define(DLOG(X), void).
-define(DLOG(X, Y), void).
-define(DFORMAT(X), void).
-define(DFORMAT(X, Y), void).
-endif.


