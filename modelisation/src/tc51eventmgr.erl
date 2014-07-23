%% Simulation/emulation event manager 
%% Authors:	Paul Valckenaers (K.U.Leuven Association, Belgium), 
%% 			Matias Novias    (Intec, Argentina)  

-module(tc51eventmgr).
-include("tc51eventrecord.hrl").
-include("debug.hrl").
%% -record (tc51event, {time_ref, notifyPid, load}). 
%% time_ref = {time, reference} where the reference ensures uniqueness 

-import(tc51etstb, [init/0, insert/2, first/1, nextEventTime/1, removefirst/1]).
%% always using fully qualified function calls to enable run-time update

%% user functions
-export([start/0, post/3, postincr/3, returntoken/2, stop/0, gettime/0, gettoken/1]). 

%% for internal use (spawn) only 
-export([loop/5]).

start() ->
	?CREATE_DEBUG_TABLE,
	?DLOG("Initialising event manager."),
	{_,E} = tc51etstb:init(), %%  ets:new(tc51eventCalendar...
	(ets:info(tc51tokenTable) =:= undefined) orelse ets:delete(tc51tokenTable),
	T = ets:new(tc51tokenTable, [public, named_table]), 
	StartToken = make_ref(), ets:insert(T, {StartToken,  self()} ), 
	% Put one token to allow a start signal for discrete event mode. 
	(whereis(tc51eventmgr) =:= undefined) orelse unregister(tc51eventmgr), 
	register(tc51eventmgr, spawn_link (tc51eventmgr, loop, [E, T, 0, 0, {0, now()}])),
	%%%% ets:insert(tc51TestOutput, {"testing 1 2 3"} ),
	{ok, {StartToken, self()}}. 

stop() -> tc51eventmgr ! stop. 
	
post(Time, NotifyPid, Load) -> 
	tc51eventmgr ! {post_event, 
	                #tc51event{time_ref = {Time, make_ref()}, notifyPid = NotifyPid, load = Load }}. 
	               
postincr(Delay, NotifyPid, Load) -> 
	tc51eventmgr ! {post__incr_event, Delay, NotifyPid, Load}. 
	
returntoken(Token, Pid) -> 
	tc51eventmgr ! {return_token, Token, Pid}. 
	
gettime() -> 
	tc51eventmgr ! {get_time, self()},
	receive
		Time -> ok
	end,
	{ok, Time}.

gettoken(OwnerPid) -> 
	tc51eventmgr ! {get_time_token, self(), OwnerPid},
	receive
		{Time,Token} -> ok
	end,
	{ok, {Time, Token}}.
	
loop( E, T, Count, Time, Clock ) -> 
		%% E 		>> Event ets table, 
		%% T 		>> Token ets table, 
		%% Count 	>> number of non-returned tokens, 
		%% Time 	>> simulation time, 
		%% Clock 	>> tuple with now()-value and 
		%% 				corresponding Time-value 
		%%				while/when entering RT mode 

%% First of all handle any outstanding requests for time and/or tokens
%% and handle the return of tokens,  
	receive		
		{ return_token, Tok, _ } ->
			ets:delete(T, Tok),
			?MODULE:loop(E, T, (Count - 1), Time, Clock);		
		{ get_time, ResponsePid } ->
			ResponsePid ! Time,
			?MODULE:loop(E, T , Count, Time, Clock);
		{ get_time_token, ResponsePid, OwnerPid } ->
			Tok = make_ref(), ets:insert_new(T, {Tok, OwnerPid}),
			ResponsePid ! {Time, Tok},
			?MODULE:loop(E, T , (Count + 1), Time, Clock)
		after 0 -> ok	%% but do not wait for messages to arrive.
	end,
	
%% Next, manage simulation calendar and progress of the simulation time base.
	NET = tc51etstb:nextEventTime(E),  %% at the start, NET equals infinity.
	if 
	%% Case 1: Simulation time has caught up with the earliest "post".
	%%  To Do: Notify the "posting process" and remove event from calendar.
		NET =< Time ->
			io:format("CASE1: UP TO DATE (Tokens: ~w)~n", [Count]),
			#tc51event{notifyPid = NotifyPid, load = Load } = tc51etstb:first(E),
			Token = make_ref(), ets:insert_new(T, {Token, NotifyPid}),
			NotifyPid ! {notify, Time, Token, Load}, tc51etstb:removefirst(E),
			?MODULE:loop(E, T , (Count + 1), Time, Clock);
			
	%% Case 2: Calendar is empty.
	%%  To Do: Receive event postings, token returns, 
	%%         handle time and token requests
		NET =:= infinity -> 
			io:format("CASE2: EMPTY (Tokens: ~w)~n", [Count]),
			receive 
				{ post__incr_event, Delay, NotifyPid, Load } -> 
					CurrentTime = adaptTime(Clock),
					tc51etstb:insert(E, #tc51event{time_ref = {CurrentTime + Delay, make_ref()}, notifyPid = NotifyPid, load = Load }),
					?MODULE:loop (E, T, Count, CurrentTime, Clock);
				{ post_event, Event } -> 
					tc51etstb:insert(E, Event),
					?MODULE:loop (E, T, Count, adaptTime(Clock), Clock); 
				{ return_token, Token, _ } ->
					ets:delete(T, Token),
					?MODULE:loop(E, T, (Count - 1), adaptTime(Clock), Clock);					
				{get_time, ResponsePid2} ->
					CurrentTime = adaptTime(Clock),
					ResponsePid2 ! CurrentTime,
					?MODULE:loop(E, T , Count, CurrentTime, Clock);					
				{get_time_token, ResponsePid2, OwnerPid2} ->
					Tok2 = make_ref(), CurrentTime = adaptTime(Clock),
					ets:insert_new(T, {Tok2, OwnerPid2}),
					ResponsePid2 ! {CurrentTime, Tok2},
					?MODULE:loop(E, T , (Count + 1), CurrentTime, Clock);					
				stop -> void	 %% to stop the testing run
			end;
			
	%% Case 3: All tokens have been returned, simulation time is earlier than earliest event 
	%%         in the simuation calendar and the calendar still contains one or more event postings. 
	%%  To Do: Move simulation time forward until the time of the earliest event in the calendar.
	%%         Resynchronise simulation time, which becomes NET, with computer's clock (i.e. now). 
		Count =< 0 -> %% all tokens have been returned, so jump to the next event
			io:format("CASE3: ALL TOKENS HAVE BEEN RETURNED (Tokens: ~w)~n", [Count]),
			?MODULE:loop(E, T, Count, NET, {NET, now()}); %% count ought to be zero, not negative. 
			
	%% Case 4: In real time mode, waiting until the time catches up with the earliest event in
	%%         the simulation calendar. There still is at least one event that needs processing. 
	%%  To Do: Receive event postings, token returns, handle time and token requests
		true ->
			io:format("CASE4: RT MODE (Tokens: ~w, Timeout:~w)~n", [Count, NET-Time]),
			receive 
					stop -> void;	 %% to stop the testing run;	
					{ post__incr_event, Delay, NotifyPid, Load } -> 
						CurrentTime = adaptTime(Clock), 
						tc51etstb:insert(E, #tc51event{time_ref = {CurrentTime + Delay, make_ref()}, notifyPid = NotifyPid, load = Load }), 
						?MODULE:loop (E, T, Count, CurrentTime, Clock);					
					{ post_event, Event } -> 
						tc51etstb:insert(E, Event),
						?MODULE:loop (E, T, Count, adaptTime(Clock), Clock);
					{ return_token, Token, _ } ->
						ets:delete(T, Token),
						?MODULE:loop(E, T, (Count - 1), adaptTime(Clock), Clock);						
					{get_time, ResponsePid3} ->
						CurrentTime = adaptTime(Clock),
						ResponsePid3 ! CurrentTime,
						?MODULE:loop(E, T , Count, CurrentTime, Clock);					
					{get_time_token, ResponsePid3, OwnerPid3} ->
						Tok3 = make_ref(), CurrentTime = adaptTime(Clock),
						ets:insert_new(T, {Tok3, OwnerPid3}),
						ResponsePid3 ! {CurrentTime, Tok3},
						?MODULE:loop(E, T , (Count + 1), CurrentTime, Clock)											
			after erlang:trunc(NET - Time) ->   
				%% @TODO: add a speed-up/slow-down factor
				%% @TODO: Sometimes it bugs because value is weird (NET - Time)
				?MODULE:loop(E, T, Count, adaptTime(Clock), Clock)
				%% timeout will be at least (NET - Time), 
				%% likely to be 1-25 milliseconds longer. 
				%% In other words, notification happens within the blink of an eye. 
		end
			
	end.

%% Compute simulation time while in real time mode, starting from latest entry
%% into real time mode. 	
adaptTime({RTentryTime, ClockAtRTentry}) -> 
	%% RTentryTime is value of the simulation clock at entry in RT mode.
	%% ClockAtRTentry is the return value of now() at entry in RT mode. 
	RTentryTime + (timer:now_diff(now(), ClockAtRTentry) div 1000).
	
	
	