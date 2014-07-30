%%% @doc Common contract for all actors.
%%% 
%%% This module provides you with a lot of functions to maintain, use, control,
%%% edit actors.
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
-module(simdr_actor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create/11, 
		 create/8, 
		 create/6,
		 create/3,
		 create/2, 
		 get_module/1,
		 add_data/2, 
		 add_in/2,
		 add_out/2,
		 add_option/3,
		 set_options/3,
		 add_to_list_data/4,
		 get_data/1,
		 get_pid/1,
		 get_name/1,
		 get_in/1,
		 get_out/1,
		 get_in_out/1,
		 get_capacity/1, 
		 get_opt/1, 
		 get_option/2,	
		 get_work_time/1, 
		 get_state/1,
		 get_mode/1,
		 get_list_data/1, 
		 set_mode/2,
		 set_pid/2,
		 set_work_time/2,
		 set_name/2,
		 set_state/2,
		 set_in/2,
		 set_out/2,
		 set_in_out/2,
		 set_capacity/2,
		 set_option/3, 
		 work/1,
		 work/2,
		 list_size/1,
		 first_key_awaiting/2,
		 different_sender/1,
		 random_id/0,
		 first/1,
		 delete_option/2,
	 	 actor_sumup/1]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback create() -> 
	Actor :: term().

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().

%% ===================================================================
%% Helper functions
%% ===================================================================

%%% @doc Helper function for create/11. This function call create/3 then.
%%% Some values are already set up:
%%%  * State = off
%%% @see create/3
%%% @see create/11
%%% @end
create(Module, Work_time) ->
	simdr_actor_contract:create(Module, off, Work_time).

%%% @doc Helper function for create/11. This function call create/8 then.
%%% Some values are already set up:
%%%  * Opt = [] (empty)
%%%  * In = [] (empty)
%%%  * Out = [] (empty)
%%%  * List_data = [] (empty)
%%% @spec (Module, State, WorkTime) -> Actor
%%% 	   Module = atom()
%%% 	   State = atom()
%%% 	   WorkTime = non_neg_integer()
%%% @see create/8
%%% @see create/11
%%% @end
create(Module, State, Work_time) ->
	simdr_actor_contract:create(Module, 
		simdr_actor_contract:random_id(), 
		[], 
		State, 
		[], 
		[], 
		Work_time, 
		[]).

%%% @doc Helper function for create/11. This function call create/8 then.
%%% Some values are already set up:
%%%  * In = [] (empty)
%%%  * Out = [] (empty)
%%% @see create/8
%%% @see create/11
%%% @end
create(Module, Name, Opt, State, Work_time, List_data) ->
	simdr_actor_contract:create(Module,
		Name, 
		Opt, 
		State, 
		[], 
		[], 
		Work_time, 
		List_data).

%%% @doc Helper function for create/11. This function call create/11 then.
%%% Some values are already set up:
%%%  * Pid = 0 (will be changed automatically when actor is started.)
%%%  * Capacity = infinity
%%% @spec (Module, Name, Opt, State, In, Out, Work_time, List_data) -> Actor
%%%       Module = atom()
%%%		  Name = any()
%%%		  Opt = list(tuple(any(), any()))
%%%		  In = tuple()
%%%		  Out = tuple()
%%%		  Work_time = non_neg_integer()
%%%		  List_data = list(any())
%%% @see create/11
%%% @end
create(Module, Name, Opt, State, In, Out, Work_time, List_data) ->
	simdr_actor_contract:create(Module, 
		Name, 
		simdr_actor_contract:random_id(),
		Opt, 
		State, 
		In, 
		Out, 
		{In, Out}, 
		infinity, 
		Work_time, 
		List_data).


%%% @doc Set up a configuration for an Actor.
%%% All ETS tables are initialized. All default values must be given by 
%%% parameters of the function.
%%% @spec (Module :: atom(), 
%%%        Name :: any(), 
%%% 	   Pid :: any(), 
%%% 	   Opt :: list(tuple(any(), any())), 
%%% 	   State :: atom(), 
%%% 	   In :: list(pid()), 
%%% 	   Out :: list(pid()), 
%%% 	   InOut :: any(), 
%%% 	   Capacity :: integer(), 
%%% 	   Work_time :: integer(), 
%%% 	   List_data :: any()) -> Actor
%%% @end
create(
		Module, 
		Name, 
		Pid, 
		Opt, 
		State, 
		In, 
		Out, 
		InOut, 
		Capacity, 
		Work_time, 
		List_data) ->
	?CREATE_DEBUG_TABLE,
	?DLOG(lists:concat(["Initialising ets tables of", Name])),
	Actor = #actor{
		module    = Module,
		name 	= Name, 
		pid        = Pid, 
		opt       = ets:new(
						list_to_atom(lists:concat(["Options_", Module, Name])),
						[duplicate_bag,
						{write_concurrency, true},
						{read_concurrency, true},
						public]),
		state     = State, 
		in        = In, 
		out       = Out, 
		in_out    = InOut, 
		capacity  = Capacity, 
		work_time = Work_time, 
		list_data = ets:new(
						list_to_atom(lists:concat(["Data_",Module, Name])), 
						[ordered_set, 
						{write_concurrency, true}, 
						{read_concurrency, true}, 
						public])},
	Actor1     = add_options_helper(Actor, Opt),
	Actor2	   = set_option(Actor1, mode, discrete),
	TableQueue = ets:new(list_to_atom(lists:concat(["Queue_",Module, Name])), [duplicate_bag, public]),
	Actor3     = simdr_actor_contract:set_option(Actor2, ets, TableQueue),
	Actor4     = add_datas_helper(Actor3, List_data),
	Actor4.

%%% @doc Get module field name of an Actor.
%%% @end
get_module(Actor) ->
	Actor#actor.module.

%%% @doc Add data in the actor's database. (To be exported)
%%% Is it stored in an ETS database.
%%% @spec (Actor, X) -> Actor
%%% @end
add_data(Actor, X) ->
	{Info, Destination}=X,
	Time = simdr_timemachine:get_time(),
	Data = {erlang:now(), Time, Actor, Info, Destination},
	ETSData = Actor#actor.list_data,
	?DLOG(
		simdr_actor_contract:get_name(Actor),
		{lists:concat(["Inserting data to", ETSData]), Data}),
	?MFORMAT(Actor, "~w `~w from ~w`~n", [actor_sumup(Actor), Info, actor_sumup(Destination)]),
	simdr_tools:add_data_in_ets(ETSData, Data),
%%	(ets:insert(ETSData, Data)=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

%%% @doc Set Pid of an Actor.
%%% @spec (Actor, Pid) -> Actor
%%% @end
set_pid(Actor, Pid) ->
	Actor#actor{pid= Pid}.

%%% @doc Get pid field of an Actor.
%%% @spec (Actor) -> integer() | pid()
%%% @end
get_pid(Actor) ->
	Actor#actor.pid.

%%% @doc Get name field of an Actor.
%%% @spec (Actor) -> string()
%%% @end
get_name(Actor) ->
	Actor#actor.name.

%%% @doc Get ETS table identifier of an Actor.
%%% @spec (Actor) -> number()
%%% @end
get_opt(Actor) ->
	Actor#actor.opt.

%%% @doc Get work time field of an Actor.
%%% @spec (Actor) -> integer()
%%% @end
get_work_time(Actor) ->
	Actor#actor.work_time.

%%% @doc Get mode option of an Actor.
%%% @spec (Actor) -> integer()
%%% @end
get_mode(Actor) ->
	A = get_option(Actor, mode),
	get_mode_helper(A).

set_mode(Actor, Mode) ->
	A = set_option(Actor, mode, Mode),
	A.

%%% @doc Get state field of an Actor.
%%% @spec (Actor) -> string()
%%% @end
get_state(Actor) ->
	Actor#actor.state.	

%%% @doc Get list data of an Actor.
%%% @spec (Actor) -> number()|atom()
%%% @end
get_list_data(Actor) ->
	Actor#actor.list_data.

%%% @doc Set work time field of an Actor.
%%% @spec (Actor, integer()) -> Actor
%%% @end
set_work_time(Actor, Work_time)->
	 Actor#actor{work_time =Work_time}.

%%% @doc Set state field of an Actor.
%%% @spec (Actor, any()) -> Actor
%%% @end
set_state(Actor, State) ->
	Actor#actor {state = State}.

%%% @doc Set name field of an Actor.
%%% @end
set_name(Actor, Name) ->
	Actor#actor {name = Name}.

%%% @doc Get `in' field of an Actor.
%%% @end
get_in(Actor) ->
	Actor#actor.in.

%%% @doc Set `in' field of an Actor.
%%% @end
set_in(Actor, In) ->
	Actor#actor{in = In}.

%%% @doc Add pid in `in' field of Actor.
%%% @spec (ActorA, In :: pid()) -> ActorB
%%% @end
add_in(Actor, In) ->
	Actor#actor{in = [In] ++ Actor#actor.in}.

%%% @doc Get `out' field of an Actor.
%%% @spec (Actor) -> list(pid())
%%% @end
get_out(Actor) ->
	Actor#actor.out.

%%% @doc Set `out' field of an Actor.
%%% @spec (Actor, Out) -> ActorB
%%% @end
set_out(Actor, Out) ->
	Actor#actor{out = Out}.

%%% @doc Add pid in `out' field of an Actor.
%%% @spec (ActorA, Out) -> ActorB
%%% @end
add_out(Actor, Out) ->
	Actor#actor{out = [Out] ++ Actor#actor.out}.

%%% @doc Get `in_out' field of an Actor.
%%% @spec (Actor) -> List
%%% 	  List = [pid()]
%%% @end
get_in_out(Actor) ->
	Actor#actor.in_out.

%%% @doc Set `in_out' field of Actor.
%%% @spec (ActorB, {In, Out}) -> ActorB
%%% 	  In = pid()
%%%		  Out = pid()
%%% @end
set_in_out(Actor, {In, Out}) ->
	Actor#actor{in_out = {In, Out}}.

%%% @doc Get `capacity' field of an Actor.
%%% @spec (Actor) -> non_neg_integer()
%%% @end
get_capacity(Actor) -> 
	Actor#actor.capacity.

%%% @doc Set `capacity' field of an Actor.
%%% @spec (ActorA, non_neg_integer()) -> (ActorB)
%%% @end
set_capacity(Actor, Capacity) ->
	Actor#actor{capacity = Capacity}.

%%% @doc Search an option using the Key.
%%% 	 Uses the ETS table in field `opts'.
%%% @end
get_option(Actor, Key) ->
	Table = Actor#actor.opt,
	simdr_tools:get_option_from_ets(Table, Key).

%%% @doc Set an option using `Key' and `Value'.
%%% @spec (Actor, Key, Value) -> Actor
%%% 	  Key = atom()
%%% 	  Value = any()
%%% @end
set_option(Actor, Key, Value) ->
	Table = Actor#actor.opt,
	?DLOG(
		simdr_actor_contract:get_name(Actor),
		{lists:concat(["Inserting option to ", Table]), {Key, Value}}),
	true = simdr_tools:set_option_in_ets(Table, Key, Value),
	simdr_actor_contract:add_data(Actor, {{option,setted}, {{Key, Value}}}),
	Actor.

%%% @doc Delete one option in the Actor ETS option table.
%%% @spec (Actor, Key :: any()) -> Actor
%%% @end	
delete_option(Actor, Key) ->
	Table = Actor#actor.opt,
	simdr_tools:delete_option_in_ets(Table, Key),
	simdr_actor_contract:add_data(Actor, {{option,deleted}, {Key}}),
	%%(ets:delete(Table, Key)=:= true) orelse ?DLOG("Deletion failed"),
	Actor.

%%% @doc Add one option in the Actor ETS option table.
%%% @spec (Actor, Key :: any(), Value :: any) -> Actor
%%% @end
add_option(Actor, Key, Value) ->
	Table = Actor#actor.opt,
	simdr_tools:add_option_in_ets(Table, Key, Value),
	simdr_actor_contract:add_data(Actor, {{option,added}, {{Key, Value}}}),
	%%(ets:insert(Table, {Key, Value})=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

%%% @doc Set options with key and a value of the list
%%% @spec (Actor, Key :: any(), List:: any()) -> Actor
%%% @end
set_options(Actor, Key, List) ->
	Table = Actor#actor.opt,
	true = simdr_tools:set_options_in_ets(Table, Key, List),
	Actor.

-ifdef(TEST).


work(_) ->
	timer:sleep(1000).

-else.
%%% @doc Shortcut for work/2.
%%% @see work/2
%%% @end
work(Actor) when is_record(Actor, actor) ->
	Time = get_work_time(Actor),
	Mode = get_mode(Actor),
	?MFORMAT(Actor, "~w working for ~w seconds.~n", [actor_sumup(Actor), Time]),
	work(Time, Mode).

-endif.

work(TimeToWork, rt) ->
	%%% Real time mode. Nothing to do.
	event_dispatcher(TimeToWork, rt);
work(TimeToWork, Value) ->
	%%% Discrete time mode. Need to forward time.
	event_dispatcher(TimeToWork, Value),
	%%% Forward time here of `TimeToWork' seconds from here.
	simdr_timemachine:forward_time(TimeToWork).


%%% @doc Sleep for the specified time (in seconds).
%%% 	 It is aware of the mode: discrete/real-time.
%%% @spec (N :: non_neg_integer(), Mode :: atom()) -> ok
%%% @end
event_dispatcher(N, rt) ->
	Time = N*1000,
	tc51eventmgr:postincr(100, self(), {work_beginner}),
	receive
		{notify, _SimuTime, BeginningWorkToken, {work_beginner}} ->
			tc51eventmgr:postincr(Time, self(), {work_ender}),
			receive
				{notify, _AnotherSimuTime, EndWorkToken, {work_ender}} ->
					%%% Work is done
					%%% Time to send back all tokens
					tc51eventmgr:returntoken(EndWorkToken, self()),
					tc51eventmgr:returntoken(BeginningWorkToken, self()),
					?DFORMAT("<io><io> Sent all tokens (RT). <oi><oi>~n")
			end
	end;
event_dispatcher(N, _) ->
	Time = N*1000,
	tc51eventmgr:postincr(Time, self(), {work_ender}),
	receive
		{notify, _AnotherSimuTime, EndWorkToken, {work_ender}} ->
			%%% Work is done
			%%% Time to send back all tokens
			tc51eventmgr:returntoken(EndWorkToken, self()),
			?DFORMAT("<io><io> Sent all tokens (D). <oi><oi>~n")
	end.


%%% @doc Size of a list.
%%% @end
list_size(List) ->
	list_size_helper(List, 0).

%%% @doc Helper function to add data to two actors.
%%% @spec (ActorA, DataA, ActorB, DataB) -> tuple(ActorA, ActorB)
%%% @see add_data/2
%%% @end
add_to_list_data(FirstActor, FirstData, SecondActor, SecondData) ->
	{add_data(FirstActor, FirstData), add_data(SecondActor, SecondData)}.

%%% @doc 
%%% @end
first_key_awaiting([], _Key) ->
	{ nothing, nothing};

%%% @doc Look up the first actor's answer of sending product 
%%% @spec (List, Pid() ) -> tuple(Pid, erlang:now())
%%% @end
first_key_awaiting(List, Key) ->
	[H|T] = List,
	{awaiting, {K, Id}} = H,
	case K =:= Key of
		true -> {awaiting, {K, Id}};
		false -> first_key_awaiting(T, Key)
	end.

%%% @doc Returns head of a list..
%%% @spec (list()) -> any()
%%% @end
first([]) ->
	[];
first([H|_T]) ->
	H.

%%% @doc Get first data inputted in the actor option ETS table.
%%% @end
get_data(Actor) ->
	ETS = Actor#actor.list_data,
	Key = ets:first(ETS),
	[HeadData|_Rest] = ets:lookup(ETS, Key),
	?DLOG(lists:concat(["First element from ", ETS]), HeadData),
	HeadData.

%%% @doc Look up if there are differents senders in the awaiting
%%% @spec (list()) -> atom()
%%% @end
different_sender(Awaiting)->
	[H|T] = Awaiting, 
	different_sender_helper(H, T).

%%% @doc Generate a number between 1 and 1000.
%%% @spec () -> non_neg_integer()
%%% @end
random_id() ->
	random:uniform(1000).

%%% @doc Furnish a smaller format of the Actor record.
%%% @spec (Actor) -> Actor | {atom(), term(), atom()}
%%% @end
actor_sumup(Actor) when is_record(Actor, actor) ->
	{simdr_actor_contract:get_module(Actor), 
	 simdr_actor_contract:get_name(Actor),
	 simdr_actor_contract:get_state(Actor)};
actor_sumup({Actor}) when is_record(Actor, actor) ->
	{simdr_actor_contract:get_module(Actor), 
	 simdr_actor_contract:get_name(Actor),
	 simdr_actor_contract:get_state(Actor)};
actor_sumup(Actor) ->
	Actor.


%% ===================================================================
%% Internal API
%% ===================================================================

get_mode_helper(discrete) ->
	discrete;
get_mode_helper([discrete]) ->
	discrete;
get_mode_helper(unknown_option) -> %%% Default case
	discrete;
get_mode_helper(rt) ->
	rt;
get_mode_helper([rt]) ->
	rt;
get_mode_helper(V) ->
	?DFORMAT("Warning: Unknown mode `~w`. Setting to discrete.~n", [V]),
	discrete.

add_datas_helper(Actor, []) ->
	Actor;
add_datas_helper(Actor, [Data|T]) ->
	Actor2 = add_data(Actor, Data),
	add_datas_helper(Actor2, T).

add_options_helper(Actor, []) ->
	Actor;
add_options_helper(Actor, [{Key, Value}|T]) ->
	Actor2 = add_option(Actor, Key, Value),
	add_options_helper(Actor2, T).

different_sender_helper(S, [H|T]) ->
	case S =:= H of 
		true -> different_sender_helper(H, T);
		false -> true
	end;

different_sender_helper(_S, []) ->
	false.

list_size_helper([], Acc) ->
	Acc;
list_size_helper(unknown_option, 0) ->
	0;
list_size_helper([_H|T], Acc) ->
	list_size_helper(T, Acc+1).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

get_data_1_test() ->
	Actor = create(mod, test, [{opt1, v2}], busy, 3, []),
	NewActor = add_data(Actor, {4, moi}),
	?_assertMatch(
		{_Date, _Hour, 4, moi}, get_data(NewActor)).

get_module_test() ->
	Actor = create(mod, test, [{opt1, v2}, {opt2, v1}], busy, 3, []),
	mod = get_module(Actor).

% add_data_test() ->
% 	Actor = create(mod, test, [], on, 0, [1,2]),
% 	NewActor = add_data(Actor, 3),
% 	[3,1,2] = NewActor#actor.list_data.

get_name_test() ->
	Actor = create(mod, test, [], off, 0, []),
	test = get_name(Actor).

get_opt_1_test() ->
	Actor = create(mod, test, [{key, value}], on, 0, []),
	[value] = simdr_actor_contract:get_option(Actor, key).

get_work_time_test() ->
	Actor = create(mod, test, [{key, value}], on, 42, []),
	42 = get_work_time(Actor).

get_state_1_test() ->
	Actor = create(mod, test, [{key, value}], on, 42, []),
	on = get_state(Actor).

get_state_2_test() ->
	Actor =create(mod, off, 0),
	off = get_state(Actor).

set_work_time_test()->
	Actor = create(mod, test, [{key, value}], on, 42, []),
	NewActor = set_work_time(Actor, 12),
	12 = NewActor#actor.work_time.

get_option_test_() ->
	Actor = create(mod, test, [{friend, yes}, {friendo, no}, {friend, haha}], on, 42, []),
	[
	?_assertEqual(
			[yes, haha],
			get_option(Actor, friend)
		),
	?_assertEqual(
			[no],
			get_option(Actor, friendo)
		),
	?_assertEqual(
			unknown_option,
			get_option(Actor, truc)
		)
	].

add_option_test_() ->
	Actor = create(mod, test, [], on, 42, []),
	ActorB = add_option(Actor, friend, no),
	ActorA = add_option(ActorB, friendo, yes),
	[
	?_assertEqual(
			[no],
			get_option(ActorA, friend)
		),
	?_assertEqual(
			[yes],
			get_option(ActorA, friendo)
		),
	?_assertEqual(
			unknown_option,
			get_option(ActorA, truc)
		)
	].

set_option_test_() ->
	Actor = simdr_actor_contract:create(mod, test, [{del, 3}, {save, 9}], on, 42, []),
	ActorD = simdr_actor_contract:create(mod, test, [{save, 9}, {save, 4578247}], on, 42, []),
	NewResult = simdr_actor_contract:get_option(
		simdr_actor_contract:set_option(Actor, del, 9),
		del),
	NewResult2 = simdr_actor_contract:get_option(
		simdr_actor_contract:set_option(ActorD, save, 42),
		save),
	[
	?_assertMatch(
		[9],
		NewResult),
	?_assertMatch(
		[42],
		NewResult2)
	].


list_size_test_() ->
	A = [1,2,3],
	B = [],
	C = [1,2],
	D = unknown_option,
	[
	?_assertEqual(3, list_size(A)),
	?_assertEqual(0, list_size(B)),
	?_assertEqual(2, list_size(C)),
	?_assertEqual(0, list_size(D))
	].


work_time_test_() ->
	Actor = create(mod, id, [], on, [], [], 10, []),
	[?_assertEqual(
		10, get_work_time(Actor))
	].

-endif.
