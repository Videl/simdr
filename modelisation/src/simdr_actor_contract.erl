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
		 list_size/1,
		 first_key_awaiting/2,
		 different_sender/1,
		 random_id/0,
		 first/1,
		 delete_option/2]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback create() -> Actor :: term().

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
%%% @see create/11
%%% @end
create(Module, Name, Opt, State, In, Out, Work_time, List_data) ->
	simdr_actor_contract:create(Module, 
		Name, 
		0,
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
%%% 
%%% @end
create(Module,Name, Pid, Opt, State, In, Out, InOut, Capacity, Work_time, List_data) ->
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
	TableQueue = ets:new(list_to_atom(lists:concat(["Queue_",Module, Name])), [duplicate_bag, public]),
	Actor3     = simdr_actor_contract:set_option(Actor1, ets, TableQueue),
	Actor4     = add_datas_helper(Actor3, List_data),
	Actor4.

%%% @doc Get module name of an Actor.
%%% @end
get_module(Actor) ->
	Actor#actor.module.

%%% @doc Add data in the actor's database. (To be exported)
%%% Is it stored in an ETS database.
%%% @spec (Actor, X) -> Actor
%%% @end
add_data(Actor, X) ->
	{Info, Destination}=X,
	Data = {erlang:now(), erlang:localtime(), Actor, Info, Destination},
	ETSData = Actor#actor.list_data,
	?DLOG(
		simdr_actor_contract:get_name(Actor),
		{lists:concat(["Inserting data to", ETSData]), Data}),
	simdr_tools:add_data_in_ets(ETSData, Data),
%%	(ets:insert(ETSData, Data)=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

%%% @doc Set Pid of an Actor.
%%% @spec (Actor, Pid) -> Actor
%%% @end
set_pid(Actor, Pid) ->
	Actor#actor{pid= Pid}.

%%% @doc Get Pid of an Actor.
%%% @spec (Actor) -> integer() | pid()
%%% @end
get_pid(Actor) ->
	Actor#actor.pid.

%%% @doc Get Name of an Actor.
%%% @spec (Actor) -> string()
%%% @end
get_name(Actor) ->
	Actor#actor.name.

%%% @doc Get ETS table identifier of an Actor.
%%% @spec (Actor) -> integer()
%%% @end
get_opt(Actor) ->
	Actor#actor.opt.

%%% @doc Get work_time of an Actor.
%%% @spec (Actor) -> integer()
%%% @end
get_work_time(Actor) ->
	Actor#actor.work_time.

%%% @doc Get State of an Actor.
%%% @spec (Actor) -> string()
%%% @end
get_state(Actor) ->
	Actor#actor.state.

%%% @doc
%%% @end
set_work_time(Actor, Work_time)->
	 Actor#actor{work_time =Work_time}.

%%% @doc
%%% @end
set_state(Actor, State) ->
	Actor#actor {state = State}.

%%% @doc
%%% @end
set_name(Actor, Name) ->
	Actor#actor {name = Name}.

%%% @doc
%%% @end
get_in(Actor) ->
	Actor#actor.in.

%%% @doc
%%% @end
set_in(Actor, In) ->
	Actor#actor{in = In}.

%%% @doc
%%% @end
add_in(Actor, In) ->
	Actor#actor{in = [In] ++ Actor#actor.in}.

%%% @doc
%%% @end
get_out(Actor) ->
	Actor#actor.out.

%%% @doc
%%% @end
set_out(Actor, Out) ->
	Actor#actor{out = Out}.

%%% @doc
%%% @end
add_out(Actor, Out) ->
	Actor#actor{out = [Out] ++ Actor#actor.out}.

%%% @doc
%%% @end
get_in_out(Actor) ->
	Actor#actor.in_out.

%%% @doc
%%% @end
set_in_out(Actor, {In, Out}) ->
	Actor#actor{in_out = {In, Out}}.

%%% @doc
%%% @end
get_capacity(Actor) -> 
	Actor#actor.capacity.

%%% @doc
%%% @end
set_capacity(Actor, Capacity) ->
	Actor#actor{capacity = Capacity}.

%%% @doc
%%% @end
get_option(Actor, Key) ->
	Table = Actor#actor.opt,
	simdr_tools:get_option_from_ets(Table, Key).

%%% @doc
%%% @end
set_option(Actor, Key, Value) ->
	Table = Actor#actor.opt,
	?DLOG(
		simdr_actor_contract:get_name(Actor),
		{lists:concat(["Inserting option to ", Table]), {Key, Value}}),
	true = simdr_tools:set_option_in_ets(Table, Key, Value),
	simdr_actor_contract:add_data(Actor, {{option,setted}, {{Key, Value}}}),
	Actor.

%%% @doc
%%% @end	
delete_option(Actor, Key) ->
	Table = Actor#actor.opt,
	simdr_tools:delete_option_in_ets(Table, Key),
	simdr_actor_contract:add_data(Actor, {{option,deleted}, {Key}}),
	%%(ets:delete(Table, Key)=:= true) orelse ?DLOG("Deletion failed"),
	Actor.

%%% @doc
%%% @end
add_option(Actor, Key, Value) ->
	Table = Actor#actor.opt,
	simdr_tools:add_option_in_ets(Table, Key, Value),
	simdr_actor_contract:add_data(Actor, {{option,added}, {{Key, Value}}}),
	%%(ets:insert(Table, {Key, Value})=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

%%% @doc
%%% @end
work(N) ->
	timer:sleep(round(N*1000)).

%%% @doc
%%% @end
list_size(List) ->
	list_size_helper(List, 0).

%%% @doc
%%% @end
add_to_list_data(FirstActor, FirstData, SecondActor, SecondData) ->
	{add_data(FirstActor, FirstData), add_data(SecondActor, SecondData)}.

%%% @doc
%%% @end
first_key_awaiting([], _Key) ->
	{ nothing, nothing};

%%% @doc
%%% @end
first_key_awaiting(List, Key) ->
[H|T] = List,
{awaiting,{K, Id}} = H,
case K =:= Key of
	true -> {awaiting,{K,Id}};
	false -> first_key_awaiting(T, Key)
end.

%%% @doc
%%% @end
first([]) ->
	[];
first([H|_T]) ->
	H.

%%% @doc
%%% @end
get_data(Actor) ->
	ETS = Actor#actor.list_data,
	Key = ets:first(ETS),
	[HeadData|_Rest] = ets:lookup(ETS, Key),
	?DLOG(lists:concat(["First element from ", ETS]), HeadData),
	HeadData.

%%% @doc
%%% @end
different_sender(Awaiting)->
	[H|T] = Awaiting, 
	different_sender_helper(H, T).

%%% @doc
%%% @end
random_id() ->
	random:uniform(1000).


%% ===================================================================
%% Internal API
%% ===================================================================

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

get_opt_2_test() ->
	Actor = create(mod, test, 0),
	[
	?_assertMatch(
		[{ets, _}, {awaiting, 0}],
		get_opt(Actor))
	].

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
