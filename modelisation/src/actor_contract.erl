-module(actor_contract).
-include_lib("eunit/include/eunit.hrl").

% config record
-include("config.hrl").

-export([create/6, 
		 create/3, 
		 get_module/1,
		 add_data/2, 
		 add_option/3,
		 add_to_list_data/2,
		 get_list_data/1,
		 get_data/1, 
		 get_previous_data/2, 
		 get_id/1, 
		 get_opt/1, 
		 get_option/2,
		 get_work_time/1, 
		 get_state/1, 
		 set_work_time/2,
		 set_state/2, 
		 work/1,
		 list_size/1,
		 answer/2,
		 first/1]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback create() -> Actor :: tuple(atom(), term()).

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().


%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Id, Work_time) ->
	Actor = #config{module=Module, id=Id, opt=[], state=off, work_time=Work_time, list_data=[]},
	Actor.

create(Module, Id, Opt, State, Work_time, List_data) ->
	Actor = #config { module=Module, id=Id, opt=Opt, state=State, work_time=Work_time, list_data=List_data },
	Actor.

get_module(Actor) ->
	Actor#config.module.

get_list_data(Actor) ->
	Actor#config.list_data.
	
get_data(Actor) ->
	get_head_data(Actor#config.list_data).

get_previous_data(Config, N) ->
	get_previous_data_helper(Config#config.list_data, N).

add_data(Actor, X) -> 
	Actor#config{list_data = [X] ++ Actor#config.list_data}.

get_id(Actor) ->
	Actor#config.id.

get_opt(Actor) ->
	Actor#config.opt.

get_work_time(Actor) ->
	Actor#config.work_time.

get_state(Actor) ->
	Actor#config.state.

set_work_time(Actor, Work_time)->
	 Actor#config{work_time =Work_time}.

set_state(Actor, State) ->
	Actor#config {state = State}.

get_option(Actor, Key) ->
	Opts = Actor#config.opt,
	get_option_helper(Opts, Key).

add_option(Actor, Key, Value) ->
	Actor#config{opt = [{Key, Value}] ++ Actor#config.opt}.

work(N) ->
	timer:sleep(N*1000).

list_size(List) ->
	list_size_helper(List, 0).

add_to_list_data({FirstActor, FirstData}, {SecondActor, SecondData}) ->
	{add_data(FirstActor, FirstData), add_data(SecondActor, SecondData)}.

answer(ActorConfig, {supervisor, ping}) ->
	{ActorConfig, {supervisor, pong}};

answer(ActorConfig, {change, work_time, N}) ->
	NewConfig = actor_contract:set_work_time(ActorConfig, N),
	{NewConfig, changed_work_time, N};

answer(ActorConfig, {change, state, State}) ->
	NewConfig = actor_contract:set_state(ActorConfig, State),
	{NewConfig, changed_state, State};

answer(ActorConfig, {add, option, Opt}) ->
	{Key, Desc}=Opt,
	NewConfig = actor_contract:add_option(ActorConfig, Key, Desc),
	{NewConfig, added_option, Opt};

answer(ActorConfig, {status, work_time}) ->
	{ActorConfig, work_time, actor_contract:get_work_time(ActorConfig)};

answer(ActorConfig, {status, state}) ->
	{ActorConfig, state, actor_contract:get_state(ActorConfig)};

answer(ActorConfig, {status, list_data}) ->
	{ActorConfig, list_data, actor_contract:get_list_data(ActorConfig)};

answer(ActorConfig, {status, option, Key}) ->
	{ActorConfig, option, actor_contract:get_option(ActorConfig, Key)};

answer(ActorConfig, {status, module}) ->
	{ActorConfig, module, actor_contract:get_module(ActorConfig)};

answer(ActorConfig, {status, id}) ->
	{ActorConfig, id, actor_contract:get_id(ActorConfig)};

answer(_, Request) ->
	{unknown_type_of_request, Request}.

first(List) ->
	get_head_data(List).
	
%% ===================================================================
%% Internal API
%% ===================================================================

get_head_data([]) ->
	undefined;
get_head_data([Head|_Tail]) ->
	Head.

get_previous_data_helper(_List, N) when N < 0 ->
	out_of_range;
get_previous_data_helper([Head|_Tail], 1) ->
	Head;
get_previous_data_helper([], 1) ->
	out_of_range;
get_previous_data_helper([_Head|Tail], N) ->
	get_previous_data_helper(Tail, N-1).

get_option_helper(AllOptions, Key) ->
	get_option_helper_two(AllOptions, [], Key).

get_option_helper_two(undefined, _, _) ->
	unknown_option;
get_option_helper_two([], [], _Key) ->
	unknown_option;
get_option_helper_two([], Result, _Key) ->
	Result;
get_option_helper_two([{Key, Value}|RestOfOptions], Result, Key) ->
	get_option_helper_two(RestOfOptions, Result ++ [Value], Key);
get_option_helper_two([_BadHead|RestOfOptions], Result, Key) ->
	get_option_helper_two(RestOfOptions, Result, Key).

list_size_helper([], Acc) ->
	Acc;
list_size_helper(unknown_option, 0) ->
	0;
list_size_helper([_H|T], Acc) ->
	list_size_helper(T, Acc+1).

%% ===================================================================
%% Tests
%% ===================================================================

mock_actor() ->
	Actor = create(mod, test, {opt1, opt2}, busy, 3, [1,2,3]),
	Actor.

get_module_test() ->
	Actor = create(mod, test, {opt1, opt2}, busy, 3, [1,2,3]),
	 mod = get_module(Actor).

get_list_data_test() ->
	Actor = create(mod, test, 0),
	[] = get_list_data(Actor).

get_data_1_test() ->
	Actor = mock_actor(),
	1 = get_data(Actor).

get_previous_data_1_test() ->
	Actor = mock_actor(),
	2 = get_previous_data(Actor, 2).

get_previous_data_2_test() ->
	Actor = mock_actor(),
	1 = get_previous_data(Actor, 1).

get_previous_data_3_test() ->
	Actor = mock_actor(),
	3 = get_previous_data(Actor, 3).

get_head_1_test() ->
	3 = get_head_data([3,2,1]).

get_head_2_test() ->
	undefined = get_head_data([]).

get_head_3_test() ->
	1 = get_head_data([1]).

get_previous_data_helper_1_test() ->
	2 = get_previous_data_helper([1,2,3], 2).

get_previous_data_helper_2_test() ->
	1 = get_previous_data_helper([1,2,3], 1).

get_previous_data_helper_3_test() ->
	3 = get_previous_data_helper([1,2,3], 3).

get_previous_data_helper_4_test() ->
	out_of_range = get_previous_data_helper([1,2,3], 4).

get_previous_data_helper_5_test() ->
	out_of_range = get_previous_data_helper([1,2,3], -3).

get_previous_data_helper_6_test() ->
	4 = get_previous_data_helper([1,2,3, 4], 4).

add_data_test() ->
	Actor = create(mod, test, undefined, on, 0, [1,2]),
	NewActor = add_data(Actor, 3),
	[3,1,2] = NewActor#config.list_data.

get_id_test() ->
	Actor = create(mod, test, 0),
	test = get_id(Actor).

get_opt_1_test() ->
	Actor = create(mod, test, opt1, on, 0, [1,2]),
	opt1= get_opt(Actor).

get_opt_2_test() ->
	Actor = create(mod, test, 0),
	[] = get_opt(Actor).

get_opt_3_test() ->
	Actor = create(mod, test, [opt1, opt2], on, 0, [1,2]),
	[opt1, opt2] = get_opt(Actor).

get_work_time_test() ->
	Actor = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	42 = get_work_time(Actor).

get_state_1_test() ->
	Actor = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	on = get_state(Actor).

get_state_2_test() ->
	Actor =create(mod, test, 0),
	off = get_state(Actor).

set_work_time_test()->
	Actor = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	NewActor = set_work_time(Actor, 12),
	12 = NewActor#config.work_time.

get_option_test_() ->
	Actor = create(mod, test, [{friend, yes}, {friendo, no}, {friend, haha}], on, 42, [1,2]),
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
	Actor = create(mod, test, [], on, 42, [1,2]),
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

add_option_list_size_test_() ->
	Actor = create(mod, test, [], on, 42, [1,2]),
	ActorB = add_option(Actor, friend, no),
	ActorA = add_option(ActorB, friend, yes),
	[
	?_assertEqual(2, actor_contract:list_size(actor_contract:get_opt(ActorA))),
	?_assertEqual(2, list_size(actor_contract:get_option(ActorA, friend)))
	].

answer_test_() ->
 Actor = create(mod, test, [{out,1},{in,2},{out,3}], on, 42, [5,6]),
 NewState = set_state(Actor,off),
 NewWTime = set_work_time(Actor,10),
 NewOpt = add_option(Actor,in,4),
 [
?_assertEqual(
	{Actor, state,on},
	answer(Actor, {status, state})),
?_assertEqual(
	{NewState, changed_state, off},
	answer(Actor, {change, state, off})),
?_assertEqual(
	{Actor, module, mod},
	answer(Actor,{status, module})),
?_assertEqual(
	{Actor, id, test},
	answer(Actor,{status, id})),
?_assertEqual(
	{Actor, work_time, 42},
	answer(Actor, {status, work_time})),
?_assertEqual(
	{NewWTime, changed_work_time, 10},
	answer(Actor, {change, work_time, 10})),
?_assertEqual(
	{Actor, option, [1,3]},
	answer(Actor, {status, option, out})),
?_assertEqual(
	{NewOpt, added_option,{in,4}},
	answer(Actor, {change, option,{in,4}})),
?_assertEqual(
	{Actor, list_data, [5,6]},
	answer(Actor, {status, list_data}))
 ].
