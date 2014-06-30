-module(actor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create/10, 
		create/8, 
		create/6,
		 create/3,
		 create/2, 
		 get_module/1,
		 add_data/2, 
		 add_in/2,
		 add_out/2,
		 add_option/3,
		 add_to_list_data/2,
		 get_list_data/1,
		 get_data/1, 
		 get_previous_data/2, 
		 get_id/1,
		 get_in/1,
		 get_out/1,
		 get_in_out/1,
		 get_capacity/1, 
		 get_opt/1, 
		 get_option/2,	 
		 get_work_time/1, 
		 get_state/1, 
		 set_id/2,
		 set_work_time/2,
		 set_state/2,
		 set_in/2,
		 set_out/2,
		 set_in_out/2,
		 set_capacity/2,
		 set_option/3, 
		 work/1,
		 list_size/1,
		 first/1,
		 answer/2,
		 random_id/0]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback create() -> Actor :: tuple(atom(), term()).

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().

%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Work_time) ->
	actor_contract:create(Module, actor_contract:random_id(), [], off, [], [], infinity, Work_time, []).

create(Module, State, Work_time) ->
	actor_contract:create(Module, actor_contract:random_id(), [], State, [], [], Work_time, []).

create(Module, Id, Opt, State, Work_time, List_data) ->
	actor_contract:create(Module, Id, Opt, State, [], [], Work_time, List_data).

create(Module, Id, Opt, State, In, Out, Work_time, List_data) ->
	actor_contract:create(Module, Id, Opt, State, In, Out, {In,Out}, infinity, Work_time, List_data).

create(Module, Id, Opt, State, In, Out, InOut, Capacity, Work_time, List_data) ->
	Actor = #config{module=Module, 
					id=Id, 
					opt=Opt, 
					state=State, 
					in=In, 
					out=Out, 
					in_out=InOut, 
					capacity=Capacity, 
					work_time=Work_time, 
					list_data=List_data},
	Actor2 = actor_contract:set_option(Actor, awaiting, 0),
	TablePid = ets:new(internal_queue, [duplicate_bag, public]),
	Actor3 = actor_contract:set_option(Actor2, ets, TablePid),
	Actor3.

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

set_id(Actor, Id) ->
	Actor#config{id= Id}.

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

get_in(Actor) ->
	Actor#config.in.

set_in(Actor, In) ->
	Actor#config{in = In}.

add_in(Actor, In) ->
	Actor#config{in = [In] ++ Actor#config.in}.

get_out(Actor) ->
	Actor#config.out.


set_out(Actor, Out) ->
	Actor#config{out = Out}.

add_out(Actor, Out) ->
	Actor#config{out = [Out] ++ Actor#config.out}.

get_in_out(Actor) ->
	Actor#config.in_out.


set_in_out(Actor, {In, Out}) ->
	Actor#config{in_out = {In, Out}}.

get_capacity(Actor) -> 
	Actor#config.capacity.

set_capacity(Actor, Capacity) ->
	Actor#config{capacity = Capacity}.

get_option(Actor, Key) ->
	Opts = Actor#config.opt,
	get_option_helper(Opts, Key).

set_option(Actor, Key, Value) ->
	NewActor = delete_option(Actor, Key),
	add_option(NewActor, Key, Value).
	
delete_option( Actor, Key) ->	
	Option = delete_option_helper(Key, get_opt(Actor), [] ),
	Actor#config {opt = Option}.

add_option(Actor, Key, Value) ->
	Actor#config{opt = [{Key, Value}] ++ Actor#config.opt}.

work(N) ->
	timer:sleep(N*1000).

list_size(List) ->
	list_size_helper(List, 0).

add_to_list_data({FirstActor, FirstData}, {SecondActor, SecondData}) ->
	{add_data(FirstActor, {FirstData, erlang:localtime()}), add_data(SecondActor, {SecondData, erlang:localtime()})}.

answer(ActorConfig, {supervisor, ping}) ->
	{ActorConfig, {supervisor, pong}};

answer(ActorConfig, {change, work_time, N}) ->
	NewConfig = actor_contract:set_work_time(ActorConfig, N),
	{NewConfig, {work_time, N, changed}, supervisor};

answer(ActorConfig, {change, state, State}) ->
	NewConfig = actor_contract:set_state(ActorConfig, State),
	{NewConfig, {state, State, changed}, supervisor};

answer(ActorConfig, {change, in_out, {In, Out}}) ->
	NewConfig = actor_contract:set_in_out(ActorConfig, {In, Out}),
	{NewConfig, {in_out, {In, Out}, changed}, supervisor};

answer(ActorConfig, {add, in, In}) ->
	NewConfig = actor_contract:add_in(ActorConfig, In),
	{NewConfig, {in, In, added}, supervisor};

answer(ActorConfig, {add, out, Out}) ->
	NewConfig = actor_contract:add_out(ActorConfig, Out),
	{NewConfig, {out, Out, added}, supervisor};

answer(ActorConfig, {add, option, Opt}) ->
	{Key, Desc}=Opt,
	NewConfig = actor_contract:add_option(ActorConfig, Key, Desc),
	{NewConfig, {option, Opt, added}, supervisor};

answer(ActorConfig, {status, work_time}) ->
	{ActorConfig, {work_time, actor_contract:get_work_time(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, state}) ->
	{ActorConfig, {state, actor_contract:get_state(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, in}) ->
	{ActorConfig, {state, actor_contract:get_in(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, out}) ->
	{ActorConfig, {state, actor_contract:get_out(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, in_out}) ->
	{ActorConfig, {state, actor_contract:get_in_out(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, capacity}) ->
	{ActorConfig, {state, actor_contract:get_capacity(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, list_data}) ->
	{ActorConfig, {list_data, actor_contract:get_list_data(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, option, Key}) ->
	{ActorConfig, {option, actor_contract:get_option(ActorConfig, Key), status}, supervisor};

answer(ActorConfig, {status, module}) ->
	{ActorConfig, {module, actor_contract:get_module(ActorConfig), status}, supervisor};

answer(ActorConfig, {status, id}) ->
	{ActorConfig, {id, actor_contract:get_id(ActorConfig), status}, supervisor};

answer(_, Request) ->
	io:format(">>>UNKNOWN ANSWER<<< (~w) (~w:~w)~n", [Request, ?MODULE, ?LINE]),
	exit(unknown_request).

first(List) ->
	get_head_data(List).

random_id() ->
	random:uniform(1000).
	
%% ===================================================================
%% Internal API
%% ===================================================================
delete_option_helper(Filtre, [Head| Tail], []) ->
	{Key,_ }=Head,
	case Key=:=Filtre of
		true -> delete_option_helper(Filtre, Tail, []);
		false -> delete_option_helper(Filtre, Tail, [Head])
	end;

delete_option_helper(Filtre, [Head| Tail], Result) ->
	{Key,_ }=Head,
	case Key=:=Filtre of
		true -> delete_option_helper(Filtre, Tail, Result);
		false -> delete_option_helper(Filtre, Tail, [Head]++Result)
	end;

delete_option_helper(_Filtre, [], Result) ->
	Result.

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


-ifdef(TEST).
%% ===================================================================
%% Tests
%% ===================================================================

mock_actor() ->
	Actor = create(mod, test, [{opt1, v2}, {opt2, v1}], busy, 3, [1,2,3]),
	Actor.

get_module_test() ->
	Actor = create(mod, test, [{opt1, v2}, {opt2, v1}], busy, 3, [1,2,3]),
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
	Actor = create(mod, test, [], on, 0, [1,2]),
	NewActor = add_data(Actor, 3),
	[3,1,2] = NewActor#config.list_data.

get_id_test() ->
	Actor = create(mod, test, [], off, 0, []),
	test = get_id(Actor).

get_opt_1_test() ->
	Actor = create(mod, test, [{key, value}], on, 0, [1,2]),
	[value] = actor_contract:get_option(Actor, key).

get_opt_2_test() ->
	Actor = create(mod, test, 0),
	[
	?_assertMatch(
		[{ets, _}, {awaiting, 0}],
		get_opt(Actor))
	].

get_opt_3_test_() ->
	Actor = create(mod, test, [{opt1, v2}, {opt2, v1}], on, 0, [1,2]),
	[
	?_assertMatch(
		[{ets, _}, {opt1, v2}, {opt2, v1}, {awaiting, 0}],
		get_opt(Actor))
	].

get_work_time_test() ->
	Actor = create(mod, test, [{key, value}], on, 42, [1,2]),
	42 = get_work_time(Actor).

get_state_1_test() ->
	Actor = create(mod, test, [{key, value}], on, 42, [1,2]),
	on = get_state(Actor).

get_state_2_test() ->
	Actor =create(mod, off, 0),
	off = get_state(Actor).

set_work_time_test()->
	Actor = create(mod, test, [{key, value}], on, 42, [1,2]),
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
	Actor = actor_contract:create(mod, test, [], on, 42, [1,2]),
	ActorB = add_option(Actor, friend, no),
	ActorA = add_option(ActorB, friend, yes),
	[
	?_assertEqual(4, actor_contract:list_size(actor_contract:get_opt(ActorA))),
	?_assertEqual(2, list_size(actor_contract:get_option(ActorA, friend)))
	].


set_option_test_() ->
	Actor = actor_contract:create(mod, test, [{del, 3},{save, 9}], on, 42, [1,2]),
	ActorD = actor_contract:create(mod, test, [{save, 9}], on, 42, [1,2]),
	[
	?_assertMatch({config,mod,test,
        [{awaiting,0},{save,9},{ets,_}],
        on,42,
        [1,2]}, delete_option(Actor, del)),
	?_assertMatch({config,mod,test,
        [{save,50},{awaiting,0},{ets,_}],
        on,42,
        [1,2]}, set_option(ActorD, save, 50))
	].



answer_test_() ->
	Actor = actor_contract:create(mod, test, [{out,1},{in,2},{out,3}], on, 42, [5,6]),
	NewState = set_state(Actor,off),
	NewWTime = set_work_time(Actor,10),
	NewOpt = add_option(Actor,in,4),
	[
	?_assertEqual(
		{Actor, {state,on, status}, supervisor},
		answer(Actor, {status, state})),
	?_assertEqual(
		{NewState, {state, off, changed}, supervisor},
		answer(Actor, {change, state, off})),
	?_assertEqual(
		{Actor, {module, mod, status}, supervisor},
		answer(Actor,{status, module})),
	?_assertEqual(
		{Actor, {id, test, status}, supervisor},
		answer(Actor,{status, id})),
	?_assertEqual(
		{Actor, {work_time, 42, status}, supervisor},
		answer(Actor, {status, work_time})),
	?_assertEqual(
		{NewWTime, {work_time, 10, changed}, supervisor},
		answer(Actor, {change, work_time, 10})),
	?_assertEqual(
		{Actor, {option, [1,3], status}, supervisor},
		answer(Actor, {status, option, out})),
	?_assertEqual(
		{NewOpt, {option,{in,4}, added}, supervisor},
		answer(Actor, {add, option,{in,4}})),
	?_assertEqual(
		{Actor, {list_data, [5,6], status}, supervisor},
		answer(Actor, {status, list_data}))
	].

-endif.
