-module(actor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([create/13, 
		create/9, 
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
		 get_speed/1,
		 get_distance/1, 
		 get_work_time/1, 
		 get_state/1, 
		 set_pid/2,
		 set_speed/2,
		 set_distance/2,
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
		 different_sender/1,
		 answer/2,
		 random_id/0,
		 first/1,
		 random/0,
		 random_weighted/2]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback create() -> Actor :: term().

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().

%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Work_time) ->
	actor_contract:create(Module, off, Work_time).

create(Module, State, Work_time) ->
	actor_contract:create(Module, 
		actor_contract:random_id(), 
		[], 
		State, 
		[], 
		[], 
		Work_time, 
		[]).

create(Module, Name, Opt, State, Work_time, List_data) ->
	actor_contract:create(Module,
		Name, 
		Opt, 
		State, 
		[], 
		[], 
		Work_time, 
		List_data).

create(Module, Name, Opt, State, In, Out, Work_time, List_data) ->
	actor_contract:create(Module, 
		Name, 
		0,
		Opt, 
		State, 
		In, 
		Out, 
		{In, Out}, 
		infinity, 
		1,
		Work_time,
		Work_time, 
		List_data).

create(Module, Name, Opt, State, In, Out, Speed, Distance, List_data) ->
	actor_contract:create(Module, 
		Name,
		0, 
		Opt, 
		State, 
		In, 
		Out, 
		{In, Out}, 
		infinity, 
		Speed,
		Distance,
		Distance/Speed, 
		List_data).

create(Module,Name, Pid, Opt, State, In, Out, InOut, Capacity, Speed, Distance, Work_time, List_data) ->
	?CREATE_DEBUG_TABLE,
	?DLOG(lists:concat(["Initialising ets tables of", Name])),
	Actor = #config{
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
		speed = Speed,
		distance = Distance,
		work_time = Work_time, 
		list_data = ets:new(
						list_to_atom(lists:concat(["Data_",Module, Name])), 
						[ordered_set, 
						{write_concurrency, true}, 
						{read_concurrency, true}, 
						public])},
	Actor1     = add_options_helper(Actor, Opt),
	TableQueue = ets:new(list_to_atom(lists:concat(["Queue_",Module, Name])), [duplicate_bag, public]),
	Actor3     = actor_contract:set_option(Actor1, ets, TableQueue),
	Actor4     = add_datas_helper(Actor3, List_data),
	Actor4.

get_module(Actor) ->
	Actor#config.module.

add_data(Actor, X) ->
	Data = {erlang:now(), erlang:localtime(), Actor, X},
	ETSData = Actor#config.list_data,
	?DLOG(
		actor_contract:get_name(Actor),
		{lists:concat(["Inserting data to", ETSData]), Data}),
	true = ets:insert_new(ETSData, Data),
%%	(ets:insert(ETSData, Data)=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

set_pid(Actor, Pid) ->
	Actor#config{pid= Pid}.

get_pid(Actor) ->
	Actor#config.pid.

get_name(Actor) ->
	Actor#config.name.

get_opt(Actor) ->
	Actor#config.opt.

get_work_time(Actor) ->
	Actor#config.work_time.

get_speed(Actor) ->
	Actor#config.speed.

get_distance(Actor) ->
	Actor#config.distance.

set_speed(Actor, Speed) -> 
	Actor#config{ speed = Speed, work_time = get_distance(Actor)/Speed}.

set_distance(Actor, Distance) -> 
	Actor#config{ distance = Distance, work_time = Distance/get_speed(Actor)}.

get_state(Actor) ->
	Actor#config.state.

set_work_time(Actor, Work_time)->
	 Actor#config{work_time =Work_time}.

set_state(Actor, State) ->
	Actor#config {state = State}.

set_name(Actor, Name) ->
	Actor#config {name = Name}.

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
	Var = ets:lookup(Opts, Key),
	Option = get_option_helper(Var, Key), 
	?DLOG(actor_contract:get_name(Actor),{lists:concat(["Elements value ", Key]), Option}),
	Option.

set_option(Actor, Key, Value) ->
	Actor1 = delete_option(Actor, Key),
	Actor2 = add_option(Actor1, Key, Value),
	Actor2.
	
delete_option(Actor, Key) ->
	Opts = Actor#config.opt,
	ets:delete(Opts, Key),
	%%(ets:delete(Opts, Key)=:= true) orelse ?DLOG("Deletion failed"),
	Actor.

add_option(Actor, Key, Value) ->
	Opts = Actor#config.opt,
	?DLOG(actor_contract:get_name(Actor),{lists:concat(["Inserting option to ", Opts]), {Key, Value}}),
	ets:insert(Opts, {Key, Value}),
	%%(ets:insert(Opts, {Key, Value})=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

work(N) ->
	timer:sleep(N*1000).

list_size(List) ->
	list_size_helper(List, 0).

add_to_list_data(FirstActor, FirstData, SecondActor, SecondData) ->
	{add_data(FirstActor, FirstData), add_data(SecondActor, SecondData)}.

first([]) ->
	[];
first([H|_T]) ->
	H.

answer(ActorConfig, {supervisor, ping}) ->
	{ActorConfig, {supervisor, pong}};

answer(ActorConfig, {change, distance, N}) ->
	NewConfig = actor_contract:set_distance(ActorConfig, N),
	{NewConfig, {distance, N, changed}, supervisor};

answer(ActorConfig, {change, speed, N}) ->
	NewConfig = actor_contract:set_speed(ActorConfig, N),
	{NewConfig, {speed, N, changed}, supervisor};

answer(ActorConfig, {change, work_time, N}) ->
	NewConfig = actor_contract:set_work_time(ActorConfig, N),
	{NewConfig, {work_time, N, changed}, supervisor};
answer(ActorConfig, {change, state, State}) ->
	NewConfig = actor_contract:set_state(ActorConfig, State),
	{NewConfig, {state, State, changed}, supervisor};

answer(ActorConfig, {change, capacity, Capacity}) ->
	NewConfig = actor_contract:set_capacity(ActorConfig, Capacity),
	{NewConfig, {capacity, Capacity, changed}, supervisor};

answer(ActorConfig, {change, in_out, {In, Out}}) ->
	NewConfig = actor_contract:set_in_out(ActorConfig, {In, Out}),
	{NewConfig, {in_out, {In, Out}, changed}, supervisor};

answer(ActorConfig, {add, in, In}) ->
	NewConfig = actor_contract:add_in(ActorConfig, In),
	{_In, Out} = actor_contract:get_in_out(NewConfig),
	NewConfig2 = actor_contract:set_in_out(NewConfig, {In, Out}),
	{NewConfig2, {in, In, added}, supervisor};

answer(ActorConfig, {add, out, Out}) ->
	NewConfig = actor_contract:add_out(ActorConfig, Out),
	{In, _Out} = actor_contract:get_in_out(NewConfig),
	NewConfig2 = actor_contract:set_in_out(NewConfig, {In, Out}),
	{NewConfig2, {out, Out, added}, supervisor};

answer(ActorConfig, {add, option, Opt}) ->
	{Key, Desc}=Opt,
	NewConfig = actor_contract:add_option(ActorConfig, Key, Desc),
	{NewConfig, {option, Opt, added}, supervisor};

answer(ActorConfig, {status, work_time}) ->
	{ActorConfig, 
	{work_time, actor_contract:get_work_time(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, distance}) ->
	{ActorConfig, 
	{distance, actor_contract:get_distance(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, speed}) ->
	{ActorConfig, 
	{speed, actor_contract:get_speed(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, state}) ->
	{ActorConfig, 
	{state, actor_contract:get_state(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, in}) ->
	{ActorConfig, 
	{state, actor_contract:get_in(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, out}) ->
	{ActorConfig, 
	{state, actor_contract:get_out(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, in_out}) ->
	{ActorConfig, 
	{state, actor_contract:get_in_out(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, capacity}) ->
	{ActorConfig, 
	{state, actor_contract:get_capacity(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, option, Key}) ->
	{ActorConfig, 
	{option, actor_contract:get_option(ActorConfig, Key), status}, 
	supervisor};

answer(ActorConfig, {status, module}) ->
	{ActorConfig, 
	{module, actor_contract:get_module(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, pid}) ->
	{ActorConfig, 
	{pid, actor_contract:get_pid(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {io_export, list_data}) ->
	TablePid = ActorConfig#config.list_data,
	Fun = export_to(io),
	ets:foldl(Fun, ok, TablePid),
	{ActorConfig, 
	{io_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {file_export, list_data}) ->
	TablePid = ActorConfig#config.list_data,
	%% File creation
	{ok, F} = file:open(lists:concat(["data_", 
					 				 actor_contract:get_module(ActorConfig),
					 				 % id here if its a number
					 				 ".log"]), [append, delayed_write, unicode]),
	Fun = export_to(file),
	ets:foldl(Fun, F, TablePid),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {csv_export, list_data}) ->
	TablePid = ActorConfig#config.list_data,
	%% File creation
	{ok, F} = file:open(lists:concat(["data_", 
					 				 actor_contract:get_module(ActorConfig),
					 				 % id here if its a number
					 				 ".csv"]), [append, delayed_write, unicode]),
	Fun = export_to(csv),
	R = io_lib:format("~s;~s;~s;~s\n",["Year,Month,Day", 
									   "Hour,Minutes,Seconds", 
									   "Source actor", 
									   "Message"]),
	%RX = erlang:iolist_to_binary(R),
	%RXF = lists:flatten(RX),
	ok = file:write(F,  R),
	ets:foldl(Fun, F, TablePid),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {io_export, debug}) ->
	?CREATE_DEBUG_TABLE,
	Fun = export_to(io),
	ets:foldl(Fun, ok, ?DEBUG_TABLE),
	{ActorConfig, 
	{io_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {file_export, debug}) ->
	?CREATE_DEBUG_TABLE,
	%% File creation
	{ok, F} = file:open("debug.log", [append, delayed_write, unicode]),
	Fun = export_to(file),
	ets:foldl(Fun, F, ?DEBUG_TABLE),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {csv_export, debug}) ->
	?CREATE_DEBUG_TABLE,
	%% File creation
	{ok, F} = file:open("debug.csv", [append, delayed_write, unicode]),
	Fun = export_to(csv),
	ets:foldl(Fun, F, ?DEBUG_TABLE),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(_, Request) ->
	io:format(">>>UNKNOWN ANSWER<<< (~w) (~w:~w)~n", [Request, ?MODULE, ?LINE]),
	exit(unknown_request).

random_id() ->
	random:uniform(1000).

get_data(Actor) ->
	ETS = Actor#config.list_data,
	Key = ets:first(ETS),
	[HeadData|_Rest] = ets:lookup(ETS, Key),
	?DLOG(lists:concat(["First element from ", ETS]), HeadData),
	HeadData.

different_sender(Awaiting)->
	[H|T] = Awaiting, 
	different_sender_helper(H, T).

random() ->
	random_weighted(33, 'Q1').

random_weighted(Luck, Objective) ->
    Rnd = random:uniform(100),
    %%% 0 can't arise in Rnd, so we go up to 100
    Result = case Luck >= Rnd of
				true ->
					Objective;
				 _ ->
				    random(Objective)
	     	 end,
    Result.	

%% ===================================================================
%% Internal API
%% ===================================================================

random('Q1') ->
    Result = case random:uniform(2) of
		 1 -> % Medium quality
		     'Q2';
		 2 -> % Bad quality
		     'Q3'
	     end,
    Result;
random('Q2') ->
    Result = case random:uniform(2) of
		 1 -> % Good quality
		     'Q1';
		 2 -> % Bad quality
		     'Q3'
	     end,
    Result;
random('Q3') ->
    Result = case random:uniform(2) of
		 1 -> % Good quality
		     'Q1';
		 2 -> % Medium quality
		     'Q2'
	     end,
    Result.

export_to(file) ->
	fun(X, FileDescriptor) -> 
		R = io_lib:format("~w\n",[X]),
		%RX = erlang:iolist_to_binary(R),
		%RXF = lists:flatten(RX),
		ok = file:write(FileDescriptor,  R),
		FileDescriptor 
	end;
export_to(csv) ->
	fun(X, FileDescriptor) ->
		%%% Value decomposition
		{_Now, {YearMonthDate, HourMinSecs}, Actor, Message} = X,
		R = io_lib:format("~w;~w;~w;~w\n",[YearMonthDate, 
										   HourMinSecs, 
										   Actor, 
										   Message]),
		%RX = erlang:iolist_to_binary(R),
		%RXF = lists:flatten(RX),
		ok = file:write(FileDescriptor,  R),
		FileDescriptor 
	end;
export_to(_) ->
	fun(X, Y) ->
		R = io_lib:format("~w\n",[X]),
		%RX = erlang:iolist_to_binary(R),
		%RXF = lists:flatten(RX),
		io:format("~w~n", [R]), Y
	end.

add_datas_helper(Actor, []) ->
	Actor;
add_datas_helper(Actor, [Data|T]) ->
	Actor2 = add_data(Actor, Data),
	add_datas_helper(Actor2, T).

get_option_helper(AllOptions, Key) ->
	get_option_helper_two(AllOptions, [], Key).

get_option_helper_two([], [], _Key) ->
	unknown_option;
get_option_helper_two([], Result, _Key) ->
	Result;
get_option_helper_two([{Key, Value}|RestOfOptions], Result, Key) ->
	get_option_helper_two(RestOfOptions, Result ++ [Value], Key);
get_option_helper_two([_BadHead|RestOfOptions], Result, Key) ->
	get_option_helper_two(RestOfOptions, Result, Key).

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
	Actor = create(mod, test, [{opt1, v2}], busy, 3, [1, 2, 3]),
	NewActor = add_data(Actor, 4),
	?_assertMatch(
		{_Date, _Hour, 4}, get_data(NewActor)).

get_module_test() ->
	Actor = create(mod, test, [{opt1, v2}, {opt2, v1}], busy, 3, [1,2,3]),
	mod = get_module(Actor).

% add_data_test() ->
% 	Actor = create(mod, test, [], on, 0, [1,2]),
% 	NewActor = add_data(Actor, 3),
% 	[3,1,2] = NewActor#config.list_data.

get_name_test() ->
	Actor = create(mod, test, [], off, 0, []),
	test = get_name(Actor).

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

set_option_test_() ->
	Actor = actor_contract:create(mod, test, [{del, 3}, {save, 9}], on, 42, [1,2]),
	ActorD = actor_contract:create(mod, test, [{save, 9}, {save, 4578247}], on, 42, [1,2]),
	NewResult = actor_contract:get_option(
		actor_contract:set_option(Actor, del, 9),
		del),
	NewResult2 = actor_contract:get_option(
		actor_contract:set_option(ActorD, save, 42),
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
		{Actor, {pid, 0, status}, supervisor},
		answer(Actor,{status, pid})),
	?_assertEqual(
		{Actor, {work_time, 42, status}, supervisor},
		answer(Actor, {status, work_time})),
	?_assertEqual(
		{NewWTime, {work_time, 10, changed}, supervisor},
		answer(Actor, {change, work_time, 10})),
	?_assertEqual(
		{NewOpt, {option,{in,4}, added}, supervisor},
		answer(Actor, {add, option,{in,4}}))%,
	% ?_assertEqual(
	% 	{Actor, {list_data, [5,6], status}, supervisor},
	% 	answer(Actor, {status, list_data}))
	].


work_time_test_() ->
	Actor = create(mod, id, [], on, [], [], 5, 10, []),
	Actor2= set_speed(Actor,2),
	Actor3= set_distance(Actor2,8),

	[?_assertEqual(
		2.0, get_work_time(Actor)),
	?_assertEqual(
		5.0, get_work_time(Actor2)),
	?_assertEqual(
		2, get_speed(Actor2)),
	?_assertEqual(
		4.0, get_work_time(Actor3))
	].

random_list_maker(List, _, 0) ->
    List;
random_list_maker(List, {Objective, Luck}, X) ->
    NewList = [random_weighted(Luck, Objective)] ++ List,
    random_list_maker(NewList, {Objective, Luck}, X-1).

random_list_maker({Objective, Luck}, X) ->
	random_list_maker([], {Objective, Luck}, X).

%%% List: list to traverse
%%% Chr: character to count
%%% Returns the number of times the character has been found.
%%% @end
occurences_in_a_list(List, Chr) ->
    Fonc = fun(X, N) when X =:= Chr ->
		   N+1;
	      (_, N) -> N
	   end,
    lists:foldl(Fonc, 0, List).

percent(Max, No) ->
    A = (No/Max)*100,
    B = float_to_list(A, [{decimals, 0}]),
    {C, _} = string:to_integer(B),
    C.

random_test_() ->
	random:seed(erlang:now()),
	ListQ1_100 = random_list_maker({'Q1', 100}, 10000),
	ListQ1_20  = random_list_maker({'Q1', 20}, 10000),
	ListQ1_50  = random_list_maker({'Q1', 50}, 10000),
	ListQ3_80  = random_list_maker({'Q3', 80}, 10000),
	ListQ3_30  = random_list_maker({'Q3', 30}, 10000),
	ListQ3_60  = random_list_maker({'Q3', 60}, 10000),
	ListQ2_85  = random_list_maker({'Q2', 85}, 10000),
	ListQ2_25  = random_list_maker({'Q2', 25}, 10000),
	ListQ2_55  = random_list_maker({'Q2', 55}, 10000),
	%%% Answer
	ResultQ1_100 = percent(10000, occurences_in_a_list(ListQ1_100, 'Q1')),
	ResultQ1_20  = percent(10000, occurences_in_a_list(ListQ1_20, 'Q1')),
	ResultQ1_50  = percent(10000, occurences_in_a_list(ListQ1_50, 'Q1')),
	ResultQ3_80  = percent(10000, occurences_in_a_list(ListQ3_80, 'Q3')),
	ResultQ3_30  = percent(10000, occurences_in_a_list(ListQ3_30, 'Q3')),
	ResultQ3_60  = percent(10000, occurences_in_a_list(ListQ3_60, 'Q3')),
	ResultQ2_85  = percent(10000, occurences_in_a_list(ListQ2_85, 'Q2')),
	ResultQ2_25  = percent(10000, occurences_in_a_list(ListQ2_25, 'Q2')),
	ResultQ2_55  = percent(10000, occurences_in_a_list(ListQ2_55, 'Q2')),
	MaxError = 3,
	[
		?_assert(ResultQ1_100 =:= 100),
		?_assert(ResultQ1_20 < (20 + MaxError)),
		?_assert(ResultQ1_20 > (20 - MaxError)),
		?_assert(ResultQ1_50 < (50 + MaxError)),
		?_assert(ResultQ1_50 > (50 - MaxError)),
		?_assert(ResultQ3_80 < (80 + MaxError)),
		?_assert(ResultQ3_80 > (80 - MaxError)),
		?_assert(ResultQ3_30 < (30 + MaxError)),
		?_assert(ResultQ3_30 > (30 - MaxError)),
		?_assert(ResultQ3_60 < (60 + MaxError)),
		?_assert(ResultQ3_60 > (60 - MaxError)),
		?_assert(ResultQ2_85 < (85 + MaxError)),
		?_assert(ResultQ2_85 > (85 - MaxError)),
		?_assert(ResultQ2_25 < (25 + MaxError)),
		?_assert(ResultQ2_25 > (25 - MaxError)),
		?_assert(ResultQ2_55 < (55 + MaxError)),
		?_assert(ResultQ2_55 > (55 - MaxError))

	].

-endif.
