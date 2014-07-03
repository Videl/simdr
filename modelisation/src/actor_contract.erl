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
		 add_to_list_data/4,
		 get_data/1,
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
		 different_sender/1,
		 answer/2,
		 random_id/0,
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

create(Module, Work_time) ->
	actor_contract:create(
		Module, 
		actor_contract:random_id(), 
		[], 
		off, 
		[], 
		[], 
		infinity, 
		Work_time, 
		[]).

create(Module, State, Work_time) ->
	actor_contract:create(Module, actor_contract:random_id(), [], State, [], [], Work_time, []).

create(Module, Id, Opt, State, Work_time, List_data) ->
	actor_contract:create(Module, Id, Opt, State, [], [], Work_time, List_data).

create(Module, Id, Opt, State, In, Out, Work_time, List_data) ->
	actor_contract:create(Module, Id, Opt, State, In, Out, {In,Out}, infinity, Work_time, List_data).

create(Module, Id, Opt, State, In, Out, InOut, Capacity, Work_time, List_data) ->
	?CREATE_DEBUG_TABLE,
	?DLOG(lists:concat(["Initialising ets tables of", Id])),
	Actor = #config{
		module    = Module, 
		id        = Id, 
		opt       = ets:new(list_to_atom(lists:concat(["Options_", Module,Id])), [duplicate_bag, public]), 
		state     = State, 
		in        = In, 
		out       = Out, 
		in_out    = InOut, 
		capacity  = Capacity, 
		work_time = Work_time, 
		list_data = ets:new(list_to_atom(lists:concat(["Data_",Module, Id])), [ordered_set, public])},
	Actor1     = add_options_helper(Actor, Opt),
	TableQueue = ets:new(list_to_atom(lists:concat(["Queue_",Module, Id])), [duplicate_bag, public]),
	Actor3     = actor_contract:set_option(Actor1, ets, TableQueue),
	Actor4     = add_datas_helper(Actor3, List_data),
	Actor4.

get_module(Actor) ->
	Actor#config.module.

% get_previous_data(Config, N) ->
% 	get_previous_data_helper(Config#config.list_data, N).

add_data(Actor, X) ->
	Data = {erlang:localtime(), X},
	ETSData = Actor#config.list_data,
	?DLOG(actor_contract:get_id(Actor),{lists:concat(["Inserting data to", ETSData]), Data}),
	ets:insert(ETSData, Data),
%%	(ets:insert(ETSData, Data)=:= true) orelse ?DLOG("Insertion failed"),
	Actor.

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
	Var = ets:lookup(Opts, Key),
	Option = get_option_helper(Var, Key), 
	?DLOG(actor_contract:get_id(Actor),{lists:concat(["Elements value ", Key]), Option}),
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
	?DLOG(actor_contract:get_id(Actor),{lists:concat(["Inserting option to ", Opts]), {Key, Value}}),
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
	{ActorConfig, 
	{id, actor_contract:get_id(ActorConfig), status}, 
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
	{ok, F} = file:open("list_data.log", [append, delayed_write, unicode]),
	Fun = export_to(file),
	ets:foldl(Fun, F, TablePid),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {csv_export, list_data}) ->
	TablePid = ActorConfig#config.list_data,
	%% File creation
	{ok, F} = file:open("list_data.csv", [append, delayed_write, unicode]),
	Fun = export_to(file),
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
	Fun = export_to(file),
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
	
	
%% ===================================================================
%% Internal API
%% ===================================================================

export_to(file) ->
	%Fun = 
	fun(X, FileDescriptor) -> 
		R = io_lib:format("~w\n",[X]),
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


-ifdef(TEST).
%% ===================================================================
%% Tests
%% ===================================================================

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
		{Actor, {id, test, status}, supervisor},
		answer(Actor,{status, id})),
	?_assertEqual(
		{Actor, {work_time, 42, status}, supervisor},
		answer(Actor, {status, work_time})),
	?_assertEqual(
		{NewWTime, {work_time, 10, changed}, supervisor},
		answer(Actor, {change, work_time, 10})),
	% ?_assertEqual(
	% 	{Actor, {option, [1,3], status}, supervisor},
	% 	answer(Actor, {status, option, out})),
	?_assertEqual(
		{NewOpt, {option,{in,4}, added}, supervisor},
		answer(Actor, {add, option,{in,4}}))%,
	% ?_assertEqual(
	% 	{Actor, {list_data, [5,6], status}, supervisor},
	% 	answer(Actor, {status, list_data}))
	].

-endif.
