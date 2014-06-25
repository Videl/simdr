-module(actor_workstation).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2,
	idling/1,
	processing/2
	]).

-export([
	wait/3,
	worker_loop/3	
	]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(), [{capacity, 1}], off, 10, []).

answer(WSConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(WSConfig)),
	{NewProductConfig, Quality} = change_product(ProductConfig),
	% List data fillers
	{NewWSConfig, NewProductConfigBis} = actor_contract:add_to_list_data(
		{WSConfig, {NewProductConfig, Quality}}, 
		{NewProductConfig, {workstation, WSConfig}}),
	% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Quality}, 
	get_destination(NewWSConfig)};
	
answer(WSConfig, Request) ->
	actor_contract:answer(WSConfig, Request).

%% Internal API

change_product(ProductConfig) ->
	Result = case random:uniform(3) of
		1 -> % Good quality
			{actor_contract:set_state(ProductConfig, 'Q1'), 'Q1'};
		2 -> % Medium quality
			{actor_contract:set_state(ProductConfig, 'Q2'), 'Q2'};
		3 -> % Bad quality
			{actor_contract:set_state(ProductConfig, 'Q3'), 'Q3'}
	end,
	Result.

get_destination(Config) ->
	ListOfOuts = actor_contract:get_option(Config, out),
	Out = case actor_contract:list_size(ListOfOuts) of
		1 ->
			ListOfOuts;
		_ ->
			supervisor
	end,
	Out.


idling(Config) ->
 	actor_contract:idling(Config).

processing(Config, NbWorker) ->
	receive
		{Sender, {actor_product, ProdConf}} ->
			Request = {actor_product,ProdConf},
			[N] = actor_contract:get_option(Config, capacity),
			case NbWorker+1 > N of
				false ->
					io:format("Workstation > Starting to work on ~w~n", [ProdConf]),
					Sender ! {self(), {control, acknowledged, actor_product}},
					spawn(?MODULE, worker_loop, [self(), Config, Request]),
					?MODULE:processing(
						actor_contract:set_state(Config, processing), 
						NbWorker+1);

				_ -> 
					MD = {actor_contract:get_work_time(Config), Request},
					io:format("Workstation > No place.~n"),
					Sender ! {self(), {control, full, MD}},
					?MODULE:processing(Config, NbWorker)
			end;
		{Sender, {control, full, {Wait_time, Request}}} ->
			spawn(?MODULE, wait, [self(), Wait_time, {Request, Sender}]),
			?MODULE:processing(Config, NbWorker);

		{Sender, Request} ->
			A =	?MODULE:answer(Config, Request),
			send_message({A,Sender}),
			?MODULE:processing(Config, NbWorker);

		{_Worker, end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
		
			send_message({LittleAnswer, Destination}),
			?MODULE:processing(
				actor_contract:set_state(NewConfig, work), 
				NbWorker-1);
		_ ->
			?MODULE:processing(Config, NbWorker)
	end.


send_message({Ans, Dest}) when is_pid(Dest) -> 
	Dest ! {self(), {Ans}};
send_message({Ans, Dest}) ->
	%% @TODO: decider de la destination
	io:format("Sending: ~w to ~w.~n", [Ans, Dest]).

wait(Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};

wait(_Pid ,Wait_time, {Ans, Dest}) ->
	actor_contract:work(Wait_time),
	io:format("Sending: ~w to ~w.~n", [Ans, Dest]).

worker_loop(Master, MasterConfig, Request) ->
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {self(), end_of_work, FullAnswer}.


%% Tests

workstation_answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation:create(),1),
	ActorProductOne = actor_product:create(product_one),
	{NewActor, _, 20} = answer(ActorWS, {change, work_time, 20}),
	{_, {actor_product, ActorProductTwo, _Quality}, _Destination} = 
		answer(ActorWS, {actor_product, ActorProductOne}),
	[
	% ?_assertEqual(
	% 	{ActorWS, {supervisor, pong}}, 
	% 	answer(ActorWS, {supervisor, ping})),
	?_assertEqual(
		20, 
		actor_contract:get_work_time(NewActor)),
	?_assert(
		raw =/= actor_contract:get_state(ActorProductTwo)
		)
	].

get_destination_test_() ->
	WorkerConfFewOut = actor_contract:add_option(
		create(), 
		out, 
		test1),
	WorkerConfManyOut = actor_contract:add_option(
		WorkerConfFewOut, 
		out, 
		test2),
	WorkerConfManyOutBis = actor_contract:add_option(
		WorkerConfManyOut, 
		out, 
		test3),
	[
	?_assertEqual([test1], get_destination(WorkerConfFewOut)),
	?_assertEqual(supervisor, get_destination(WorkerConfManyOut)),
	?_assertEqual(supervisor, get_destination(WorkerConfManyOutBis)),
	?_assertEqual(supervisor, get_destination(create()))
	].

data_filler_test_() ->
	BaseWS = actor_contract:set_work_time(actor_workstation:create(),1),
	BasePO = actor_product:create(product_one),
	{NewWS, {_, NewPO, Quality}, _} = 
		answer(BaseWS, {actor_product, BasePO}),
	MockProduct = actor_contract:set_state(BasePO, Quality),
	LastDataWS = actor_contract:get_data(NewWS),
	LastDataPO = actor_contract:get_data(NewPO),
	[
	?_assertEqual({MockProduct, Quality}, LastDataWS),
	?_assertEqual({workstation, BaseWS}, LastDataPO)
	].
