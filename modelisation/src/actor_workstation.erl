-module(actor_workstation).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, work_one, 5).

answer(WSConfig, {actor_product, ProductConfig, transformation}) ->
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

%% Tests
workstation_answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation:create(),1),
	ActorProductOne = actor_product:create(product_one),
	{_, _, NewActor} = answer(ActorWS, {supervisor, work_time, 20}),
	{_, {actor_product, ActorProductTwo, _Quality}, _Destination} = 
		answer(ActorWS, {actor_product, ActorProductOne, transformation}),
	[
	?_assertEqual(
		{ActorWS, {supervisor, pong}}, 
		answer(ActorWS, {supervisor, ping})),
	?_assertEqual(
		20, 
		actor_contract:get_work_time(NewActor)),
	?_assert(
		raw =/= actor_contract:get_state(ActorProductTwo)
		)
	].

get_destination_test_() ->
	WorkerConfFewOut = actor_contract:add_option(create(), out, test1),
	WorkerConfManyOut = actor_contract:add_option(WorkerConfFewOut, out, test2),
	WorkerConfManyOutBis = actor_contract:add_option(WorkerConfManyOut, out, test3),
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
		answer(BaseWS, {actor_product, BasePO, transformation}),
	MockProduct = actor_contract:set_state(BasePO, Quality),
	LastDataWS = actor_contract:get_data(NewWS),
	LastDataPO = actor_contract:get_data(NewPO),
	[
	?_assertEqual({MockProduct, Quality}, LastDataWS),
	?_assertEqual({workstation, BaseWS}, LastDataPO)
	].
