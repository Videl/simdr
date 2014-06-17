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
	% Answer
	{WSConfig, 
	{actor_product, NewProductConfig, Quality}, 
	get_destination(WSConfig)};
	
answer(WSConfig, Request) ->
	actor_contract:answer(WSConfig, Request).

%% Internal API

change_product(ProductConfig) ->
	Result = case random:uniform(3) of
		1 -> % Good quality
			{actor_contract:set_state(ProductConfig, "Q1"), "Q1"};
		2 -> % Medium quality
			{actor_contract:set_state(ProductConfig, "Q2"), "Q2"};
		3 -> % Bad quality
			{actor_contract:set_state(ProductConfig, "Q3"), "Q3"}
	end,
	Result.

get_destination(Config) ->
	ListOfOuts = actor_contract:get_option(Config, out),
	case actor_contract:list_size(ListOfOuts) of
		1 ->
			[Out] = ListOfOuts;
		_ ->
			Out = supervisor
	end,
	Out.

%% Tests
workstation_answer_test_() ->
	ActorWS = actor_workstation:create(),
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
	WorkerConfManyOut2 = actor_contract:add_option(WorkerConfFewOut, out, test2),
	WorkerConfManyOut = actor_contract:add_option(WorkerConfManyOut2, out, test2),
	[
	?_assertEqual(test1, get_destination(WorkerConfFewOut)),
	?_assertEqual(supervisor, get_destination(WorkerConfManyOut)),
	?_assertEqual(supervisor, get_destination(WorkerConfManyOut))
	].