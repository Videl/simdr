-module(actor_rfid).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(), [{capacity, 4}], undefined, 2, []).


answer(RFIDConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{NewRFIDConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{RFIDConfig, ProductConfig}, 
		{ProductConfig, RFIDConfig}),
	% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, actor_contract:get_id(NewProductConfig)}, 
	supervisor};
answer(RFIDConfig, Request) ->
	actor_contract:answer(RFIDConfig, Request).

%% Tests

answer_test_() ->
	ActorRFID = actor_rfid:create(),
	Id= actor_contract:get_id(ActorRFID),
	ActorProduct = actor_product:create(product_one, 6),
{RFID, _, supervisor}=answer(ActorRFID, {actor_product, ActorProduct}),
	[
	?_assertEqual(
		{ActorRFID, {supervisor, pong}}, 
		answer(ActorRFID, {supervisor, ping})),
	?_assertMatch(
	{config, actor_rfid, Id, [{capacity,4}], undefined, 2,[{ActorProduct, _}]},
	RFID)
].
