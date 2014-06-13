-module(actor_rfid).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2,
	create/0
	]).

%% Behavior implementation

answer(RFIDConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{RFIDConfig, answer, actor_contract:get_id(ProductConfig)};

answer(RFIDConfig, {supervisor, ping}) ->
	{RFIDConfig, answer, pong};

answer(_RFIDConfig, _) ->
	undefined.

create() ->
	actor_contract:create(?MODULE, rfid, 0).

%% Internal API

answer_test_() ->
	ActorRFID = actor_rfid:create(),
	ActorProduct = actor_product:create(product_one),
	[?_assertEqual(
		{ActorRFID, answer, product_one}, 
		answer(ActorRFID, {actor_product, ActorProduct})),
	?_assertEqual(
		{ActorRFID, answer, pong}, 
		{ActorRFID, answer, pong} = answer(ActorRFID, {supervisor, ping})),
	?_assert(answer(weird_message, hioho) =:= undefined)
	].
