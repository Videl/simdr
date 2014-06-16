-module(actor_rfid).
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
	actor_contract:create(?MODULE, rfid, 0).

answer(RFIDConfig, {actor_product, ProductConfig, id}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	% Answer
	{RFIDConfig, 
	{actor_product, ProductConfig, actor_contract:get_id(ProductConfig)}, 
	anyone};
answer(RFIDConfig, Request) ->
	actor_contract(RFIDConfig, Request).



%% Tests

answer_test_() ->
	ActorRFID = actor_rfid:create(),
	ActorProduct = actor_product:create(product_one),
	[?_assertEqual(
		{ActorRFID, answer, product_one}, 
		answer(ActorRFID, {actor_product, ActorProduct})),
	?_assertEqual(
		{ActorRFID, answer, pong}, 
		answer(ActorRFID, {supervisor, ping})),
	?_assert(answer(weird_message, hioho) =:= undefined)
	].
