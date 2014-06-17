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
	{NewRFIDConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{RFIDConfig, ProductConfig}, 
		{ProductConfig, RFIDConfig}),
	% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, actor_contract:get_id(NewProductConfig)}, 
	anyone};

answer(RFIDConfig, Request) ->
	actor_contract:answer(RFIDConfig, Request).


%% Tests

answer_test_() ->
	
	ActorRFID = actor_rfid:create(),
	ActorProduct = actor_product:create(product_one),
	{RFIDResult, ProdResult}= actor_contract:add_to_list_data({ActorRFID, ActorProduct}, 
		{ActorProduct, ActorRFID}),	[
	?_assertEqual(
		{ActorRFID, {supervisor, pong}}, 
		answer(ActorRFID, {supervisor, ping})),
	?_assertEqual(
		{RFIDResult,{actor_product, ProdResult, product_one}, anyone},
	answer(ActorRFID, {actor_product, ActorProduct, id}))	].
