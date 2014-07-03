-module(actor_rfid).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(), [{capacity, 4}], undefined, 2, []).


answer(RFIDConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{NewRFIDConfig, NewProductConfig} = {RFIDConfig, ProductConfig},
	% actor_contract:add_to_list_data(
	% 	{RFIDConfig, ProductConfig}, 
	% 	{ProductConfig, RFIDConfig}),
	% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, actor_contract:get_id(NewProductConfig)}, 
	supervisor};
answer(RFIDConfig, Request) ->
	actor_contract:answer(RFIDConfig, Request).

%% Tests
-ifdef(TEST).

answer_test_() ->
	% ActorRFID = actor_rfid:create(),
	% Id= actor_contract:get_id(ActorRFID),
	% ActorProduct = actor_product:create(product_one, 6),
	% {RFID, _, supervisor}=answer(ActorRFID, {actor_product, ActorProduct}),
	[
	% ?_assertEqual(
	% 	{ActorRFID, {supervisor, pong}}, 
	% 	answer(ActorRFID, {supervisor, ping})),
	% ?_assertMatch(
	% {config, actor_rfid, Id, [{ets, _}, {capacity,4}, {awaiting, 0}], undefined, 2,[{ActorProduct, _}]},
	% RFID)
	].

-endif.
