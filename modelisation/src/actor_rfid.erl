-module(actor_rfid).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	create/1,
	answer/2]).

%% Behavior implementation

create() ->
	create(actor_contract:random_id()).
create(Name) ->
	actor_contract:create(?MODULE, Name, [], off, 2, []).


answer(RFIDConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{NewRFIDConfig, NewProductConfig} = actor_contract:add_to_list_data(
		RFIDConfig, {{scanned,product}, {ProductConfig}}, 
		ProductConfig, {{was,scanned,by},{RFIDConfig}}),
	%%% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, actor_contract:get_name(NewProductConfig)}, 
	supervisor};
answer(RFIDConfig, Request) ->
	actor_contract:answer(RFIDConfig, Request).

%% Tests
-ifdef(TEST).

answer_test_() ->
	ActorRFID = actor_rfid:create(),
	ActorProduct = actor_product:create(),
	Name = actor_contract:get_name(ActorProduct),
	{_, {actor_product, EndProduct, NewName}, _Dest} = 
		actor_rfid:answer(ActorRFID, {actor_product, ActorProduct}),
	[
		%%% Test: The product does not change before/after the answer/2.
		?_assertEqual(
			ActorProduct,
			EndProduct),
		%%% Test: the ID is in the 'special place'.
		?_assertEqual(
			Name,
			NewName)
	].

-endif.
