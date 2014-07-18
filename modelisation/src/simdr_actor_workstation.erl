-module(simdr_actor_workstation).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

%% External interface

-export([
	create/1,
	create/2
	]).

%% Behavior implementation

create() ->
	%%% module, state, work_time
	create(simdr_actor_contract:random_id(),{'Q1', 33} ).
create({Quality, Luck}) ->
	create(simdr_actor_contract:random_id(),{Quality, Luck});

create(Name ) ->
	create(Name,{'Q1', 33}).

create(Name, {Quality, Luck}) ->
	Ac1 = 	simdr_actor_contract:create(?MODULE, Name, [], off, 1, []),
	Ac2 = simdr_actor_contract:set_option(Ac1, workstation_luck, {Quality, Luck}),
	Ac2.

answer(WSConfig, {actor_product, ProductConfig}) ->
	simdr_actor_contract:work(simdr_actor_contract:get_work_time(WSConfig)),
	%%% Product transformation
	{NewProductConfig, Quality} = change_product(WSConfig, ProductConfig),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = simdr_actor_contract:add_to_list_data(
		WSConfig, {{changed,for, Quality,'of',product}, {ProductConfig}}, 
		NewProductConfig, {{quality,became,Quality,because,'of'},{WSConfig}}),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Quality}, 
	simdr_actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	simdr_actor_default:answer(WSConfig, Request).

%% Internal API

change_product(WSConfig, ProductConfig) ->
	[Transfo] = simdr_actor_contract:get_option(WSConfig, workstation_luck),
	simdr_actor_contract:set_option(ProductConfig, processed, Transfo),
	{simdr_actor_contract:set_state(ProductConfig, processed), Transfo}.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = simdr_actor_contract:set_work_time(simdr_actor_workstation:create(),1),
	ActorProductOne = simdr_actor_product:create(),
	{_, {actor_product, ActorProductTwo, _Quality}, _Destination} = 
		simdr_actor_workstation:answer(ActorWS, {actor_product, ActorProductOne}),
	[
		%%% Test: quality of a product is different
		?_assert(
			raw =/= simdr_actor_contract:get_state(ActorProductTwo)
		)
	].

change_product_test_() ->
	BaseWS = create({'Q3', 100}),
	BasePO = simdr_actor_product:create(),
	{_NewWS, {actor_product, NewPO, Quality}, _} = 
	 	answer(BaseWS, {actor_product, BasePO}),
	[
		?_assertMatch(
			[{'Q3', 100}], 
			simdr_actor_contract:get_option(NewPO, processed)),
		?_assertMatch(
			{'Q3', _},
			Quality)
	].

-endif.