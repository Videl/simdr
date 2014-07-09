-module(actor_workstation_finish).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).


%% Behavior implementation

create() ->
	%%% module, state, work_time
	Ac1 = actor_contract:create(?MODULE, off, 10),
	Ac2 = actor_contract:set_capacity(Ac1, 1),
	Ac2.



answer(WSConfig, {actor_product, ProductConfig}) ->
case actor_contract:get_option(ProductConfig, quality) of 
	['Q1']-> Finish='Q1';
	['Q2']-> 	actor_contract:work(actor_contract:get_work_time(WSConfig)),
			actor_contract:set_option(ProductConfig, quality, {'Q1', pastille}),
			Finish={'Q1', pastille};
	['Q3']-> 	actor_contract:work(actor_contract:get_work_time(WSConfig)),
			actor_contract:set_option(ProductConfig, quality, {'Q2', pastille}),
			Finish={'Q2', pastille}
	end,
	NewProductConfig = actor_contract:set_state(ProductConfig, finished),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = actor_contract:add_to_list_data(
		WSConfig, {finish,'of',product, {ProductConfig, for, Finish}}, 
		NewProductConfig, {quality,became,Finish,because,'of',{WSConfig}}),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Finish}, 
	actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	actor_contract:answer(WSConfig, Request).


% ===================================================================
% Tests
% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation_finish:create(),1),
	ActorProductOne = actor_product:create(),
	ProductConf =actor_contract:set_option(ActorProductOne, quality, 'Q2'),
	{_, {actor_product, ActorProductTwo, Quality}, _Destination} = 
		answer(ActorWS, {actor_product, ProductConf}),
	[
		%%% Test: quality of a product is different
		?_assert(
			assembled =/= actor_contract:get_state(ActorProductTwo)
		),
		?_assertEqual(
			{'Q1', pastille},Quality
		)
	].

data_filler_test_() ->
	BaseWS = create(),
	BasePO = actor_product:create(),
	ProductConf =actor_contract:set_option(BasePO, quality, 'Q2'),
	{NewWS, {actor_product, NewPO, Finish}, _} = 
	 	answer(BaseWS, {actor_product, ProductConf}),
	LastDataWS = actor_contract:get_data(NewWS),
	LastDataPO = actor_contract:get_data(NewPO),
	[
		%%% Test: last data exists in product
		?_assertMatch(
			{_ErlangNow, _Time, {quality,became,Finish,because,'of',{BaseWS}}},
			LastDataPO),
		%%% Test: last data exists in workstation
		?_assertMatch(
			{_ErlangNow, _Time, {finish,'of',product, {BasePO, for, Finish}}}, 
			LastDataWS)
	].

-endif.