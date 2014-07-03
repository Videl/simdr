-module(actor_workstation).
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
	actor_contract:work(actor_contract:get_work_time(WSConfig)),
	{NewProductConfig, Quality} = change_product(ProductConfig),
	% List data fillers
	{NewWSConfig, NewProductConfigBis} = actor_contract:add_to_list_data(
		WSConfig, {changed,quality,'of',product, {ProductConfig, for, Quality}}, 
		NewProductConfig, {quality,became,Quality,because,'of',{WSConfig}}),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Quality}, 
	actor_contract:get_out(NewWSConfig)};
	
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

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation:create(),1),
	ActorProductOne = actor_product:create(product_one),
	{_, {actor_product, ActorProductTwo, Quality}, _Destination} = 
		actor_workstation:answer(ActorWS, {actor_product, ActorProductOne}),
	[
		%%% Test: quality of a product is different
		?_assert(
			raw =/= actor_contract:get_state(ActorProductTwo)
		),
		%%% Test: quality of product is really the one said it is
		?_assertMatch(
			Quality,
			actor_contract:get_state(ActorProductTwo)
		)
	].

data_filler_test_() ->
	BaseWS = create(),
	BasePO = actor_product:create(),
	{NewWS, {actor_product, NewPO, Quality}, _} = 
	 	answer(BaseWS, {actor_product, BasePO}),
	LastDataWS = actor_contract:get_data(NewWS),
	LastDataPO = actor_contract:get_data(NewPO),
	[
		%%% Test: last data exists in product
		?_assertMatch(
			{_ErlangNow, _Time, {quality,became,Quality,because,'of',{BaseWS}}},
			LastDataPO),
		%%% Test: last data exists in workstation
		?_assertMatch(
			{_ErlangNow, _Time, {changed,quality,'of',product, {BasePO, for, Quality}}}, 
			LastDataWS)
	].

-endif.