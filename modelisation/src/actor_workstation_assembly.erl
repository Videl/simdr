-module(actor_workstation_assembly).
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

%% External interface

-export([
	create/1
	]).

%% Behavior implementation

create() ->
	%%% module, state, work_time
	Ac1 = actor_contract:create(?MODULE, off, 10),
	Ac2 = actor_contract:set_capacity(Ac1, 1),
	Ac3 = actor_contract:add_option(Ac2, order, {1,0,1,0}),
	Ac3.

create({Stop, Manip, Evac}) ->
	Ac1 = create(),
	actor_contract:set_option(Ac1, stop, Stop),
	actor_contract:set_option(Ac1, manipulation, Manip),
	actor_contract:set_option(Ac1, evacuation, Evac),
	Ac2 =actor_contract:set_work_time(Ac1, Stop+Manip+Evac),
	Ac3 = actor_contract:add_option(Ac2, order, {1,0,1,0}),
	Ac3.

answer(WSConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(WSConfig)),
	[Transfo] = actor_contract:get_option(WSConfig, order),
	actor_contract:set_option(ProductConfig, assembled, Transfo),
	NewProductConfig = actor_contract:set_state(ProductConfig, assembled),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = actor_contract:add_to_list_data(
		WSConfig, {changed,assembly,'of',product, {ProductConfig, for, Transfo}}, 
		NewProductConfig, {assembly,became,Transfo,because,'of',{WSConfig}}),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Transfo}, 
	actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	actor_contract:answer(WSConfig, Request).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation_assembly:create(),1),
	ActorProductOne = actor_product:create(),
	{_, {actor_product, ActorProductTwo, _Quality}, _Destination} = 
		actor_workstation_assembly:answer(ActorWS, {actor_product, ActorProductOne}),
	[
		%%% Test: quality of a product is different
		?_assert(
			processed =/= actor_contract:get_state(ActorProductTwo)
		)
	].

data_filler_test_() ->
	BaseWS = create(),
	BasePO = actor_product:create(),
	{NewWS, {actor_product, NewPO, Transfo}, _} = 
	 	answer(BaseWS, {actor_product, BasePO}),
	LastDataWS = actor_contract:get_data(NewWS),
	LastDataPO = actor_contract:get_data(NewPO),
	[
		%%% Test: last data exists in product
		?_assertMatch(
			{_ErlangNow, _Time, {assembly,became,Transfo,because,'of',{BaseWS}}},
			LastDataPO),
		%%% Test: last data exists in workstation
		?_assertMatch(
			{_ErlangNow, _Time, {changed,assembly,'of',product, {BasePO, for, Transfo}}}, 
			LastDataWS)
	].

create_test_() ->
	BaseWS = create({3,2,1}),
	[
		?_assertEqual(
			6 , actor_contract: get_work_time(BaseWS)
			)
	].

-endif.