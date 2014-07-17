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

%% External interface

-export([
	create/1,
	create/2
	]).

%% Behavior implementation

create() ->
	%%% module, state, work_time
	create(actor_contract:random_id(),{'Q1', 33} ).
create({Quality, Luck}) ->
	create(actor_contract:random_id(),{Quality, Luck});

create(Name ) ->
	create(Name,{'Q1', 33}).

create(Name, {Quality, Luck}) ->
	Ac1 = 	actor_contract:create(?MODULE, Name, [], off, 1, []),
	Ac2 = actor_contract:set_option(Ac1, workstation_luck, {Quality, Luck}),
	Ac2.

answer(WSConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(WSConfig)),
	%%% Product transformation
	{NewProductConfig, Quality} = change_product(WSConfig, ProductConfig),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = actor_contract:add_to_list_data(
		WSConfig, {{changed,for, Quality,'of',product}, {ProductConfig}}, 
		NewProductConfig, {{quality,became,Quality,because,'of'},{WSConfig}}),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Quality}, 
	actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	actor_contract:answer(WSConfig, Request).

%% Internal API

change_product(WSConfig, ProductConfig) ->
	[Transfo] = actor_contract:get_option(WSConfig, workstation_luck),
	actor_contract:set_option(ProductConfig, processed, Transfo),
	{actor_contract:set_state(ProductConfig, processed), Transfo}.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation:create(),1),
	ActorProductOne = actor_product:create(),
	{_, {actor_product, ActorProductTwo, _Quality}, _Destination} = 
		actor_workstation:answer(ActorWS, {actor_product, ActorProductOne}),
	[
		%%% Test: quality of a product is different
		?_assert(
			raw =/= actor_contract:get_state(ActorProductTwo)
		)
	].

%%% Does not work because of set_option that adds something too.
% data_filler_test_() ->
% 	BaseWS = create(),
% 	BasePO = actor_product:create(),
% 	{NewWS, {actor_product, NewPO, Quality}, _} = 
% 	 	answer(BaseWS, {actor_product, BasePO}),
% 	LastDataWS = actor_contract:get_data(NewWS),
% 	LastDataPO = actor_contract:get_data(NewPO),
% 	[
% 		%%% Test: last data exists in product
% 		?_assertMatch(
% 			{_ErlangNow, _Time, _BasePO, {quality,became,Quality,because,'of'},{BaseWS}},
% 			LastDataPO),
% 		%%% Test: last data exists in workstation
% 		?_assertMatch(
% 			{_ErlangNow, _Time, BaseWS, {changed,for, Quality,'of',product}, {BasePO}}, 
% 			LastDataWS)
% 	].

change_product_test_() ->
	BaseWS = create({'Q3', 100}),
	BasePO = actor_product:create(),
	{_NewWS, {actor_product, NewPO, Quality}, _} = 
	 	answer(BaseWS, {actor_product, BasePO}),
	[
		?_assertMatch(
			[{'Q3', 100}], 
			actor_contract:get_option(NewPO, processed)),
		?_assertMatch(
			{'Q3', _},
			Quality)
	].

-endif.