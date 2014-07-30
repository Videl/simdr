%%% @doc Actor Workstation
%%% 
%%% This module provides you with a Workstation Actor. 
%%%
%%% When the actor simdr_actor_product is in this actor, the workstation
%%% will write in the product the configuration of the workstation.
%%% When this product will enter a scanner, the product will have its quality
%%% computed. Change the option `workstation_luck' for the workstation's
%%% configuration.
%%% 
%%% Suitable options: capacity = limited, only one actor entering in this actor,
%%% only one actor exiting this actor. (One actor in `in'/`out'.)
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
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

create(Name) ->
	create(Name,{'Q1', 33}).

create(Name, {Quality, Luck}) ->
	Ac1 = simdr_actor_contract:create(?MODULE, Name, [], off, 1, []),
	Ac2 = simdr_actor_contract:set_option(Ac1, workstation_luck, {Quality, Luck}),
	Ac2.

answer(WSConfig, {actor_product, ProductConfig}) ->
	simdr_actor_contract:work(WSConfig),
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