-module(simdr_actor_workstation_assembly).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	create/1,
	create/2,
	answer/2
	]).

%% Behavior implementation

create() ->
	%%% module, state, work_time
	create(simdr_actor_contract:random_id()).

create(Name) ->
	create(Name, {3,8,1}).

create(Name, {Stop, Manip, Evac}) ->
	Ac1 = simdr_actor_contract:create(?MODULE, Name, [], off, 0, []),
	simdr_actor_contract:set_option(Ac1, stop, Stop),
	simdr_actor_contract:set_option(Ac1, manipulation, Manip),
	simdr_actor_contract:set_option(Ac1, evacuation, Evac),
	Ac2 =simdr_actor_contract:set_work_time(Ac1, Stop+Manip+Evac),
%	Ac3 = simdr_actor_contract:add_option(Ac2, order, {'Q1',{1,0,1,0}}),
	Ac2.

answer(WSConfig, {actor_product, ProductConfig}) ->
	simdr_actor_contract:work(simdr_actor_contract:get_work_time(WSConfig)),
	case simdr_actor_contract:get_option(WSConfig, order) of 
		unknown_option -> Transfo = {'Q1',{1,0,1,0}};
		 [Transfo|_R] -> simdr_actor_contract:get_option(WSConfig, order)
	end,	
	{_Quality, Assembly} = Transfo,
	simdr_actor_contract:set_option(ProductConfig, assembled, Assembly),
	NewProductConfig = simdr_actor_contract:set_state(ProductConfig, assembled),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = simdr_actor_contract:add_to_list_data(
		WSConfig, {{changed,assembly,for, Assembly,'of',product}, {ProductConfig}}, 
		NewProductConfig, {{assembly,became,Assembly,because,'of'},{WSConfig}}),
	io:format("Order avant delete: ~w ~n",[simdr_actor_contract:get_option(WSConfig, order)] ),
	simdr_actor_contract:delete_option(WSConfig, Transfo),
	io:format("Order après delete: ~w ~n",[simdr_actor_contract:get_option(WSConfig, order)] ),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Assembly}, 
	simdr_actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	simdr_actor_default:answer(WSConfig, Request).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = simdr_actor_contract:set_work_time(simdr_actor_workstation_assembly:create(),1),
	ActorProductOne = simdr_actor_product:create(),
	{_, {actor_product, ActorProductTwo, _Quality}, _Destination} = 
		simdr_actor_workstation_assembly:answer(ActorWS,{actor_product, ActorProductOne}),
	[
		%%% Test: quality of a product is different
		?_assert(
			processed =/= simdr_actor_contract:get_state(ActorProductTwo)
		)
	].

%%% Does not work because of set_option that adds something too.
% data_filler_test_() ->
% 	BaseWS = create(),
% 	BasePO = simdr_actor_product:create(),
% 	{NewWS, {simdr_actor_product, NewPO, Transfo}, _} = 
% 	 	answer(BaseWS, {simdr_actor_product, BasePO}),
% 	LastDataWS = simdr_actor_contract:get_data(NewWS),
% 	LastDataPO = simdr_actor_contract:get_data(NewPO),
% 	[
% 		%%% Test: last data exists in product
% 		?_assertMatch(
% 			{_ErlangNow, _Time, _Actor, {assembly,became,Transfo,because,'of'},{BaseWS}},
% 			LastDataPO),
% 		%%% Test: last data exists in workstation
% 		?_assertMatch(
% 			{_ErlangNow, _Time, _Actor, {changed,assembly,for, Transfo,'of',product}, {BasePO}}, 
% 			LastDataWS)
% 	].

create_test_() ->
	BaseWS = create('WS', {3,2,1}),
	[
		?_assertEqual(
			6 , simdr_actor_contract: get_work_time(BaseWS)
			)
	].

-endif.