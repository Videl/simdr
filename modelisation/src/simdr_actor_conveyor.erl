%%% @doc Actor Conveyor
%%% 
%%% This module provides you with a Conveyor Actor. It mainly moves actor
%%% from two ends. Its only purpose is to delay a product in real-time.
%%% 
%%% Suitable options: capacity = infinity, only one actor entering in this actor,
%%% only one other actor exiting this actor. (One actor in `in'/`out' field.)
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
-module(simdr_actor_conveyor).
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

%% User functions export

-export([
	create/1,
	create/3
	]).

%% Export for spawns

-export([
	send_rfid/2
	]).

%% Behavior implementation

create() ->
	create(simdr_actor_contract:random_id()).
	
create(Name) ->
	create(Name, 1,1).

create(Name, Speed, Distance) ->
		Actor= simdr_actor_contract:create(?MODULE, Name, [], off, Distance/Speed, []),
		simdr_actor_contract:set_option(Actor, distance, Distance),
		simdr_actor_contract: set_option(Actor, speed, Speed),
		Actor.


answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	spawn(?MODULE, send_rfid, [ConveyorConfig, ProductConfig]),
	simdr_actor_contract:work(ConveyorConfig),
	{NewConveyorConfig, NewProductConfig} = simdr_actor_contract:add_to_list_data(
		ConveyorConfig, 
		{{moved, simdr_actor_contract:get_module(ProductConfig)}, {ProductConfig}}, 
		ProductConfig, 
		{{was,moved,by,simdr_actor_contract:get_module(ConveyorConfig)}, {ConveyorConfig}}),
	%%% Answer
	Destination = simdr_actor_contract:get_out(ConveyorConfig),
	{NewConveyorConfig, {actor_product, NewProductConfig, Destination}, Destination};
answer(ConveyorConfig, {change, distance, N}) ->
	simdr_actor_contract:set_option(ConveyorConfig, distance, N),
	Speed = simdr_actor_contract:get_option(ConveyorConfig,speed),
	NewConfig = simdr_actor_contract:set_work_time(ConveyorConfig, N/Speed),
	{NewConfig, {distance, N, changed}, supervisor};
answer(ConveyorConfig, {change, speed, N}) ->
	simdr_actor_contract: set_option(ConveyorConfig, speed, N),
	Distance = simdr_actor_contract:get_option(ConveyorConfig,distance),
	NewConfig = simdr_actor_contract:set_work_time(ConveyorConfig, Distance/N),
	{NewConfig, {speed, N, changed}, supervisor};
answer(ConveyorConfig, {status, distance}) ->
	[Distance] = simdr_actor_contract:get_option(ConveyorConfig,distance),
	{ConveyorConfig, {distance, Distance, status}, supervisor};
answer(ConveyorConfig, {status, speed}) ->
	[Speed] = simdr_actor_contract:get_option(ConveyorConfig,speed),
	{ConveyorConfig, {speed, Speed , status}, 	supervisor};
answer(ConveyorConfig, Request) ->
	simdr_actor_default:answer(ConveyorConfig, Request).

%% Internal API

send_rfid(Conf, ProdConf) ->
	case simdr_actor_contract:get_option(Conf, rfid) of 
		[RFID] -> RFID ! {self(), {actor_product, ProdConf}};
		_ -> {nothing}
	end.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	Conv = create('C1', 4, 6),
	Prod = simdr_actor_product:create(),
	{_, {actor_product, ProdTwo, Destination}, Destination} = answer(Conv, {actor_product, Prod}),
	[
		%%% Test: product does not change
		?_assertEqual(
			Prod,
			ProdTwo),
		?_assertEqual({ Conv, {speed, 4, status},supervisor}, answer(Conv, {status, speed})),
		?_assertEqual({ Conv, {work_time, 1.5, status},supervisor}, answer(Conv, {status, work_time}))
	].

-endif.