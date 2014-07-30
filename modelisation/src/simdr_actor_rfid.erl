%%% @doc Actor RFID
%%% 
%%% This module provides you with an RFID Actor. It is meant to be used as an
%%% add-on on some actors. Just configure the actor like so:
%%% `Actor ! {self(), {add, option, {rfid, RFIDActor}}}.'
%%%
%%% Right now, no supervisor uses its data.
%%% 
%%% Suitable options: capacity = infinity, only one actor entering in this actor,
%%% zero actor exiting this actor. (One actor in `in'.)
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
-module(simdr_actor_rfid).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

%% simdr_Actor Contract Behaviors Callbacks

-export([
	create/0,
	create/1,
	answer/2]).

%% Behavior implementation

create() ->
	create(simdr_actor_contract:random_id()).
create(Name) ->
	simdr_actor_contract:create(?MODULE, Name, [], off, 2, []).


answer(RFIDConfig, {actor_product, ProductConfig}) ->
	simdr_actor_contract:work(RFIDConfig),
	{NewRFIDConfig, NewProductConfig} = simdr_actor_contract:add_to_list_data(
		RFIDConfig, {{scanned,product}, {ProductConfig}}, 
		ProductConfig, {{was,scanned,by},{RFIDConfig}}),
	%%% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, simdr_actor_contract:get_name(NewProductConfig)}, 
	supervisor};
answer(RFIDConfig, Request) ->
	simdr_actor_default:answer(RFIDConfig, Request).

%% Tests
-ifdef(TEST).

answer_test_() ->
	ActorRFID = simdr_actor_rfid:create(),
	ActorProduct = simdr_actor_product:create(),
	Name = simdr_actor_contract:get_name(ActorProduct),
	{_, {actor_product, EndProduct, NewName}, _Dest} = 
		simdr_actor_rfid:answer(ActorRFID, {actor_product, ActorProduct}),
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
