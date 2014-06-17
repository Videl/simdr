-module(actor_railway).
-behaviour(actor_contract).
-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

%% External API

-export([
	create/1
	]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, random_id(),[], undefined, 0, []).

create(Id) ->
	actor_contract:create(?MODULE, Id, [], undefined, 0, []).

answer(RailwayConfig,{actor_product, ProductConfig, switch}) ->
	MesOut = case actor_contract:list_size(actor_contract:get_option(RailwayConfig, out)) of 
			1 ->{[no_prob_out], actor_contract:get_option(RailwayConfig,out)}; 
			_ ->{[prob_out], supervisor}
			end,
	MesIn = case actor_contract:list_size(actor_contract:get_option(RailwayConfig, in)) of 
	1 -> { Info, Rec} =MesOut,
		{[no_prob_in]++Info, Rec};
	_ -> { Info, _Rec} =MesOut,
		{[prob_in]++Info, supervisor}
			end,
	{InfoProb, Dest} = MesIn,

	case Dest =:= supervisor of
		true ->	{RailwayConfig, {actor_product, ProductConfig, InfoProb}, Dest};
		false -> {RailwayConf, ProductConf} = actor_contract:add_to_list_data({RailwayConfig, {ProductConfig,
					{ actor_contract:get_option(RailwayConfig, in),
					actor_contract:get_option(RailwayConfig, out)} }}, {ProductConfig, RailwayConfig}),
				{RailwayConf, {actor_product, ProductConf, InfoProb}, Dest}
	end;

answer(RailwayConfig, {supervisor, ProductConfig, Decision}) ->
	{_In, Out}= Decision,
	{Conf, Prod} = actor_contract:add_to_list_data({RailwayConfig, {ProductConfig, Decision}}, 
		{ProductConfig, RailwayConfig}),
	case Decision =/= actor_contract:get_state(Conf) of
		true ->	RailwayConf=actor_contract:set_state(Conf, Decision),
				actor_contract:work(actor_contract:get_work_time(RailwayConf)),
				{RailwayConf,{actor_product, Prod,switched}, Out};
		false -> {Conf,{actor_product, Prod,switched}, Out}
	end;
	
answer(RailwayConfig, Request) ->
	actor_contract:answer(RailwayConfig, Request).

%Internal API


random_id() ->
	random:uniform(1000).
%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->
	Rail = create(),
	Prod = actor_product:create(),
	NewRail = actor_contract:add_option(Rail, in, 1),
	NewRail1 = actor_contract:add_option(NewRail, out, 2),
	NewRail2 = actor_contract:add_option(NewRail1, in, 2),
	NewRail3 = actor_contract:add_option(NewRail2, out, 2),
	NewRail4 = actor_contract:add_option(NewRail1, out, 2),
	{NewRail1bis, Prodbis}= actor_contract:add_to_list_data({NewRail1, 
					{Prod,{1,2}}},{Prod,NewRail1}),
	RailwayConfig=actor_contract: set_state(NewRail2, {2,3}),
	{RailwayConf, Product}= actor_contract:add_to_list_data({RailwayConfig, 
					{Prod,{2,3}}},{Prod,RailwayConfig}),
	[?_assertEqual(
		{ NewRail2, {actor_product, Prod, [prob_in,no_prob_out]}, supervisor},
		answer(NewRail2, {actor_product, Prod, switch})),
	?_assertEqual(
		{ NewRail1bis, {actor_product, Prodbis, [no_prob_in,no_prob_out]}, actor_contract:get_option(RailwayConfig,out)},
		answer(NewRail1, {actor_product, Prod, switch})),
	?_assertEqual(
		{ NewRail3, {actor_product, Prod, [prob_in,prob_out]}, supervisor},
		answer(NewRail3, {actor_product, Prod, switch})),
	?_assertEqual(
		{ NewRail4, {actor_product, Prod, [no_prob_in,prob_out]}, supervisor},
		answer(NewRail4, {actor_product, Prod, switch})),
	?_assertEqual(
		{RailwayConf, {actor_product, Product, switched}, 3},
		answer(NewRail2, {supervisor, Prod, {2,3}})),
	?_assertEqual(
		{Rail, {supervisor, pong}}, 
		answer(Rail, {supervisor, ping}))
	].