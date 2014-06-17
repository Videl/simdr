-module(actor_railway).
-behaviour(actor_contract).
-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).
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

case Dest =/= supervisor of
		true -> {RailwayConfig, {actor_product, ProductConfig, InfoProb}, Dest};
		false -> actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
				{RailwayConfig, {actor_product, ProductConfig, InfoProb}, Dest}
end;

answer(RailwayConfig, {supervisor, ProductConfig, Decision}) ->
	RailwayConf=actor_contract: set_state(RailwayConfig, Decision),
	{_In, Out}= Decision,
	actor_contract:work(actor_contract:get_work_time(RailwayConf)),
	{RailwayConf,{actor_product, ProductConfig,switched}, Out};

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
	RailwayConfig=actor_contract: set_state(NewRail2, {2,3}),
	[?_assertEqual(
		{ NewRail2, {actor_product, Prod, [prob_in,no_prob_out]}, supervisor},
		answer(NewRail2, {actor_product, Prod, switch})),
	?_assertEqual(
		{ NewRail1, {actor_product, Prod, [no_prob_in,no_prob_out]}, actor_contract:get_option(RailwayConfig,out)},
		answer(NewRail1, {actor_product, Prod, switch})),
	?_assertEqual(
		{ NewRail3, {actor_product, Prod, [prob_in,prob_out]}, supervisor},
		answer(NewRail3, {actor_product, Prod, switch})),
	?_assertEqual(
		{ NewRail4, {actor_product, Prod, [no_prob_in,prob_out]}, supervisor},
		answer(NewRail4, {actor_product, Prod, switch})),
	?_assertEqual(
		{RailwayConfig, {actor_product, Prod, switched}, 3},
		answer(NewRail2, {supervisor, Prod, {2,3}})),
	?_assertEqual(
		{Rail, {supervisor, pong}}, 
		answer(Rail, {supervisor, ping}))
	].