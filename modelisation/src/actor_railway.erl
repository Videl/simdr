-module(actor_railway).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).


%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(),[], undefined, 0, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	MesOut = case actor_contract:list_size(actor_contract:get_out(RailwayConfig)) of 
		1 ->
			{no_prob_out, actor_contract:get_out(RailwayConfig)}; 
		_ ->
			{prob_out, supervisor}
		end,

	{InfoProb, Dest} = MesOut,

	case Dest =:= supervisor of
		true ->	
			{RailwayConfig, {actor_product, ProductConfig, InfoProb}, Dest};
		false -> 
			%{In, _Out}= actor_contract: get_in_out(RailwayConfig),
			{RailwayConf, ProductConf} = actor_contract:add_to_list_data(
				RailwayConfig, 
				{took,a,product, {ProductConfig}},
				ProductConfig, 
				{entered,railway, {RailwayConfig}}),
			{RailwayConf, {actor_product, ProductConf, InfoProb}, Dest}
	end;

answer(RailwayConfig, {prob_out, ProductConfig, Decision}) ->
	{In, _Out} = actor_contract:get_in_out(RailwayConfig),
	NewOut = Decision,
	{Conf, Prod} = {RailwayConfig, ProductConfig},
	% actor_contract:add_to_list_data(
	% 	{RailwayConfig, 
	% 		{ProductConfig, 
	% 		{[In],[NewOut]}}}, 
	% 	{ProductConfig, RailwayConfig}),
	case Decision =/= actor_contract:get_in_out(Conf) of
		true ->	
			RailwayConf = actor_contract:set_in_out(Conf, {In, NewOut}),
			actor_contract:work(actor_contract:get_work_time(RailwayConf)),
			{RailwayConf,{actor_product, Prod,switched}, NewOut};
		false -> 
			actor_contract:work(actor_contract:get_work_time(RailwayConfig)/2),
			{Conf,{actor_product, Prod,switched}, NewOut}
	end;
	
answer(RailwayConfig, Request) ->
	actor_contract:answer(RailwayConfig, Request).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

% answer_test_() ->
% 	Rail = create(),
% 	Prod = actor_product:create(),
% 	NewRail = actor_contract:add_option(Rail, in, 1),
% 	NewRail1 = actor_contract:add_option(NewRail, out, 2),
% 	NewRail2 = actor_contract:add_option(NewRail1, in, 2),
% 	NewRail3 = actor_contract:add_option(NewRail2, out, 2),
% 	NewRail4 = actor_contract:add_option(NewRail1, out, 2),
% 	{NewRail1bis, Prodbis}= actor_contract:add_to_list_data({NewRail1, 
% 					{Prod,{[1],[2]}}},{Prod,NewRail1}),
	
% 	{RailwayConf, Product}= actor_contract:add_to_list_data({NewRail2, 
% 					{Prod,{[2],[3]}}},{Prod,NewRail2}),
% 	RailwayConfig=actor_contract: set_state(RailwayConf, {2,3}),
% 	[
% 	% ?_assertEqual(
% 	% 	{ NewRail2, {actor_product, Prod, [prob_in,no_prob_out]}, supervisor},
% 	% 	answer(NewRail2, {actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{ NewRail1bis, {actor_product, Prodbis, [no_prob_in,no_prob_out]}, actor_contract:get_option(RailwayConfig,out)},
% 	% 	answer(NewRail1, {actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{ NewRail3, {actor_product, Prod, [prob_in,prob_out]}, supervisor},
% 	% 	answer(NewRail3, {actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{ NewRail4, {actor_product, Prod, [no_prob_in,prob_out]}, supervisor},
% 	% 	answer(NewRail4, {actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{RailwayConfig, {actor_product, Product, switched}, 3},
% 	% 	answer(NewRail2, {actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{Rail, {supervisor, pong}}, 
% 	% 	answer(Rail, {supervisor, ping}))
% 	].

-endif.