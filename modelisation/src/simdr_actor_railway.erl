%%% @doc Actor Railway
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
-module(simdr_actor_railway).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).


%% simdr_Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

%% User functions exports

-export([
	create/1
	]).

%% Spawn exports

-export([
	send_scanner/2
	]).

%% Behavior implementation

create() ->
	create( simdr_actor_contract:random_id()).

create(Name) ->
	simdr_actor_contract:create(?MODULE, Name, [], off, 5, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	Supervisor = simdr_actor_contract:get_option(RailwayConfig, supervisor) ,
	spawn(?MODULE, send_scanner, [RailwayConfig, ProductConfig]),
%%	io:format (" Nombre de sorties : ~w~n", [simdr_actor_contract:list_size(simdr_actor_contract:get_out(RailwayConfig))]),
	{RailwayConf, ProductConf} = simdr_actor_contract:add_to_list_data(
		RailwayConfig, 
		{{took,a,product}, {ProductConfig}},
		ProductConfig, 
		{{entered,railway}, {RailwayConfig}}),
	MesOut = case simdr_actor_contract:list_size(simdr_actor_contract:get_out(RailwayConfig)) of 
				1 ->
					{no_prob_out, simdr_actor_contract:get_out(RailwayConfig)}; 
				_ ->
					{prob_out, Supervisor}
			 end,

	{InfoProb, Dest} = MesOut,

	case Dest =:= Supervisor of
		true ->	
			{RailwayConfig, {actor_product, ProductConfig, InfoProb}, Dest};
		false -> 
			%{In, _Out}= simdr_actor_contract: get_in_out(RailwayConfig),
			{RailwayConf, {actor_product, ProductConf, InfoProb}, Dest}
	end;
answer(RailwayConfig, {prob_out, ProductConfig, Decision}) ->
	{In, Out} = simdr_actor_contract:get_in_out(RailwayConfig),
	NewOut = Decision,
	%%% List data fillers
	{ActorConfig, Prod} = simdr_actor_contract:add_to_list_data(
		RailwayConfig, 
		{{going, into, position, {In, NewOut}, for}, ProductConfig}, 
		ProductConfig, 
		{{railway, went, into, position, {In, NewOut}}, RailwayConfig}),
	%%% Answer
	case Decision =/= Out of
		true ->	
			RailwayConf = simdr_actor_contract:set_in_out(ActorConfig, {In, NewOut}),
			simdr_actor_contract:work(RailwayConf),
			{RailwayConf, {actor_product, Prod, switched}, NewOut};
		false -> 
			Time = simdr_actor_contract:get_work_time(ActorConfig)/3,
			simdr_actor_contract:work(
				Time,
				simdr_actor_contract:get_mode(ActorConfig)
				),
			{ActorConfig, {actor_product, Prod, switched}, NewOut}
	end;
answer(RailwayConfig, Request) ->
	simdr_actor_default:answer(RailwayConfig, Request).

%% Internal API

send_scanner(Conf, ProdConf) ->
	?DFORMAT("Railway: option : ~w ~n", [simdr_actor_contract:get_option(Conf, scanner)]),
	case simdr_actor_contract:get_option(Conf, scanner) of 
		[SCANNER] -> SCANNER ! {self(), {actor_product, ProdConf}};
		_ -> {nothing}
	end.


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

% answer_test_() ->
% 	Rail = create(),
% 	Prod = simdr_actor_product:create(),
% 	NewRail = simdr_actor_contract:add_option(Rail, in, 1),
% 	NewRail1 = simdr_actor_contract:add_option(NewRail, out, 2),
% 	NewRail2 = simdr_actor_contract:add_option(NewRail1, in, 2),
% 	NewRail3 = simdr_actor_contract:add_option(NewRail2, out, 2),
% 	NewRail4 = simdr_actor_contract:add_option(NewRail1, out, 2),
% 	{NewRail1bis, Prodbis}= simdr_actor_contract:add_to_list_data({NewRail1, 
% 					{Prod,{[1],[2]}}},{Prod,NewRail1}),
	
% 	{RailwayConf, Product}= simdr_actor_contract:add_to_list_data({NewRail2, 
% 					{Prod,{[2],[3]}}},{Prod,NewRail2}),
% 	RailwayConfig=simdr_actor_contract: set_state(RailwayConf, {2,3}),
% 	[
% 	% ?_assertEqual(
% 	% 	{ NewRail2, {simdr_actor_product, Prod, [prob_in,no_prob_out]}, supervisor},
% 	% 	answer(NewRail2, {simdr_actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{ NewRail1bis, {simdr_actor_product, Prodbis, [no_prob_in,no_prob_out]}, simdr_actor_contract:get_option(RailwayConfig,out)},
% 	% 	answer(NewRail1, {simdr_actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{ NewRail3, {simdr_actor_product, Prod, [prob_in,prob_out]}, supervisor},
% 	% 	answer(NewRail3, {simdr_actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{ NewRail4, {simdr_actor_product, Prod, [no_prob_in,prob_out]}, supervisor},
% 	% 	answer(NewRail4, {simdr_actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{RailwayConfig, {simdr_actor_product, Product, switched}, 3},
% 	% 	answer(NewRail2, {simdr_actor_product, Prod})),
% 	% ?_assertEqual(
% 	% 	{Rail, {supervisor, pong}}, 
% 	% 	answer(Rail, {supervisor, ping}))
% 	].

-endif.