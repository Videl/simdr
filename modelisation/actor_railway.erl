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

answer(RailwayConfig,{next, ProductConfig}) ->
MesA = case actor_contract:list_size(actor_contract:get_option(RailwayConfig, in)) of 
	1 ->{no_change};
	_ ->{ProductConfig, {supervisor, RailwayConfig}}
end,

MesB = case actor_contract:list_size(actor_contract:get_option(RailwayConfig, out)) of 
	1 ->{ProductConfig, actor_contract:get_option(RailwayConfig,out)}; 
	_ ->{ProductConfig, {supervisor, RailwayConfig}}
end,
{MesA,MesB};

answer(RailwayConfig, {supervisor, state}) ->
	{answer, state, actor_contract:get_state(RailwayConfig)};

answer(ProductConfig, {supervisor, RailwayConfig, Decision}) ->
	actor_contract: set_state(RailwayConfig, Decision),
	{_In, Out}= Decision,
	actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
	{ProductConfig, Out}.

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
	NewRail1 = actor_contract:add_option(NewRail, in, 2),
	NewRail2 = actor_contract:add_option(NewRail1, out, 2),
		[?_assertEqual(
		{{ Prod, {supervisor, NewRail2}},{Prod,[2]}},
		answer(NewRail2, {next, Prod})),
		?_assertEqual(
		{answer, state, undefined},
		answer(NewRail2, {supervisor,state}))	
	].