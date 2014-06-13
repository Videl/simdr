-module(actor_railway).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).
create() ->
   actor_contract:create(?MODULE, actor_railway, undefined, off, 2, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
	{answer, actor_contract:get_state(RailwayConfig), actor_contract:get_id(ProductConfig)};

answer(RailwayConfig, {supervisor, state}) ->
	{ answer, state, actor_contract:get_state(RailwayConfig)}.
%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->
	Rail = create(),
	Prod = actor_product:create(),
		Id = actor_contract:get_id(Prod),
	[?_assertEqual(
		{ answer, off, Id},
		answer(Rail, {actor_product, Prod})),
	?_assertEqual(
		{answer, state, off},
		answer(Rail, {supervisor,state}))	
	].