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

answer(ProductConfig, RailwayConfig) ->
case taille(get_option(RailwayConfig, in)) of 
	1 ->{ok};
	_ ->{ProductConfig, {supervisor, RailwayConfig}}
end,

case taille(get_option(RailwayConfig, out)) of 
	1 ->{ProductConfig, actor_contract:get_option(RailwayConfig,out)}; 
	_ ->{ProductConfig, {supervisor, RailwayConfig}}
end.


answer(RailwayConfig, {supervisor, state}) ->
	{answer, state, actor_contract:get_state(RailwayConfig)};

answer(ProductConfig, {supervisor, RailwayConfig, Decision}) ->
	actor_contract: set_state(RailwayConfig, Decision),
	{_In, Out}= Decision,
	actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
	{ProductConfig, Out}.

%Internal API

taille([_ | Queue]) -> 1 + taille(Queue).

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