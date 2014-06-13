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
create() ->
   actor_contract:create(?MODULE, actor_railway, undefined, off, 2, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
	{answer, actor_contract:get_state(RailwayConfig), actor_contract:get_id(ProductConfig)}.


%% ===================================================================
%% Tests
%% ===================================================================

answer_test() ->
	{ok, Rail} = create(),
	{ok, Prod} = actor_product:create(),
	{ answer, off, 450} = answer(Rail, {actor_product, Prod}).
