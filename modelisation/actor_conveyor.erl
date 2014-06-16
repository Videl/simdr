-module(actor_conveyor).
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
   actor_contract:create(?MODULE, actor_conveyor,  [], off, 4, []).

answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	{ProductConfig, actor_contract:get_option(ConveyorConfig,out)}.



%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->
	Conv = create(),
	Prod = actor_product:create(),
	NewConv = actor_contract:add_option(Conv, out, 2),
	[?_assertEqual(
		{Prod, unknown_option},
		 answer(Conv, {actor_product, Prod})),
	?_assertEqual(
		{Prod, [2]},
		 answer(NewConv, {actor_product, Prod}))].
