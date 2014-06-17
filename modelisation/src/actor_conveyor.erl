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
	Destination = actor_contract:get_option(ConveyorConfig, out),
	{NewConveyorConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{ConveyorConfig, ProductConfig}, 
		{ProductConfig, ConveyorConfig}),
	% Answer
	{NewConveyorConfig, {actor_product, NewProductConfig, Destination}, Destination};

answer(ConveyorConfig, Request) ->
	actor_contract:answer(ConveyorConfig, Request).

%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->
	Conv = create(),
	Prod = actor_product:create(),
	NewConv = actor_contract:add_option(Conv, out, 2),
	{ConvResult, ProdResult}= actor_contract:add_to_list_data({NewConv, Prod}, 
		{Prod, NewConv}),
	{_, _, Destination} = answer(Conv, {actor_product, Prod}),
	{_, _, DestinationTwo} = answer(NewConv, {actor_product, Prod}),
	[?_assertEqual(
		%{Conv, {actor_product, Prod, unknown_option}, unknown_option}
		unknown_option,
		Destination),
	?_assertEqual(
		[2],
		DestinationTwo),
	?_assertEqual(
		{ConvResult, {actor_product, ProdResult, [2]}, [2]},
		answer(NewConv, {actor_product, Prod}))].
