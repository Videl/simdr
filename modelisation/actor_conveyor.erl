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
   actor_contract:create(?MODULE, actor_conveyor,  undefined, off, 4, []).

answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	{ProductConfig, actor_contract:get_option(ConveyorConfig,out)}.



%% ===================================================================
%% Tests
%% ===================================================================

answer_test() ->
	{ok, Conv} = create(),
	{ok, Prod} = actor_product:create(),
	{ Conv, finish, Prod} = answer(Conv, {actor_product, Prod}).