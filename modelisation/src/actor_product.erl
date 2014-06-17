-module(actor_product).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2,
	create/0
	]).

%% External API

-export([
	create/1
	]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, random_id(), [], raw, 0, []).

create(Id) ->
	actor_contract:create(?MODULE, Id, [], raw, 0, []).

answer(ProductConfig, state) ->
	{answer, actor_contract:get_id(ProductConfig), state, actor_contract:get_state(ProductConfig)};

answer(ProductConfig, list_data) ->
	{answer,  actor_contract:get_id(ProductConfig),	list_data, actor_contract:get_list_data(ProductConfig)}.


%% Internal API

random_id() ->
	random:uniform(1000).

%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->	
	Prod = create(),
	NewProd = actor_contract:add_data(Prod,{21,05,02, q2}),
	Id = actor_contract:get_id(NewProd),
	[?_assertEqual(
		{answer, Id, list_data, [{21,05,02, q2}]},
		 answer(NewProd, list_data)),
	?_assertEqual(
		{answer, Id, state, raw},
		answer(NewProd, state))	
	].