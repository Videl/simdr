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
	create/1,
	create/2
	]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, raw, 0).

create(Quality) ->
	actor_contract:create(?MODULE, actor_contract:random_id(), [{quality_required, Quality}], raw, 0, []).

create(Id, Quality) ->
	actor_contract:create(?MODULE, Id, [{quality_required, Quality}], raw, 0, []).

answer(ProdConfig, {change, Data,_}) ->
	{ProdConfig, Data, no_change};

answer(ProdConfig, Request) ->
	actor_contract:answer(ProdConfig, Request).


%% Internal API

%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->	
	Prod = create(),
	NewProd = actor_contract:add_data(Prod, {21,05,02, q2}),
	[
	?_assertEqual( %% State test
		raw,
		actor_contract:get_state(NewProd)),
	?_assertEqual(
		{21,05,02, q2},
		actor_contract:get_data(NewProd))
	].