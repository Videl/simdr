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
	actor_contract:create(?MODULE, random_id(), [], raw, 0, []).

create(Id) ->
	actor_contract:create(?MODULE, Id, [], raw, 0, []).

create(Id, Quality) ->
	actor_contract:create(?MODULE, Id, [{quality_required, Quality}], raw, 0, []).

answer(ProdConfig, {change, Data,_}) ->
	{ProdConfig, Data, no_change};

answer(ProdConfig, Request) ->
	actor_contract:answer(ProdConfig, Request).


%% Internal API

random_id() ->
	random:uniform(1000).

%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->	
	Prod = create(123456),
	NewProd = actor_contract:add_data(Prod, {21,05,02, q2}),
	[?_assertEqual( %% ID test
		123456,
		actor_contract:get_id(NewProd)),
	?_assertEqual( %% State test
		raw,
		actor_contract:get_state(NewProd)),
	?_assertEqual(
		{21,05,02, q2},
		actor_contract:get_data(NewProd))
	].