-module(actor_product).
-behaviour(actor_contract).
-include("config.hrl").
-include_lib("eunit/include/eunit.hrl").

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
   actor_contract:create(?MODULE, random_id(), undefined, raw, 0, []).

create(Id) ->
	actor_contract:create(?MODULE, Id, undefined, raw, 0, []).

answer(ProductConfig, _ ) ->
		{answer, actor_contract:get_state(ProductConfig), actor_contract:get_list_data(ProductConfig)}.


%% Internal API

random_id() ->
	random:uniform(1000).

%% ===================================================================
%% Tests
%% ===================================================================

answer_test() ->	
	{ok, Prod} = create(),
	NewProd = actor_contract:add_data(Prod,{21,05,02, q2}),
	{ answer, raw, [{21,05,02, q2}]} = answer(NewProd, undefined).

