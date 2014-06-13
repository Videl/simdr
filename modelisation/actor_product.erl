-module(actor_product).
-behaviour(actor_contract).
-include("config.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).

create() ->
   actor_contract:create(?MODULE, 450, undefined, off,0, []).

answer(ProductConfig, _ ) ->
		{answer, actor_contract:get_state(ProductConfig), actor_contract:get_list_data(ProductConfig)}.

%% ===================================================================
%% Tests
%% ===================================================================

answer_test() ->	
	{ok, Prod} = create(),
	NewProd = actor_contract:add_data(Prod,{21,05,02, q2}),
	{ answer, off, [{21,05,02, q2}]} = answer(NewProd, undefined).

