-module(actor_product).
-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).

create() ->
   actor_contract:create(?MODULE, actor_product, off, undefined, 10, []).

answer(ProductConfig, _) ->
		{answer, actor_contract:get_state(ProductConfig), actor_contract:get_data(ProductConfig)}.



