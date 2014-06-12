-module(actor_railway).
-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).
create() ->
   actor_contract:create(?MODULE, actor_railway, off, undefined, 10, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
	{answer, actor_contract:get_state(RailwayConfig), actor_contract:get_id(ProductConfig)}.

