-module(actor_workstation).
-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).

%% Behavior implementation

answer(WSConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(WSConfig)),
	NewProductConfig = change_product(ProductConfig),
	{WSConfig, answer, NewProductConfig};

answer(WSConfig, {supervisor, ping}) ->
	{WSConfig, answer, pong};

answer(WSConfig, {supervisor, work_time, N}) ->
	NewWsConfig = actor_contract:set_work_time(WSConfig, N),
	{WSConfig, answer, NewWsConfig};

answer(_WSConfig, _) ->
	undefined.

%% External API

create() ->
	actor_contract:create(?MODULE, workstation, 0).

%% Internal API

change_product(ProductConfig) ->
	case random:uniform(3) of
		1 -> % Good quality
			NewP = actor_contract:set_state(ProductConfig, "Q1");
		2 -> % Medium quality
			NewP = actor_contract:set_state(ProductConfig, "Q2");
		3 -> % Bad quality
			NewP = actor_contract:set_state(ProductConfig, "Q3")
	end,
	NewP.