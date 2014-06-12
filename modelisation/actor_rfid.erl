-module(actor_rfid).
-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).

%% External API

-export([create/0]).

%% Behavior implementation

answer(RFIDConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{RFIDConfig, answer, actor_contract:get_id(ProductConfig)};

answer(RFIDConfig, ping) ->
	{RFIDConfig, answer, pong};

answer(_RFIDConfig, _) ->
	undefined.

%% External API

create() ->
	actor_contract:create(?MODULE, rfid, 0).

%% Internal API