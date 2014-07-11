-module(actor_product).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

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
	RandomQuality = actor_contract:random(),
	create(RandomQuality).

create(Quality) ->
	create(actor_contract:random_id(),Quality).

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
-ifdef(TEST).


-endif.