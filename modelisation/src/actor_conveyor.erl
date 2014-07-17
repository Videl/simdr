-module(actor_conveyor).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	create/1,
	create/3,
	answer/2]).

%% Export for spawns
-export([
	send_rfid/2]).

%% Behavior implementation

create() ->
	create( actor_contract:random_id()).
	
create(Name) ->
	create(Name, 1,1).

create(Name, Speed, Distance) ->
		Actor= actor_contract:create(?MODULE, Name, [], off, Distance/Speed, []),
		actor_contract:set_option(Actor, distance, Distance),
		actor_contract: set_option(Actor, speed, Speed),
		Actor.


answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	spawn(?MODULE, send_rfid, [ConveyorConfig, ProductConfig]),
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	{NewConveyorConfig, NewProductConfig} = actor_contract:add_to_list_data(
		ConveyorConfig, 
		{{moved, actor_contract:get_module(ProductConfig)}, {ProductConfig}}, 
		ProductConfig, 
		{{was,moved,by,actor_contract:get_module(ConveyorConfig)}, {ConveyorConfig}}),
	%%% Answer
	Destination = actor_contract:get_out(ConveyorConfig),
	{NewConveyorConfig, {actor_product, NewProductConfig, Destination}, Destination};


answer(ActorConfig, {change, distance, N}) ->
	actor_contract:set_option(ActorConfig, distance, N),
	Speed = actor_contract:get_option(ActorConfig,speed),
	NewConfig = actor_contract:set_work_time(N/Speed),
	{NewConfig, {distance, N, changed}, supervisor};

answer(ActorConfig, {change, speed, N}) ->
	actor_contract: set_option(ActorConfig, speed, N),
	Distance = actor_contract:get_option(ActorConfig,distance),
	NewConfig = actor_contract:set_work_time(Distance/N),
	{NewConfig, {speed, N, changed}, supervisor};


answer(ActorConfig, {status, distance}) ->
	[Distance] = actor_contract:get_option(ActorConfig,distance),
	{ActorConfig, {distance, Distance, status}, supervisor};

answer(ActorConfig, {status, speed}) ->
	[Speed] = actor_contract:get_option(ActorConfig,speed),
	{ActorConfig, {speed, Speed , status}, 	supervisor};

answer(ConveyorConfig, Request) ->
	actor_contract:answer(ConveyorConfig, Request).

%% Internal API

send_rfid(Conf, ProdConf) ->
	case actor_contract:get_option(Conf, rfid) of 
		[RFID] -> RFID ! {self(), {actor_product, ProdConf}};
		_ -> {nothing}
	end.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
	Conv = create('C1', 4, 6),
	Prod = actor_product:create(),
	{_, {actor_product, ProdTwo, Destination}, Destination} = answer(Conv, {actor_product, Prod}),
	[
		%%% Test: product does not change
		?_assertEqual(
			Prod,
			ProdTwo),
		?_assertEqual({ Conv, {speed, 4, status},supervisor}, answer(Conv, {status, speed})),
		?_assertEqual({ Conv, {work_time, 1.5, status},supervisor}, answer(Conv, {status, work_time}))
	].

-endif.