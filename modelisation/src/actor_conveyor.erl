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
	answer/2]).

%% Export for spawns
-export([
	send_rfid/2]).

%% Behavior implementation

create() ->
	create( actor_contract:random_id()).
	
create(Name) ->
	actor_contract:create(?MODULE, Name, [], off, 5, []).


answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	spawn(?MODULE, send_rfid, [ConveyorConfig, ProductConfig]),
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	{NewConveyorConfig, NewProductConfig} = actor_contract:add_to_list_data(
		ConveyorConfig, 
		{moved, actor_contract:get_module(ProductConfig), {ProductConfig}}, 
		ProductConfig, 
		{was,moved,by,actor_contract:get_module(ConveyorConfig), {ConveyorConfig}}),
	%%% Answer
	Destination = actor_contract:get_out(ConveyorConfig),
	{NewConveyorConfig, {actor_product, NewProductConfig, Destination}, Destination};
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
	Conv = create(),
	Prod = actor_product:create(),
	{_, {actor_product, ProdTwo, Destination}, Destination} = answer(Conv, {actor_product, Prod}),
	[
		%%% Test: product does not change
		?_assertEqual(
			Prod,
			ProdTwo)
	].

-endif.