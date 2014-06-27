-module(actor_conveyor).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Export for spawns
-export([
	send_rfid/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(),  [{capacity,1}], off, 5, []).


answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	spawn(?MODULE, send_rfid, [ConveyorConfig, ProductConfig]),
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	Destination = actor_contract:get_option(ConveyorConfig, out),
	{NewConveyorConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{ConveyorConfig, ProductConfig}, 
		{ProductConfig, ConveyorConfig}),
	% Answer
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
	Prod = actor_product:create(2),
	%Id = actor_contract:get_id(Conv),
	NewConv = actor_contract:add_option(Conv, out, 2),
%%	{ConvResult, ProdResult}= actor_contract:add_to_list_data({NewConv, Prod}, 
%%		{Prod, NewConv}),
	{_, _, Destination} = answer(Conv, {actor_product, Prod}),
	{_Conveyor, _, DestinationTwo} = answer(NewConv, {actor_product, Prod}),
	[?_assertEqual(
		%{Conv, {actor_product, Prod, unknown_option}, unknown_option}
		unknown_option,
		Destination),
	?_assertEqual(
		[2],
		DestinationTwo)
	% ?_assertMatch(
	% {config, actor_conveyor, Id, [{out,2},{capacity,1}, {awaiting, 0}, {ets, _}], off, _,[{Prod, _}]},
	% Conveyor)
	].

-endif.