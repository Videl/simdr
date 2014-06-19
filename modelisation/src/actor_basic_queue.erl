-module(actor_basic_queue).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").
-include("debug.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

-export([
	init/0,
	idling/1,
	powered/1,
	processing/1,
	worker_loop/3]).

%% Behavior implementation
create() ->
	TablePid = ets:new(test2, [duplicate_bag, public]),
	actor_contract:add_option(actor_contract:create(?MODULE, 'BasicQueue', 1),
							  ets,
							  TablePid).

%% Possible answer: a new product arriving
answer(BasicQueueConfig, {actor_product, ProductConfig, register}) ->
	%% Add it to ETS
	%% @TODO: time ?!
	[TablePid] = actor_contract:get_option(BasicQueueConfig, ets),
	ets:insert(TablePid, {product, awaiting_processing, ProductConfig}),
	%% @TODO: Add it to list_data (this actor + the product) to know we had it
	{BasicQueueConfig, {actor_product, ProductConfig, saved_ets}, main_loop};
answer(BasicQueueConfig, Request) ->
	actor_contract:answer(BasicQueueConfig, Request).


%% Main loop
init() ->
	?CREATE_DEBUG_TABLE,
	?DLOG("Basic Queue Actor initialisation."),
	Conf = ?MODULE:create(),
	spawn(?MODULE, idling, [Conf]).


%% Awaiting the start command
idling(Config) ->
	receive
		{start} ->
			?MODULE:powered(actor_contract:set_state(Config, on));
		{Sender, actor_product, _, _} when is_pid(Sender) ->
			Sender ! {state, actor_contract:get_state(Config)},
			?MODULE:idling(Config);
		_ ->
			?MODULE:idling(Config)
	end.

%% Awaiting a new product
powered(Config) ->
	receive
		{stop} ->
			?MODULE:idling(actor_contract:set_state(Config, off));
		{Sender, Request} -> 
			io:format("received request~n"),
			spawn(?MODULE, worker_loop, [self(), Config, Request]),
			?MODULE:processing(
				actor_contract:set_state(Config, processing));
		_ ->
			?MODULE:powered(Config)
	end.

%% Awaiting new products
%% Trying to send them over to a work station
processing(Config) ->
	receive
		{Sender, Request} ->
			%io:format("received request~n"),
			spawn(?MODULE, worker_loop, [self(), Config, Request]),
			?MODULE:processing(Config);
		{end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Being here means: a new product has arrived to my attention and 
			% 					has been set up in ETS.
			% So there is quite nothing to do here.
			%io:format("request done~n"),
			MODULE:processing(actor_contract:set_state(NewConfig, processing));
		_ ->
			?MODULE:processing(Config, MainWorker)
		after make_up_wait_time(Config) ->
			%% Try to send the first product arrived
			%% to the workstation to out.
			ok
	end.


worker_loop(Master, MasterConfig, Request) ->
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {end_of_work, FullAnswer}.

%% Internal API

make_up_wait_time(Config) ->
	actor_contract:get_work_time(Config)*1000.

%% Tests

answer_test_() ->
	ActorBasicQueue = actor_rfid:create(),
	ActorProduct = actor_product:create(product_one),
	{BQResult, ProdResult}= actor_contract:add_to_list_data({ActorBasicQueue, ActorProduct}, 
		{ActorProduct, ActorBasicQueue}),	[
	?_assertEqual(
		{ActorBasicQueue, {supervisor, pong}}, 
		answer(ActorBasicQueue, {supervisor, ping})),
	?_assertEqual(
		{BQResult,{actor_product, ProdResult, product_one}, anyone},
	answer(ActorBasicQueue, {actor_product, ActorProduct, id}))	].
