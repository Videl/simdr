-module(actor_rfid).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

%%

-export([
	idling/1,
	powered/1,
	processing/3,
	worker_loop/3]).

%% Behavior implementation
create() ->
	actor_contract:create(?MODULE, rfid, 1).

answer(RFIDConfig, {actor_product, ProductConfig, id}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{NewRFIDConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{RFIDConfig, ProductConfig}, 
		{ProductConfig, RFIDConfig}),
	% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, actor_contract:get_id(NewProductConfig)}, 
	anyone};

answer(RFIDConfig, Request) ->
	actor_contract:answer(RFIDConfig, Request).

%% Main loop
idling(Config) ->
	receive
		{start} ->
			?MODULE:powered(actor_contract:set_state(Config, on));
		{Sender, actor_product, _, _} ->
			Sender ! {state, actor_contract:get_state(Config)},
			?MODULE:idling(Config);
		_ ->
			?MODULE:idling(Config)
	end.

powered(Config) ->
	receive
		{stop} ->
			?MODULE:idling(actor_contract:set_state(Config, off));
		{Sender, Request} -> 
			Pid = spawn(?MODULE, worker_loop, [self(), Config, Request]),
			?MODULE:processing(actor_contract:set_state(Config, processing), Pid, Sender);
		_ ->
			?MODULE:powered(Config)
	end.

processing(Config, MainWorker, Sender) ->
	receive
		{Sender, _Request} ->
			Sender ! {state, busy},
			?MODULE:processing(Config, MainWorker);
		{MainWorker, end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
			Sender ! {LittleAnswer, Destination},
			?MODULE:powered(actor_contract:set_state(NewConfig, on));
		_ ->
			?MODULE:processing(Config, MainWorker)
	end.

worker_loop(Master, MasterConfig, Request) ->
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {self(), end_of_work, FullAnswer}.

%% Tests

answer_test_() ->
	
	ActorRFID = actor_rfid:create(),
	ActorProduct = actor_product:create(product_one),
	{RFIDResult, ProdResult}= actor_contract:add_to_list_data({ActorRFID, ActorProduct}, 
		{ActorProduct, ActorRFID}),	[
	?_assertEqual(
		{ActorRFID, {supervisor, pong}}, 
		answer(ActorRFID, {supervisor, ping})),
	?_assertEqual(
		{RFIDResult,{actor_product, ProdResult, product_one}, anyone},
	answer(ActorRFID, {actor_product, ActorProduct, id}))	].
