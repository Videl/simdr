-module(actor_rfid).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).



-export([
	idling/1,
	powered/1,
	processing/3,
	worker_loop/3]).

%% Behavior implementation
create() ->
	actor_contract:create(?MODULE,rfid,[{capacity, 2}], undefined, 10, []).

answer(RFIDConfig, {actor_product, ProductConfig, id}) ->
	actor_contract:work(actor_contract:get_work_time(RFIDConfig)),
	{NewRFIDConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{RFIDConfig, ProductConfig}, 
		{ProductConfig, RFIDConfig}),
	% Answer
	{NewRFIDConfig, 
	{actor_product, NewProductConfig, actor_contract:get_id(NewProductConfig)}, 
	supervisor};

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

		{_Sender, Request} ->
					Pid = spawn(?MODULE, worker_loop, [self(), Config, Request]),
					?MODULE:processing(actor_contract:set_state(Config, processing), Pid, 1);
		_ ->
			?MODULE:powered(Config)
	end.

processing(Config, Worker, NbWorker) ->
	receive
		{Sender, {actor_product,ProdConf, _}} ->
					Request= {actor_product,ProdConf, id},
					case NbWorker> actor_contract:get_option(Config, capacity) of
					true -> Sender ! { state, full, NbWorker},
							?MODULE:processing(Config, Worker, NbWorker);
					false -> Pid = spawn(?MODULE, worker_loop, [self(), Config, Request]),
								?MODULE:processing(actor_contract:set_state(Config, processing), Pid, NbWorker+1)
					end;

		{_Worker, end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
		
			send_message({LittleAnswer,NbWorker, Destination}),
			?MODULE:processing(actor_contract:set_state(NewConfig, work),2, NbWorker-1);
		_ ->
			?MODULE:processing(Config, Worker, NbWorker)
	end.

send_message({Ans, Nb, Dest}) when is_pid(Dest) -> 
	Dest ! Ans;
send_message({Ans, Nb, Dest}) ->
	%% @TODO: decider de la destination
	io:format("Sending: ~w and ~w to ~w.~n", [Ans, Nb, Dest]).

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
