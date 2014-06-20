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
	processing/2,
	worker_loop/3]).

%% Behavior implementation
create() ->
	actor_contract:create(?MODULE,rfid,[{capacity, 4}], undefined, 20, []).

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
 	actor_contract : idling(Config).

processing(Config, NbWorker) ->
	receive
		{Sender, {actor_product,ProdConf, _}} ->
			Request= {actor_product,ProdConf, id},
			[N] = actor_contract:get_option(Config, capacity),
			case NbWorker> N-1 of
				false -> 
					spawn(?MODULE, worker_loop, [self(), Config, Request]),
					?MODULE:processing(actor_contract:set_state(Config, processing), NbWorker+1);

				_-> 
					Sender ! { self(), {control, full,{actor_contract : get_work_time(Config), Request}}},
					?MODULE:processing(Config, NbWorker)
				
			end;
		{Sender, {control, full, {Wait_time, Request}}} ->
			spawn(?MODULE, wait, [self(), Wait_time,{Request, Sender}]),
			?MODULE:processing(Config, NbWorker);

		{Sender, Request} ->
			A =	?MODULE:answer(Config, Request),
			send_message({A,Sender}),
			?MODULE:processing(Config, NbWorker);

		{_Worker, end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
		
			send_message({LittleAnswer, Destination}),
			?MODULE:processing(actor_contract:set_state(NewConfig, work), NbWorker-1);
		_ ->
			?MODULE:processing(Config, NbWorker)
	end.


send_message( {Ans, Dest}) when is_pid(Dest) -> 
	Dest ! {self(), {Ans}};
send_message({Ans, Dest}) ->
	%% @TODO: decider de la destination
	io:format("Sending: ~w to ~w.~n", [Ans, Dest]).

wait(Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};

wait(_Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	io:format("Sending: ~w to ~w.~n", [Ans, Dest]).

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
