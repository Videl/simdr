-module(actor_rfid).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation
create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(), [{capacity, 4}], undefined, 2, []).

answer(RFIDConfig, {actor_product, ProductConfig}) ->
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

% %% Main loop
% idling(Config) ->
%  	actor_contract:idling(Config).

% processing(Config, NbWorker) ->
% 	receive
% 		{Sender, {actor_product,ProdConf}} ->
% 			Request = {actor_product,ProdConf},
% 			[N] = actor_contract:get_option(Config, capacity),
% 			case NbWorker+1> N of
% 				false -> Sender ! { self(), {control, ok, Request}},
% 						spawn(?MODULE, physical_work, [self(), Config, Request]),
% 						?MODULE:processing(actor_contract:set_state(Config, processing), NbWorker+1);

% 				_-> Sender ! { self(), {control, full,{actor_contract:get_work_time(Config), Request}}},
% 						?MODULE:processing(Config, NbWorker)				
% 			end;
% 		{Sender, {control, full, {Wait_time, Request}}} ->
% 			spawn(?MODULE, wait, [self(), Wait_time,{Request, Sender}]),
% 			?MODULE:processing(Config, NbWorker);

% 		{_Sender, Request} ->
% 			spawn(?MODULE, logical_work, [self(), Config, Request]),
% 			?MODULE:processing(Config, NbWorker);

% 		{_Worker, end_physical_work, {NewConfig, LittleAnswer, Destination}} ->
% 			% Find destination in 'out' pool
% 			% Send LittleAnswer
% 			{actor_product, ConfProd, _} = LittleAnswer,
% 			send_message({{actor_product, ConfProd}, Destination}),
% 			?MODULE:processing(actor_contract:set_state(NewConfig, work), NbWorker-1);
	
% 		{_Worker, end_logical_work, {NewConfig, LittleAnswer, Destination}} ->
% 			send_message({LittleAnswer, Destination}),
% 			%io:format(" Nouvelle config ~w ~n",[NewConfig]),
% 			?MODULE:processing(NewConfig, NbWorker);

% 		_ ->
% 			?MODULE:processing(Config, NbWorker)
% 	end.

% send_message( {Ans, [Dest]}) when is_pid(Dest) -> 
% 	io:format("RFID Sending: ~w, ~w.~n", [self(), {Ans}]),
% 	Dest ! {self(), {Ans}};

% send_message({Ans, Dest}) ->
% 	%% @TODO: decider de la destination
% 	io:format(" RFID Sending: ~w to ~w.~n", [Ans, Dest]).
% wait(Pid ,Wait_time, {Ans, [Dest]}) when is_pid(Dest)->
% 	actor_contract:work(Wait_time),
% 	Dest ! {Pid, {Ans}};

% wait(Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
% 	actor_contract:work(Wait_time),
% 	Dest ! {Pid, {Ans}};

% wait(_Pid ,Wait_time, {Ans, Dest}) ->
% 	actor_contract:work(Wait_time),
% 	io:format(" RFID Sending: ~w to ~w.~n", [Ans, Dest]).

% physical_work(Master, MasterConfig, Request) ->
% 	io:format("RFID work ~w.~n", [{MasterConfig,Request}]),
% 	FullAnswer = ?MODULE:answer(MasterConfig, Request),
% 	Master ! {self(), end_physical_work, FullAnswer}.

% logical_work(Master, MasterConfig, Request) ->
% 	FullAnswer = ?MODULE:answer(MasterConfig, Request),
% 	Master ! {self(), end_logical_work, FullAnswer}.

%% Tests

answer_test_() ->
	ActorRFID = actor_rfid:create(),
	Id= actor_contract:get_id(ActorRFID),
	ActorProduct = actor_product:create(product_one, 6),
{RFID, _, supervisor}=answer(ActorRFID, {actor_product, ActorProduct}),
	[
	?_assertEqual(
		{ActorRFID, {supervisor, pong}}, 
		answer(ActorRFID, {supervisor, ping})),
	?_assertMatch(
	{config, actor_rfid, Id, [{capacity,4}], undefined, 2,[{ActorProduct, _}]},
	RFID)
].
