-module(actor_conveyor).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	answer/2
	]).
-export([
	idling/1,
	processing/2,
	worker_loop/3,
	get_information/3,
	wait/3]).

%% External API

-export([create/0]).

create() ->
   actor_contract:create(?MODULE, actor_conveyor,  [{capacity,1}], off, 5, []).

answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	Destination = actor_contract:get_option(ConveyorConfig, out),
	{NewConveyorConfig, NewProductConfig} = actor_contract:add_to_list_data(
		{ConveyorConfig, ProductConfig}, 
		{ProductConfig, ConveyorConfig}),
	% Answer
	{NewConveyorConfig, {actor_product, NewProductConfig, Destination}, Destination};

answer(ConveyorConfig, Request) ->
	actor_contract:answer(ConveyorConfig, Request).

idling(Config) ->
 	actor_contract : idling(Config).

processing(Config, NbWorker) ->
	receive
		{Sender, {actor_product,ProdConf}} ->
			Request= {actor_product,ProdConf},
			[N] = actor_contract:get_option(Config, capacity),
			case NbWorker> N-1 of
				false -> Sender ! { self(), {control, ok, Request}},
						spawn(?MODULE, worker_loop, [self(), Config, Request]),
						?MODULE:processing(actor_contract:set_state(Config, processing), NbWorker+1);

				_-> Sender ! { self(), {control, full,{actor_contract : get_work_time(Config), Request}}},
						?MODULE:processing(Config, NbWorker)
				
			end;
		{Sender, {control, full, {Wait_time, Request}}} ->
			spawn(?MODULE, wait, [self(), Wait_time,{Request, Sender}]),
			?MODULE:processing(Config, NbWorker);

		{_Sender, Request} ->
			spawn(?MODULE, get_information, [self(), Config, Request]),
			?MODULE:processing(Config, NbWorker);

		{_Worker, end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
		 	{actor_product, ConfProd, _} = LittleAnswer,
			send_message({{actor_product, ConfProd}, Destination}),
			?MODULE:processing(actor_contract:set_state(NewConfig, work), NbWorker-1);
	
		{_Worker, information, {NewConfig, _, Information}} ->
			send_message({Information, superviseur}),
			%io:format(" Nouvelle config ~w ~n",[NewConfig]),
			?MODULE:processing(NewConfig, NbWorker);

		_ ->
			?MODULE:processing(Config, NbWorker)
	end.


send_message( {Ans, [Dest]}) when is_pid(Dest) -> 
io:format("Conveyor Sending: ~w to ~w.~n", [ Ans, Dest]),
	Dest ! {self(), Ans};
send_message( {Ans, Dest}) when is_pid(Dest) -> 
	Dest ! {self(), {Ans}};
send_message({Ans, Dest}) ->
	%% @TODO: decider de la destination
	io:format("Conveyor Sending: ~w to ~w.~n", [Ans, Dest]).

wait(Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};

wait(_Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	io:format("Conveyor Sending: ~w to ~w.~n", [Ans, Dest]).

worker_loop(Master, MasterConfig, Request) ->
	io:format("Conveyor work ~w.~n", [{MasterConfig,Request}]),
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {self(), end_of_work, FullAnswer}.

get_information(Master, MasterConfig, Request) ->
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {self(), information, FullAnswer}.


%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->
	Conv = create(),
	Prod = actor_product:create(),
	NewConv = actor_contract:add_option(Conv, out, 2),
	{ConvResult, ProdResult}= actor_contract:add_to_list_data({NewConv, Prod}, 
		{Prod, NewConv}),
	{_, _, Destination} = answer(Conv, {actor_product, Prod}),
	{_, _, DestinationTwo} = answer(NewConv, {actor_product, Prod}),
	[?_assertEqual(
		%{Conv, {actor_product, Prod, unknown_option}, unknown_option}
		unknown_option,
		Destination),
	?_assertEqual(
		[2],
		DestinationTwo),
	?_assertEqual(
		{ConvResult, {actor_product, ProdResult, [2]}, [2]},
		answer(NewConv, {actor_product, Prod}))].
