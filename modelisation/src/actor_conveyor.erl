-module(actor_conveyor).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2,
	idling/1,
	send_rfid/2,
	processing/2, 
	logical_work/3,
	wait/3,
	physical_work/3]).


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
			case NbWorker+1> N of
				false -> Sender ! { self(), {control, ok, Request}},
						spawn(?MODULE, send_rfid, [Config, ProdConf]),
						spawn(?MODULE, physical_work, [self(), Config, Request]),
						?MODULE:processing(actor_contract:set_state(Config, processing), NbWorker+1);

				_-> Sender ! { self(), {control, full,{actor_contract : get_work_time(Config), Request}}},
						?MODULE:processing(Config, NbWorker)
				
			end;
		{Sender, {control, full, {Wait_time, Request}}} ->
			spawn(?MODULE, wait, [self(), Wait_time,{Request, Sender}]),
			?MODULE:processing(Config, NbWorker);

		{_Sender, Request} ->
			spawn(?MODULE, logical_work, [self(), Config, Request]),
			?MODULE:processing(Config, NbWorker);

		{_Worker, end_physical_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
		 	{actor_product, ConfProd, _} = LittleAnswer,
			send_message({{actor_product, ConfProd}, Destination}),
			?MODULE:processing(actor_contract:set_state(NewConfig, free), NbWorker-1);
	
		{_Worker, end_logical_work, {NewConfig, LittleAnswer, Destination}} ->
			send_message({LittleAnswer, Destination}),
			%io:format(" Nouvelle config ~w ~n",[NewConfig]),
			?MODULE:processing(NewConfig, NbWorker);
		_ ->
			?MODULE:processing(Config, NbWorker)
	end.

send_rfid( Conf, ProdConf) ->
	case actor_contract:get_option(Conf, rfid) of 
		[RFID] -> RFID ! {self(), {actor_product, ProdConf}};
		_ -> {nothing}
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

physical_work(Master, MasterConfig, Request) ->
	io:format("Conveyor work ~w.~n", [{MasterConfig,Request}]),
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {self(), end_physical_work, FullAnswer}.

logical_work(Master, MasterConfig, Request) ->
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {self(), end_logical_work, FullAnswer}.


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
