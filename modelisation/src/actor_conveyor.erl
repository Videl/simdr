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
	physical_work/3]).


create() ->
	TablePid = ets:new(intern_queue, [duplicate_bag, public]),
   actor_contract:create(?MODULE, actor_contract:random_id(),  [{capacity,1}, {ets, TablePid}, {awaiting, 0}], off, 5, []).

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
			%io:format("Receiving product (~w)..~n", [self()]),
			[Awaiting] = actor_contract: get_option(Config, awaiting),
			case Awaiting > 0 of 
				true ->  NewConfig = actor_contract:set_option(Config, awaiting, Awaiting-1);
				false -> NewConfig = Config
			end,
			Request= {actor_product,ProdConf},	
			spawn(?MODULE, send_rfid, [NewConfig, ProdConf]),
			spawn(?MODULE, physical_work, [self(), NewConfig, Request]),
			?MODULE:processing(actor_contract:set_state(NewConfig, processing), NbWorker+1);

		{Sender, {control, ok}} ->
			%io:format("Receiving request of product~n"),
			[TablePid] = actor_contract:get_option(Config, ets),
			ListEntry = ets:match_object(
							TablePid, {product, awaiting_sending, '$1'}
						),
			case actor_contract:list_size(ListEntry) > 0 of
				true ->
					%io:format("Sending product..~n"),
					FirstEntry = actor_contract:first(ListEntry),
					{product, awaiting_sending, Prod} = FirstEntry,
					send_message({{actor_product, Prod}, Sender}),
					ets:delete_object(TablePid, FirstEntry),
					ets:insert(TablePid, {product, sent, Prod}),
					%io:format("End of sending product..~n"),
					processing(actor_contract:set_state(Config, free), NbWorker-1);

				false ->
					processing(Config, NbWorker)
			end;

		{Sender, awaiting_product} ->
			[Capacity]= actor_contract:get_option(Config, capacity),
			%io:format("NbWorker: ~w/Capacity: ~w~n", [NbWorker, Capacity]),
			case NbWorker<Capacity of 
				true -> 
					Sender ! {self(),{control, ok}},
					%io:format("JE SUIS ~w ET JE VEUX UN PRODUIT!~n", [self()]),
					processing(Config, NbWorker);

				false -> 
					[Awaiting] = actor_contract:get_option(Config, awaiting),
					processing(actor_contract:set_option(Config, awaiting, Awaiting+1), NbWorker)
			end;
			

		{_Sender, Request} ->
			spawn(?MODULE, logical_work, [self(), Config, Request]),
			?MODULE:processing(Config, NbWorker);

		{_Worker, end_physical_work, {NewConfig, LittleAnswer, Destination}} ->
			% Find destination in 'out' pool
			% Send LittleAnswer
			[TablePid] = actor_contract:get_option(Config, ets), 
			{actor_product, ConfProd, _} = LittleAnswer,
			ets:insert(TablePid, {product, awaiting_sending, ConfProd}),
		 	[Next]=Destination,
		 	%io:format("await"),
		 	Next ! {self(), awaiting_product},
			[Awaiting] = actor_contract:get_option(Config, awaiting),
			 case Awaiting>0 of
			  	true -> 
			  		actor_contract:get_option(Config, in) ! {self(), {control, ok}};
			 	false -> 
			 		%io:format("J'attends.~n"),
					wait
			 end,
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
	%io:format("Conveyor Sending: ~w to ~w.~n", [ Ans, Dest]),
	Message = {self(), Ans},
	io:format("send_message(),~w:~w -> ~w~n", [?MODULE, ?LINE, Message]),
	Dest ! Message;
send_message( {Ans, Dest}) when is_pid(Dest) ->
	Message = {self(), Ans},
	io:format("send_message(),~w:~w -> ~w~n", [?MODULE, ?LINE, Message]),
	Dest ! Message;
send_message({Ans, Dest}) ->
	%% @TODO: decider de la destination
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
	Prod = actor_product:create(2),
	Id = actor_contract:get_id(Conv),
	NewConv = actor_contract:add_option(Conv, out, 2),
%%	{ConvResult, ProdResult}= actor_contract:add_to_list_data({NewConv, Prod}, 
%%		{Prod, NewConv}),
	{_, _, Destination} = answer(Conv, {actor_product, Prod}),
	{Conveyor, _, DestinationTwo} = answer(NewConv, {actor_product, Prod}),
	[?_assertEqual(
		%{Conv, {actor_product, Prod, unknown_option}, unknown_option}
		unknown_option,
		Destination),
	?_assertEqual(
		[2],
		DestinationTwo),
	?_assertMatch(
	{config, actor_conveyor, Id, [{out,2},{capacity,1}], off, 2,[{Prod, _}]},
	Conveyor)
	].