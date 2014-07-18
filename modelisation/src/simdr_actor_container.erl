-module(simdr_actor_container).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([wait/3,
		 idling/1,
		 processing/2,
		 physical_work/3,
		 logical_work/3]).

%% ===================================================================
%% Loops functions
%% ===================================================================

idling(Config) ->
	?CREATE_DEBUG_TABLE,
	receive
		{start} ->
			?DLOG(
				simdr_actor_contract:get_module(Config), 
				"Entering processing state."),
			NewConfig = simdr_actor_contract:set_pid(Config, self()),
			processing(simdr_actor_contract:set_state(NewConfig, awaiting), 0);
		{Sender, actor_product, _, _} when is_pid(Sender) ->
			Sender ! {state, simdr_actor_contract:get_state(Config)},
			idling(Config);
		_ ->
			idling(Config)
	end.


processing(Config, NbWorkers) ->
	receive
		{_Sender, {prob_in, Decision}} ->
			{_In, Out} = simdr_actor_contract:get_in_out(Config),
			NewIn = Decision,
			NewConfig = simdr_actor_contract:set_in_out(Config, {NewIn, Out}),
			NewIn ! {self(), {control, ok}},
			processing(NewConfig, NbWorkers);

		{_Sender, {prob_out, Prod, Decision}} ->
			spawn(?MODULE, physical_work, [self(), Config, {prob_out, Prod, Decision}]),
			processing(Config, NbWorkers);

		{Sender, Request} ->
			{NewConfig, NewWorkers} = 
				manage_request({Config, NbWorkers, Sender}, Request),
			processing(NewConfig, NewWorkers);
		
		{_Worker, end_physical_work, Request} ->
			{NewConfig, NewNbWorkers} = end_of_physical_work(
				{Config, NbWorkers}, 
				Request),
			processing(NewConfig, NewNbWorkers);

		{_Worker, end_logical_work, Request} ->
			{NewConfig, NewNbWorkers} = end_of_logical_work(
				{Config, NbWorkers}, 
				Request),
			?DLOG({configuration,has,changed,{NewConfig}}),
			processing(NewConfig, NewNbWorkers);

		V ->
			io:format(">>>UNKNOWN REQUEST<<< (~w)~n", [V]),
			?MODULE:processing(Config, NbWorkers)
	end.


physical_work(Master, MasterConfig, Request) ->
	FullAnswer = 
		(simdr_actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_physical_work, FullAnswer}.


logical_work(Master, MasterConfig, Request) ->
	FullAnswer = 
		(simdr_actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_logical_work, FullAnswer}.


%%% If there is a problem of destinations, a specific messsage is sent to 
%%% supervisor which take a decision.
end_of_physical_work({_Config, NbWorkers},{NewConf, {actor_product, _ProductConf, prob_out}, Dest}) ->
	send_message(NewConf, {NewConf,{actor_product, _ProductConf, prob_out}}, Dest),
	{NewConf, NbWorkers};
%%% A worker node has ended.
%%% Things we have to do:
%%%  1) Add a debug line
%%%  2) Add it in the list of products waiting to be sent.
%%%  3) Send message to next actor to notify him.
%%%  4) Check if I have been notified of new products ready for me
%%%     If yes, send a message to actor in `in' if there are no problems,
%%%     or to the supervisor.
%%%  5) Take its new configuration for us. (@TODO: why don't we remove this?)
%%% @end
end_of_physical_work(
					 {Config, NbWorkers}, 
					 {NewConfig, LittleAnswer, Destination}) ->
	[TablePid] = simdr_actor_contract:get_option(Config, ets), 
	{actor_product, ProductConfig, _Detail} = LittleAnswer,
	Awaiting = ets:match_object(TablePid, {awaiting, '$1'}),
	?DLOG(
		simdr_actor_contract:get_module(Config), 
		{work,done,on,product,ProductConfig}),
	% io:format("~w >>> Work is done on product id ~p.\n", 
	% 	[simdr_actor_contract:get_module(NewConfig), simdr_actor_contract:get_name(ProductConfig)]),
	simdr_actor_contract:add_data(
		NewConfig, 
		{{work,on,product,is,done},{ProductConfig}}), 
	simdr_actor_contract:add_data(
		ProductConfig, 
		{{processing,done,by},{NewConfig}}),
	io:format(" ~w, ~w finish to work product : ~w ~n ~n",
		[simdr_actor_contract:get_module(NewConfig), 
		 simdr_actor_contract:get_name(NewConfig), 
		 simdr_actor_contract:get_name(ProductConfig)]),
	ets:insert(TablePid, {product, awaiting_sending, ProductConfig}),
 	send_message(Config, awaiting_product, Destination),
	%[Awaiting] = simdr_actor_contract:get_option(NewConfig, awaiting),
	case simdr_actor_contract:list_size(Awaiting) > 0 of
		true -> 
			case simdr_actor_contract:list_size(simdr_actor_contract:get_in(Config)) of 
				1 ->
					[InActor] = simdr_actor_contract:get_in(Config),
					Workers = NbWorkers+1,
					InActor ! {self(), {control, ok}};
				_ -> 
					case simdr_actor_contract:different_sender(Awaiting) of 
						%%% Sending messageder to supervisor 
						true ->	
							Workers = NbWorkers,
							send_message(Config, {Config, prob_in}, supervisor);
						false -> 
							[H|_Rest] = Awaiting,
							io:format("same sender~n"),
							{awaiting, {S, _Date}} = H,
							Workers = NbWorkers+1,
							S ! {self(), {control, ok}}
					end
			end;
		false -> 
			Workers = NbWorkers,
			%%% No new products incoming.
			wait
	end,
	{simdr_actor_contract:set_state(NewConfig, awaiting), Workers}.


end_of_logical_work({_Config, NbWorkers}, 
					{NewConfig, LittleAnswer, Destination}) ->
	send_message(NewConfig, LittleAnswer, Destination),
	{NewConfig, NbWorkers}.

%%% Receiving a product
%%% This function is called when we receive a product, and we receive a
%%% product when WE ask for it first.
%%% So Awaiting > 0 when we are here, hopefully..
%%% Returns: {NewConfig, NewNbWorkers}
%%% @end
manage_request({Config, NbWorkers, Sender}, {actor_product, ProdConf}) ->
	io:format("~w receive product ~w ~n~n", 
		[simdr_actor_contract:get_name(Config), 
		 simdr_actor_contract:get_name(ProdConf)]),
	?DLOG(
		simdr_actor_contract:get_module(Config), 
		{starting,to,work,on,product,ProdConf}),
	% io:format("~w >>> Work is starting on product id ~p.\n", [simdr_actor_contract:get_module(NewConfig), simdr_actor_contract:get_name(ProdConf)]),
	simdr_actor_contract:add_data(Config, {{new,product,has,arrived}, {ProdConf}}), 
	simdr_actor_contract:add_data(ProdConf, {{processing,started,by},Config}),
	%%% Decrement the number of products waiting for us.
	[TablePid] = simdr_actor_contract:get_option(Config, ets),
	Awaiting = ets:match_object(
					TablePid, {awaiting, '$1'}),
	case  simdr_actor_contract:list_size(Awaiting) > 0 of 
		true -> 
			FirstAwaiting = simdr_actor_contract:first_key_awaiting(Awaiting,Sender),
			ets:delete_object(TablePid, FirstAwaiting),
			Awaiting2 = ets:match_object(
					TablePid, {awaiting, '$1'});
		false ->
			ok
	end,
	Request = {actor_product, ProdConf},
	spawn(?MODULE, physical_work, [self(), Config, Request]),
	{Config, NbWorkers};
%%% Receiving request of a product from actor in `out'.
%%% Consequence: one of my product in the waiting list
%%% is sent.
%%% @end
manage_request({Config, NbWorkers, Sender}, {control, ok}) ->
	io:format("~w receive request of product~n~n", [simdr_actor_contract:get_name(Config)]),
	[TablePid] = simdr_actor_contract:get_option(Config, ets),
	ListEntry = ets:match_object(
					TablePid, {product, awaiting_sending, '$1'}
				),
	case simdr_actor_contract:list_size(ListEntry) > 0 of
		true ->
			%io:format("Sending product..~n"),
			FirstEntry = simdr_actor_contract:first(ListEntry),
			{product, awaiting_sending, Prod} = FirstEntry,
			simdr_actor_contract:add_data(Config, {{sending, product}, {Prod}}), 
			simdr_actor_contract:add_data(Prod, {{being,sent,to, Sender, by},{Config}}), 
			send_message(Config, {actor_product, Prod}, Sender),
			ets:delete_object(TablePid, FirstEntry),
			ets:insert(TablePid, {product, sent, Prod}),
			%io:format("End of sending product..~n"),
			NewConfig = simdr_actor_contract:set_state(Config, on),
			NewNbWorkers = NbWorkers-1;

		false ->
			%%% No change
			NewConfig = Config,
			NewNbWorkers = NbWorkers
	end,
	{NewConfig, NewNbWorkers};
%%% Receiving a notification from one of my actor in `in' that
%%% a product can be sent.
%%% @end
manage_request({Config, NbWorkers, Sender}, awaiting_product) ->
	Capacity= simdr_actor_contract:get_capacity(Config),
	io:format(" ~w < ~w ~n", [NbWorkers, Capacity]),
	%[Awaiting] = simdr_actor_contract:get_option(Config, awaiting),
	[TablePid] = simdr_actor_contract:get_option(Config, ets),
	ets:insert(TablePid, {awaiting, {Sender, erlang:now()}}),
	case NbWorkers < Capacity of
		true -> 
			Sender ! {self(), {control, ok}},
			Workers = NbWorkers+1;
		false -> 
			Workers = NbWorkers
	end,
	{Config, Workers};

%%% Automatic propagation of `in' configuration to next actor
%%% when suplying the `out' option.
%%% @end
manage_request({Config, NbWorkers, _Sender}, {add, out, Out}) ->
 	Out ! {self(), {add, in, self()}},
	%%% Normal request, it does not change NbWorkers value
	%spawn(?MODULE, logical_work, [self(), Config, {add, out, Out}]),
	FullAnswer = 
		(simdr_actor_contract:get_module(Config)):answer(Config, {add, out, Out}),
	{NewConfig, LittleAnswer, Destination} = FullAnswer,
	send_message(Config, LittleAnswer, Destination),
	{NewConfig, NbWorkers};
%%% Automatic register when receiving a supervisor.
manage_request({Config, NbWorkers, _Sender}, {add, option, {supervisor, V}}) ->

	%%% Normal request, it does not change NbWorkers value
	%spawn(?MODULE, logical_work, [self(), Config, Request]),
	 FullAnswer = 
		(simdr_actor_contract:get_module(Config)):answer(Config, {add, option, {supervisor, V}}),
	{NewConfig, _LittleAnswer, _Destination} = FullAnswer,
	V ! {self(), {add, actor, Config}},
	% send_message(Config, LittleAnswer, Destination),
	{NewConfig, NbWorkers};
%%% If the request is not about products, then it's not about a
%%% physical stream... so we launch a 'logical' work, directed at the 
%%% supervisor in the end.
%%% @end
manage_request({Config, NbWorkers, _Sender}, Request) ->
	%%% Normal request, it does not change NbWorkers value
	%spawn(?MODULE, logical_work, [self(), Config, Request]),
	FullAnswer = 
		(simdr_actor_contract:get_module(Config)):answer(Config, Request),
	{NewConfig, LittleAnswer, Destination} = FullAnswer,
	send_message(Config, LittleAnswer, Destination),
	{NewConfig, NbWorkers}.

send_message(Config, Ans, RawDest) ->
	Destination = get_destination(Config, RawDest),
	sender(Ans, Destination).

get_destination(Config, supervisor) ->
	%%% Find supervisor PID in ETS option table.
	case simdr_actor_contract:get_option(Config, supervisor) of
		[SuperPid] when is_pid(SuperPid) ->
			SuperPid;
		_ ->
			supervisor
	end;
get_destination(_Config, [Dest]) when is_pid(Dest) -> 
	Dest;
get_destination(_Config, Dest) when is_pid(Dest) ->
	Dest;
get_destination(_Config, WeirdDestination) ->
	%% @TODO: choose destination
	io:format("~w COULDN'T send  message to ~w because of BAD FORMAT. " ++ 
		"Using supervisor.~n~n", [self(), WeirdDestination]),
	supervisor.

sender(Ans, supervisor) ->
	io:format("~w send ~w to ~w.~n~n", [self(), Ans, supervisor]);
sender(Ans, Dest) ->
	io:format("~w send ~w to ~w.~n~n", [self(), Ans, Dest]),
	Dest ! {self(), Ans}.


wait(Pid, Wait_time, {Ans, [Dest]}) when is_pid(Dest)->
	simdr_actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(Pid, Wait_time, {Ans, Dest}) when is_pid(Dest)->
	simdr_actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(Pid, Wait_time, {Ans, Dest}) ->
	simdr_actor_contract:work(Wait_time),
	io:format(" ~w send ~w to ~w.~n~n", [Pid, Ans, Dest]).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

% mock_actor_create() ->
% 	simdr_actor_contract:create(mocking_actor, 
% 		777, 
% 		[], 
% 		off, 
% 		1, 
% 		[]).

get_destination_test_() ->
	[
		?_assertEqual(self(), get_destination(void, [self()])),
		?_assertEqual(self(), get_destination(void, self())),
		?_assertEqual(supervisor, get_destination(void, [])),
		?_assertEqual(supervisor, get_destination(void, [self(), self()]))
	].

-endif.

