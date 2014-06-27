-module(container).
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
			?DLOG(actor_contract:get_module(Config), "Entering processing state."),
			processing(actor_contract:set_state(Config, on), 0);
		{Sender, actor_product, _, _} when is_pid(Sender) ->
			Sender ! {state, actor_contract:get_state(Config)},
			idling(Config);
		_ ->
			idling(Config)
	end.


processing(Config, NbWorkers) ->
	receive
		{Sender, {control, full, {Wait_time, Request}}} ->
			spawn(?MODULE, wait, [self(), Wait_time, {Request, Sender}]),
			processing(Config, NbWorkers);

		{Sender, Request} ->
			io:format("~w Received >>> ~w~n", [self(), Request]),
			{NewConfig, NewWorkers} = manage_request({Config, NbWorkers, Sender}, Request),
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
			processing(NewConfig, NewNbWorkers);

		V ->
			io:format(">>>UNKNOW REQUEST<<< (~w)~n", [V]),
			?MODULE:processing(Config, NbWorkers)
	end.


physical_work(Master, MasterConfig, Request) ->
	io:format("<?.??.?> ~w work ~w.~n", [actor_contract:get_module(MasterConfig), 
								{MasterConfig, Request}]),
	FullAnswer = (actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_physical_work, FullAnswer}.


logical_work(Master, MasterConfig, Request) ->
	FullAnswer = (actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_logical_work, FullAnswer}.


end_of_physical_work({Config, NbWorkers}, {NewConfig, LittleAnswer, Destination}) ->
	[TablePid] = actor_contract:get_option(Config, ets), 
	{actor_product, ConfProd, _} = LittleAnswer,
	ets:insert(TablePid, {product, awaiting_sending, ConfProd}),
	%io:format("await"),
 	send_message(awaiting_product, Destination),
	[Awaiting] = actor_contract:get_option(Config, awaiting),
	%io:format("~n~n Awaiting (~w): ~w ~n~n", [actor_contract:get_module(Config), Awaiting]),
	case Awaiting > 0 of
		true -> 
			%io:format("~n~n TRUE == ~w > 0 ~n~n", [Awaiting]),
			[InActor] = actor_contract:get_option(Config, in),
			%io:format("So I send the message to... ~w~n", [InActor]),
			InActor ! {self(), {control, ok}};
		false -> 
			%io:format("~n~n FALSE: ~w > 0 ~n~n", [Awaiting]),
			%io:format("J'attends.~n"),
		wait
	end,
	%send_message({actor_product, ConfProd}, Destination),
	{actor_contract:set_state(NewConfig, free), NbWorkers-1}.


end_of_logical_work({_Config, NbWorkers}, {NewConfig, LittleAnswer, Destination}) ->
	send_message(LittleAnswer, Destination),
	{NewConfig, NbWorkers}.

%%% Receiving a product
%%% Returns: {NewConfig, NewNbWorkers}
%%% @end
manage_request({Config, NbWorkers, _Sender}, {actor_product, ProdConf}) ->
	io:format("~w Receiving product (~w)..~n", [self(), actor_contract:get_module(Config)]),
	[Awaiting] = actor_contract:get_option(Config, awaiting),
	case Awaiting > 0 of 
		true ->  NewConfig = actor_contract:set_option(Config, awaiting, Awaiting-1);
		false -> NewConfig = Config
	end,
	Request = {actor_product, ProdConf},
	% [N] = actor_contract:get_option(Config, capacity),
	% case (NbWorkers+1) > N of
	% 	false ->
	% 		Sender ! { self(), {control, ok, Request}},
			spawn(?MODULE, physical_work, [self(), NewConfig, Request]),
			NewWorker = NbWorkers + 1,
	% 	_-> 
	% 		Sender ! {self(), {control, full, {actor_contract:get_work_time(Config), Request}}},
	% 		NewWorker = NbWorkers
	% end,
	{NewConfig, NewWorker};
%%% Receiving request of a product from actor in `out'.
manage_request({Config, NbWorkers, Sender}, {control, ok}) ->
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
			send_message({actor_product, Prod}, Sender),
			ets:delete_object(TablePid, FirstEntry),
			ets:insert(TablePid, {product, sent, Prod}),
			%io:format("End of sending product..~n"),
			NewConfig = actor_contract:set_state(Config, free),
			NewNbWorkers = NbWorkers-1;

		false ->
			%%% No change
			NewConfig = Config,
			NewNbWorkers = NbWorkers
	end,
	{NewConfig, NewNbWorkers};
%%% Receiving a notification from one of my actor in `in' that
%%% a product can be sent.
manage_request({Config, NbWorkers, Sender}, awaiting_product) ->
	[Capacity]= actor_contract:get_option(Config, capacity),
	%io:format("NbWorkers: ~w/Capacity: ~w~n", [NbWorkers, Capacity]),
	case NbWorkers < Capacity of 
		true -> 
			Sender ! {self(),{control, ok}},
			%io:format("JE SUIS ~w ET JE VEUX UN PRODUIT!~n", [self()]),
			NewConfig = Config;
		false -> 
			[Awaiting] = actor_contract:get_option(Config, awaiting),
			NewConfig = actor_contract:set_option(Config, awaiting, Awaiting+1)
	end,
	{NewConfig, NbWorkers};
%%% If the request is not about products, then it's not about a
%%% physical stream... then we launch a 'logical' work, directed at the 
%%% supervisor in the end.
manage_request({Config, NbWorkers, _Sender}, Request) ->
	%%% Normal request, it does not change NbWorkers value
	spawn(?MODULE, logical_work, [self(), Config, Request]),
	{Config, NbWorkers}.


send_message(Ans, [Dest]) when is_pid(Dest) -> 
	io:format("~w Container Sending: ~w to ~w.~n", [self(), {self(), Ans}, Dest]),
	Dest ! {self(), Ans};
send_message(Ans, Dest) when is_pid(Dest) -> 
	io:format("~w Container Sending: ~w to ~w.~n", [self(), {self(), Ans}, Dest]),
	Dest ! {self(), Ans};
send_message(Ans, Dest) ->
	%% @TODO: choose destination
	io:format("~w Container Sending: ~w to ~w.~n", [self(), {self(), Ans}, Dest]).


wait(Pid, Wait_time, {Ans, [Dest]}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(Pid, Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(_Pid, Wait_time, {Ans, Dest}) ->
	actor_contract:work(Wait_time),
	io:format("Container Sending: ~w to ~w.~n", [Ans, Dest]).

%% Tests

-ifdef(TEST).

-endif.