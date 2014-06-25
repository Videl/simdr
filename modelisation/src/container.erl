-module(container).
-include_lib("eunit/include/eunit.hrl").

% config record
-include("config.hrl").

-export([wait/3,
		 idling/1,
		 processing/2]).

%% ===================================================================
%% Loops functions
%% ===================================================================

idling(Config) ->
	receive
		{start} ->
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
			NewWorkers = manage_request({Config, NbWorkers, Sender}, Request),
			processing(Config, NewWorkers);
		
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

		_ ->
			?MODULE:processing(Config, NbWorkers)
	end.


end_of_physical_work({_Config, NbWorkers}, {NewConfig, LittleAnswer, Destination}) ->
	%% Set up the item in the ETS table
	%% for later sending.
	{actor_product, ConfProd, _} = LittleAnswer,
	send_message({{actor_product, ConfProd}, Destination}),
	%% @todo: Change config
	{NewConfig, NbWorkers-1}.


end_of_logical_work({_Config, NbWorkers}, {NewConfig, LittleAnswer, Destination}) ->
	send_message({LittleAnswer, Destination}),
	{NewConfig, NbWorkers}.


manage_request({Config, NbWorkers, Sender}, {actor_product, ProdConf}) ->
	Request = {actor_product,ProdConf},
	[N] = actor_contract:get_option(Config, capacity),
	case (NbWorkers+1) > N of
		false ->
			Sender ! { self(), {control, ok, Request}},
			spawn(?MODULE, physical_work, [self(), Config, Request]),
			NewWorker = NbWorkers + 1;
		_-> 
			Sender ! {self(), {control, full, {actor_contract:get_work_time(Config), Request}}},
			NewWorker = NbWorkers
	end,
	NewWorker;
manage_request({Config, NbWorkers, _Sender}, Request) ->
	%%% Normal request, it does not change NbWorkers value
	spawn(?MODULE, logical_work, [self(), Config, Request]),
	NbWorkers.


send_message({Ans, [Dest]}) when is_pid(Dest) -> 
	io:format("Sending: ~w, ~w.~n", [self(), {Ans}]),
	Dest ! {self(), {Ans}};
send_message({Ans, Dest}) ->
	%% @TODO: decider de la destination
	io:format("Sending: ~w to ~w.~n", [Ans, Dest]).


wait(Pid, Wait_time, {Ans, [Dest]}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(Pid, Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(_Pid, Wait_time, {Ans, Dest}) ->
	actor_contract:work(Wait_time),
	io:format("Sending: ~w to ~w.~n", [Ans, Dest]).
