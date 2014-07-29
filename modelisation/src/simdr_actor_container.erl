-module(simdr_actor_container).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
		 idling/1,
		 processing/1,
		 physical_work/3,
		 logical_work/3]).

init(Config) ->
	?CREATE_DEBUG_TABLE,
	process_flag(trap_exit, true),
	idling(Config).


%% ===================================================================
%% Loops functions
%% ===================================================================

%%% @doc Initial loop of a container node used with an Actor configuration.
%%% This function mainly starts the function and waits for the {start} message.
%%% The pid and state field values are altered when the function goes on
%%% to processing/2.
%%% @see processing/2
%%% @end
idling(Config) ->
	receive
		{start} ->
			?DLOG(
				simdr_actor_contract:get_module(Config), 
				"Entering processing state."),
			NewConfig = simdr_actor_contract:set_pid(Config, self()),
			NewConfig2 = simdr_actor_contract:set_option(NewConfig, workers, 0),
			processing(simdr_actor_contract:set_state(NewConfig2, awaiting));
		{Sender, actor_product, _, _} when is_pid(Sender) ->
			Sender ! {state, simdr_actor_contract:get_state(Config)},
			idling(Config);
		_ ->
			idling(Config)
	end.

%%% @doc Main loop of a container used with an Actor configuration.
%%%      This loop has a main `receive ... end' that awais any request.
%%%      Requests can be of two types: physical and logical.
%%%      The function manage_request/2 makes the distinction.
%%% @end
processing(Config) ->
	receive
		{_Sender, {prob_in, Decision}} ->
			{_In, Out} = simdr_actor_contract:get_in_out(Config),
			NewIn = Decision,
			NewConfig = simdr_actor_contract:set_in_out(Config, {NewIn, Out}),
			NewIn ! {self(), {control, ok}},
			processing(NewConfig);

		{_Sender, {prob_out, Prod, Decision}} ->
			spawn(?MODULE, physical_work, [self(), Config, {prob_out, Prod, Decision}]),
			processing(Config);

		{Sender, Request} ->
			NewConfig = manage_request({Config, Sender}, Request),
			processing(NewConfig);
		
		{_Worker, end_physical_work, Request} ->
			NewConfig = end_of_physical_work(Config, Request),
			processing(NewConfig);

		{_Worker, end_logical_work, Request} ->
			NewConfig = end_of_logical_work(Config, Request),
			?DLOG({configuration,has,changed,{NewConfig}}),
			processing(NewConfig);

		{'EXIT', Pid, Reason} ->
			io:format("An actor died (~w). His reason was: ~w.~n", [Pid, Reason]),
			exit(Reason);

		V ->
			?DFORMAT(">>>UNKNOWN REQUEST<<< (~w)~n", [V]),
			?MODULE:processing(Config)
	end.


physical_work(Master, MasterConfig, Request) ->
	FullAnswer = 
		(simdr_actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_physical_work, FullAnswer}.


logical_work(Master, MasterConfig, Request) ->
	FullAnswer = 
		(simdr_actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_logical_work, FullAnswer}.

%% ===================================================================
%% Internal API
%% ===================================================================


%%% If there is a problem of destinations, a specific messsage is sent to 
%%% supervisor which take a decision.
end_of_physical_work({_Config},{NewConf, {actor_product, Product, prob_out}, Dest}) ->
	send_message(NewConf, {NewConf,{actor_product, Product, prob_out}}, Dest),
	NewConf;
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
end_of_physical_work(Config, {WorkConfig, LittleAnswer, Destination}) ->
	[TablePid] = simdr_actor_contract:get_option(Config, ets), 
	{actor_product, ProductConfig, _Detail} = LittleAnswer,
	Awaiting = ets:match_object(TablePid, {awaiting, '$1'}),
	?DLOG(
		simdr_actor_contract:get_module(Config), 
		{work,done,on,product,ProductConfig}),
	% ?DFORMAT("~w >>> Work is done on product id ~p.\n", 
	% 	[simdr_actor_contract:get_module(WorkConfig), simdr_actor_contract:get_name(ProductConfig)]),
	simdr_actor_contract:add_data(
		WorkConfig, 
		{{work,on,product,is,done},{ProductConfig}}), 
	simdr_actor_contract:add_data(
		ProductConfig, 
		{{processing,done,by},{WorkConfig}}),
	?DFORMAT(" ~w, ~w finish to work product : ~w ~n ~n",
		[simdr_actor_contract:get_module(WorkConfig), 
		 simdr_actor_contract:get_name(WorkConfig), 
		 simdr_actor_contract:get_name(ProductConfig)]),
	ets:insert(TablePid, {product, awaiting_sending, ProductConfig}),
 	send_message(WorkConfig, awaiting_product, Destination),
 	LastConfig = case simdr_actor_contract:list_size(Awaiting) > 0 of
		true -> 
			case simdr_actor_contract:list_size(simdr_actor_contract:get_in(WorkConfig)) of 
				1 ->
					[InActor] = simdr_actor_contract:get_in(WorkConfig),
					InActor ! {self(), {control, ok}},
					simdr_actor_contract:increment_workers(WorkConfig);
				_ -> 
					case simdr_actor_contract:different_sender(Awaiting) of 
						%%% Sending message to supervisor 
						true ->	
							%%% No chang in workers
							send_message(WorkConfig, {WorkConfig, prob_in}, supervisor),
							WorkConfig;
						false -> 
							[H|_Rest] = Awaiting,
							?DFORMAT("same sender~n"),
							{awaiting, {S, _Date}} = H,
							S ! {self(), {control, ok}},
							simdr_actor_contract:increment_workers(WorkConfig)
					end
			end;
		false -> 
			%%% No new products incoming.
			%%% NbWorker not changed.
			WorkConfig
	end,
	simdr_actor_contract:set_state(LastConfig, awaiting).


end_of_logical_work(_Config, 
					{NewConfig, LittleAnswer, Destination}) ->
	send_message(NewConfig, LittleAnswer, Destination),
	NewConfig.

%%% @doc Taking care of request type actor_product
%%% * Physical: There is a physical item passing between actors.
%%%             Here, it's a product (actor_product), but can be enhanced.
%%% *Logical: Any other thing is a logical request.
%%%           The physical request are then being passed on to logical_work
%%% Receiving a product
%%% This function is called when we receive a product, and we receive a
%%% product when WE ask for it first.
%%% So Awaiting > 0 when we are here, hopefully..
%%% Returns: {NewConfig, NewNbWorkers}
%%% @todo: have a way to program a request to be 'physical' or not.
%%% @end
manage_request({Config, Sender}, {actor_product, ProdConf}) ->
	?DFORMAT("~w receive product ~w ~n~n", 
		[simdr_actor_contract:get_name(Config), 
		 simdr_actor_contract:get_name(ProdConf)]),
	?DLOG(
		simdr_actor_contract:get_module(Config), 
		{starting,to,work,on,product,ProdConf}),
	% ?DFORMAT("~w >>> Work is starting on product id ~p.\n", [simdr_actor_contract:get_module(NewConfig), simdr_actor_contract:get_name(ProdConf)]),
	simdr_actor_contract:add_data(Config, {{new,product,has,arrived}, {ProdConf}}), 
	simdr_actor_contract:add_data(ProdConf, {{processing,started,by},Config}),
	%%% Decrement the number of products waiting for us.
	[TablePid] = simdr_actor_contract:get_option(Config, ets),
	Awaiting = ets:match_object(
					TablePid, {awaiting, '$1'}),
	case simdr_actor_contract:list_size(Awaiting) > 0 of 
		true -> 
			FirstAwaiting = simdr_actor_contract:first_key_awaiting(Awaiting,Sender),
			ets:delete_object(TablePid, FirstAwaiting);
			% Awaiting2 = ets:match_object(
			% 		TablePid, {awaiting, '$1'});
		false ->
			ok
	end,
	Request = {actor_product, ProdConf},
	spawn(?MODULE, physical_work, [self(), Config, Request]),
	Config;
%%% @doc Taking care of request of a product from actor in `out'.
%%% Consequence: one of my product in the waiting list
%%% is sent.
%%% @end
manage_request({Config, Sender}, {control, ok}) ->
	?DFORMAT("~w receive request of product~n~n", [simdr_actor_contract:get_name(Config)]),
	?MFORMAT(Config, "~w receive request of product~n", [simdr_actor_contract:actor_sumup(Config)]),
	[TablePid] = simdr_actor_contract:get_option(Config, ets),
	ListEntry = ets:match_object(
					TablePid, {product, awaiting_sending, '$1'}
				),
	case simdr_actor_contract:list_size(ListEntry) > 0 of
		true ->
			%?DFORMAT("Sending product..~n"),
			FirstEntry = simdr_actor_contract:first(ListEntry),
			{product, awaiting_sending, Prod} = FirstEntry,
			simdr_actor_contract:add_data(Config, {{sending, product}, {Prod}}), 
			simdr_actor_contract:add_data(Prod, {{being,sent,to, Sender, by},{Config}}), 
			send_message(Config, {actor_product, Prod}, Sender),
			ets:delete_object(TablePid, FirstEntry),
			ets:insert(TablePid, {product, sent, Prod}),
			%?DFORMAT("End of sending product..~n"),
			NewConfigBis = simdr_actor_contract:set_state(Config, on),
			NewConfig = simdr_actor_contract:decrement_workers(NewConfigBis);

		false ->
			%%% No change (even workers does not change)
			NewConfig = Config
	end,
	NewConfig;
%%% @doc Taking care of request type notification from one of my actor in `in' 
%%% actor record field that a product can be sent.
%%% @end
manage_request({Config, NbWorkers, Sender}, awaiting_product) ->
	Capacity = simdr_actor_contract:get_capacity(Config),
	[NbWorkers] = simdr_actor_contract:get_option(Config, workers),
	?DFORMAT("~w Capacity: ~w < ~w (current < max)  ~n", [simdr_actor_contract:actor_sumup(Config), 
							   NbWorkers, 
							   Capacity]),
	%[Awaiting] = simdr_actor_contract:get_option(Config, awaiting),
	[TablePid] = simdr_actor_contract:get_option(Config, ets),
	ets:insert(TablePid, {awaiting, {Sender, erlang:now()}}),
	NewConfig = case NbWorkers < Capacity of
		true -> 
			Sender ! {self(), {control, ok}},
			simdr_actor_contract:increment_workers(Config);

		false ->
			% nothing_to_do
			Config
	end,
	NewConfig;

%%% @doc Automatic propagation of `in' configuration to next actor
%%% when suplying the `out' option.
%%% @end
manage_request({Config, _Sender}, {add, out, Out}) ->
 	Out ! {self(), {add, in, self()}},
 	link(Out),
	%%% Normal request, it does not change NbWorkers value
	%spawn(?MODULE, logical_work, [self(), Config, {add, out, Out}]),
	FullAnswer = 
		(simdr_actor_contract:get_module(Config)):answer(Config, {add, out, Out}),
	{NewConfig, LittleAnswer, Destination} = FullAnswer,
	send_message(Config, LittleAnswer, Destination),
	NewConfig;
%%% @doc Automatic register when receiving a supervisor.
%%% @end
manage_request({Config, _Sender}, {add, option, {supervisor, V}}) ->

	%%% Normal request, it does not change NbWorkers value
	%spawn(?MODULE, logical_work, [self(), Config, Request]),
	 FullAnswer = 
		(simdr_actor_contract:get_module(Config)):answer(Config, {add, option, {supervisor, V}}),
	{NewConfig, _LittleAnswer, _Destination} = FullAnswer,
	V ! {self(), {add, actor, Config}},
	% send_message(Config, LittleAnswer, Destination),
	NewConfig;
%%% @doc Taking care of normal request.
%%% If the request is not about products, then it's not about a
%%% physical stream... so we launch a 'logical' work, directed at the 
%%% supervisor in the end.
%%% @end
manage_request({Config, _Sender}, Request) ->
	%%% Normal request, it does not change NbWorkers value
	%spawn(?MODULE, logical_work, [self(), Config, Request]),
	FullAnswer = 
		(simdr_actor_contract:get_module(Config)):answer(Config, Request),
	{NewConfig, LittleAnswer, Destination} = FullAnswer,
	send_message(Config, LittleAnswer, Destination),
	NewConfig.

%%% @doc Send specified message to destionation.
%%% @spec (Actor, Ans, RawDest) -> tuple(pid(), Ans)
%%%        Ans = any()
%%%        RawDest = atom() | pid()
%%% @end
send_message(Config, Ans, RawDest) ->
	Destination = get_destination(Config, RawDest),
	sender(Ans, Destination).

%%% @doc Find the right destination
%%% If supervisor, find supervisor's pid
%%% Or else, try to find the pid in the data.
%%% @end
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
	io:format("Warning: ~w COULDN'T send message to ~w because of BAD FORMAT. " ++ 
		"Message not sent.~n", [self(), WeirdDestination]),
	broken.

sender(Ans, broken) ->
	io:format("Warning: ~w Message was: ~w.~n", [self(), Ans]);
sender(Ans, supervisor) ->
	?DFORMAT("~w send ~w to ~w.~n~n", [self(), Ans, supervisor]);
sender(Ans, Dest) ->
	?DFORMAT("~w send ~w to ~w.~n~n", [self(), Ans, Dest]),
	Dest ! {self(), Ans}.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

get_destination_test_() ->
	[
		?_assertEqual(self(), get_destination(void, [self()])),
		?_assertEqual(self(), get_destination(void, self())),
		?_assertEqual(supervisor, get_destination(void, [])),
		?_assertEqual(supervisor, get_destination(void, [self(), self()]))
	].

-endif.

