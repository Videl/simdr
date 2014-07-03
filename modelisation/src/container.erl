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
			?DLOG(
				actor_contract:get_module(Config), 
				"Entering processing state."),
			NewConfig = actor_contract:set_id(Config, self()),
			processing(actor_contract:set_state(NewConfig, on), 0);
		{Sender, actor_product, _, _} when is_pid(Sender) ->
			Sender ! {state, actor_contract:get_state(Config)},
			idling(Config);
		_ ->
			idling(Config)
	end.


processing(Config, NbWorkers) ->
	receive

		% Dead code, should be removed
		% {Sender, {control, full, {Wait_time, Request}}} ->
		% 	spawn(?MODULE, wait, [self(), Wait_time, {Request, Sender}]),
		% 	processing(Config, NbWorkers);
		%  @end

		{_Sender, {prob_in, Decision}} ->
			{_In, Out} = actor_contract:get_in_out(Config),
			NewIn = Decision,
			NewConfig = actor_contract:set_in_out(Config, {NewIn, Out}),
			NewIn ! {self(), {control, ok}},
			processing(NewConfig, NbWorkers);

		{_Sender, {prob_out, Prod, Decision}} ->
			FullAnswer = (actor_contract:get_module(Config)):answer(Config,{prob_out, Prod, Decision}),
			io:format(" Prob out Answer : ~w ~n", [FullAnswer]),
			{NewConfig, NewNbWorkers} = end_of_physical_work(
				{Config, NbWorkers}, 
				FullAnswer),
			processing(NewConfig, NewNbWorkers);

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
		(actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_physical_work, FullAnswer}.


logical_work(Master, MasterConfig, Request) ->
	FullAnswer = 
		(actor_contract:get_module(MasterConfig)):answer(MasterConfig, Request),
	Master ! {self(), end_logical_work, FullAnswer}.

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
	[TablePid] = actor_contract:get_option(Config, ets), 
	{actor_product, ProductConfig, Detail} = LittleAnswer,
	?DLOG(
		actor_contract:get_module(Config), 
		{work,done,on,product,ProductConfig}),
	io:format("Container >>> Work is done on product id ~p.\n", [actor_contract:get_id(ProductConfig)]),
	actor_contract:add_data(
		NewConfig, 
		{work,on,product,is,done,{ProductConfig, Detail}}), 
	actor_contract:add_data(
		ProductConfig, 
		{processing,done,by,NewConfig}),
	io:format(" ~w, ~w finish to work product : ~w ~n ~n",
		[actor_contract:get_module(NewConfig), 
		 actor_contract:get_id(NewConfig), 
		 actor_contract:get_id(ProductConfig)]),
	ets:insert(TablePid, {product, awaiting_sending, ProductConfig}),
	%io:format("await"),
 	send_message(awaiting_product, Destination),
	[Awaiting] = actor_contract:get_option(NewConfig, awaiting),
	case Awaiting > 0 of
		true -> 
			case actor_contract:list_size(actor_contract:get_in(Config)) of 
				1 ->
					[InActor] = actor_contract:get_in(Config),
					InActor ! {self(), {control, ok}};
				_ ->
					%%% Sending message to supervisor 
					send_message({Config, prob_in}, supervisor)
			end;
		false -> 
			%%% No new products incoming.
			wait
	end,
	{actor_contract:set_state(NewConfig, free), NbWorkers-1}.


end_of_logical_work(
					{_Config, NbWorkers}, 
					{NewConfig, LittleAnswer, Destination}) ->
	send_message(LittleAnswer, Destination),
	{NewConfig, NbWorkers}.

%%% Receiving a product
%%% This function is called when we receive a product, and we ALWAYS receive a
%%% product when we ask for it first.
%%% So Awaiting > 0 when we are here, hopefully..
%%% Returns: {NewConfig, NewNbWorkers}
%%% @end
manage_request({Config, NbWorkers, _Sender}, {actor_product, ProdConf}) ->
	io:format("~w receive product ~w ~n~n", 
		[self(), 
		 actor_contract:get_id(ProdConf)]),
	?DLOG(
		actor_contract:get_module(Config), 
		{starting,to,work,on,product,ProdConf}),
	io:format("Container >>> Work is starting on product id ~p.\n", [actor_contract:get_id(ProdConf)]),
	actor_contract:add_data(Config, {new,product,has,arrived, {ProdConf}}), 
	actor_contract:add_data(ProdConf, {processing,started,by,Config}),
	%%% Decrement the number of products waiting for us.
	[Awaiting] = actor_contract:get_option(Config, awaiting),
	case Awaiting > 0 of 
		true ->
			NewConfig = actor_contract:set_option(Config, awaiting, Awaiting-1);
		false ->
			NewConfig = Config
	end,
	Request = {actor_product, ProdConf},
	spawn(?MODULE, physical_work, [self(), NewConfig, Request]),
	NewWorker = NbWorkers + 1,
	{NewConfig, NewWorker};
%%% Receiving request of a product from actor in `out'.
%%% @end
manage_request({Config, NbWorkers, Sender}, {control, ok}) ->
	io:format("~w receive request of product~n~n", [self()]),
	[TablePid] = actor_contract:get_option(Config, ets),
	ListEntry = ets:match_object(
					TablePid, {product, awaiting_sending, '$1'}
				),
	case actor_contract:list_size(ListEntry) > 0 of
		true ->
			%io:format("Sending product..~n"),
			FirstEntry = actor_contract:first(ListEntry),
			{product, awaiting_sending, Prod} = FirstEntry,
			actor_contract:add_data(Config, {sending, product, {Prod}}), 
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
%%% @end
manage_request({Config, NbWorkers, Sender}, awaiting_product) ->
	Capacity= actor_contract:get_capacity(Config),
	case NbWorkers < Capacity of 
		true -> 
			Sender ! {self(), {control, ok}},
			NewConfig = Config;
		false -> 
			io:format(" a product is waiting ~n"),
			[Awaiting] = actor_contract:get_option(Config, awaiting),
			NewConfig = actor_contract:set_option(Config, awaiting, Awaiting+1)
	end,
	{NewConfig, NbWorkers};
%%% Automatic propagation of `in' configuration to next actor
%%% when suplying the `out' option.
%%% @end
manage_request({Config, NbWorkers, _Sender}, {add, out , Out}) ->
 	Out ! {self(), {add, in, self()}},
	%%% Normal request, it does not change NbWorkers value
	spawn(?MODULE, logical_work, [self(), Config, {add, out , Out}]),
	{Config, NbWorkers};
%%% If the request is not about products, then it's not about a
%%% physical stream... so we launch a 'logical' work, directed at the 
%%% supervisor in the end.
%%% @end
manage_request({Config, NbWorkers, _Sender}, Request) ->
	%%% Normal request, it does not change NbWorkers value
	spawn(?MODULE, logical_work, [self(), Config, Request]),
	{Config, NbWorkers}.

send_message(Ans, RawDest) ->
	Destination = get_destination(RawDest),
	sender(Ans, Destination).

get_destination(supervisor) ->
	supervisor;
get_destination([Dest]) when is_pid(Dest) -> 
	Dest;
get_destination(Dest) when is_pid(Dest) ->
	Dest;
get_destination(WeirdDestination) ->
	%% @TODO: choose destination
	io:format(
		"~w COULDN'T send  message to ~w because of BAD FORMAT. Using supervisor.~n~n", 
		[self(), WeirdDestination]),
	supervisor.

sender(Ans, supervisor) ->
	io:format("~w send ~w to ~w.~n~n", [self(), Ans, supervisor]);
sender(Ans, Dest) ->
	io:format("~w send ~w to ~w.~n~n", [self(), Ans, Dest]),
	Dest ! {self(), Ans}.


wait(Pid, Wait_time, {Ans, [Dest]}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(Pid, Wait_time, {Ans, Dest}) when is_pid(Dest)->
	actor_contract:work(Wait_time),
	Dest ! {Pid, {Ans}};
wait(Pid, Wait_time, {Ans, Dest}) ->
	actor_contract:work(Wait_time),
	io:format(" ~w send ~w to ~w.~n~n", [Pid, Ans, Dest]).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

% mock_actor_create() ->
% 	actor_contract:create(mocking_actor, 
% 		777, 
% 		[], 
% 		off, 
% 		1, 
% 		[]).

get_destination_test_() ->
	[
		?_assertEqual(self(), get_destination([self()])),
		?_assertEqual(self(), get_destination(self())),
		?_assertEqual(supervisor, get_destination([])),
		?_assertEqual(supervisor, get_destination([self(), self()]))
	].


-endif.
