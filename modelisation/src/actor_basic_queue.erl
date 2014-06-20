-module(actor_basic_queue).
-include_lib("eunit/include/eunit.hrl").

-behaviour(actor_contract).
-include("config.hrl").
-include("debug.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

-export([
	create/1,
	init/0,
	idling/1,
	powered/1,
	processing/1,
	worker_loop/3]).

%% Behavior implementation
create() ->
	create(void).

create(Out) ->
	TablePid = ets:new(test2, [duplicate_bag, public]),
	Ac1 = actor_contract:add_option(
			  actor_contract:create(?MODULE, 'BasicQueue', 10),
			  ets,
			  TablePid),
	Ac2 = actor_contract:add_option(Ac1, out, Out),
	Ac2.

%% Possible answer: a new product arriving
answer(BasicQueueConfig, {actor_product, ProductConfig, register}) ->
	%% Add it to ETS
	%% @TODO: time ?!
	[TablePid] = actor_contract:get_option(BasicQueueConfig, ets),
	ets:insert(TablePid, {product, awaiting_processing, ProductConfig}),
	%% @TODO: Add it to list_data (this actor + the product) to know we had it
	{BasicQueueConfig, {actor_product, ProductConfig, saved_ets}, main_loop};
answer(BasicQueueConfig, Request) ->
	actor_contract:answer(BasicQueueConfig, Request).


%% Main loop
init() ->
	?CREATE_DEBUG_TABLE,
	?DLOG("Basic Queue Actor initialisation."),
	Conf = ?MODULE:create(),
	spawn(?MODULE, idling, [Conf]).


%% Awaiting the start command
idling(Config) ->
	receive
		{start} ->
			?MODULE:powered(actor_contract:set_state(Config, on));
		{Sender, actor_product, _, _} when is_pid(Sender) ->
			Sender ! {state, actor_contract:get_state(Config)},
			?MODULE:idling(Config);
		_ ->
			?MODULE:idling(Config)
	end.

%% Awaiting a new product
powered(Config) ->
	receive
		{stop} ->
			?MODULE:idling(actor_contract:set_state(Config, off));
		{_Sender, Request} -> 
			io:format("received request~n"),
			spawn(?MODULE, worker_loop, [self(), Config, Request]),
			?MODULE:processing(
				actor_contract:set_state(Config, processing));
		_ ->
			?MODULE:powered(Config)
	end.

%% Awaiting new products
%% Trying to send them over to a work station
processing(Config) ->
	receive
		{Sender, Request} ->
			%io:format("received request~n"),
			spawn(?MODULE, worker_loop, [self(), Config, Request]),
			?MODULE:processing(Config);
		{_Pid, end_of_work, {NewConfig, LittleAnswer, Destination}} ->
			% Being here means: a new product has arrived to my attention and 
			% 					has been set up in ETS.
			% So there is quite nothing to do here.
			%io:format("request done~n"),
			?MODULE:processing(actor_contract:set_state(NewConfig, processing));
		_ ->
			?MODULE:processing(Config)
		after make_up_wait_time(Config) ->
			%% Try to send the first product arrived in the queue to
			%% the workstation, that should be in out.
			[WS] = actor_contract:get_option(Config, out),
			%% Send message to know if it's available
			WS ! {self(), free_space, {product}},
			receive
				{WS, yes, {}} ->
					%% yes -> send an item
					%%        1) fetch the first item from ets
					%%        2) send it
		            %%		  3) if product is acknowledged,
					%%        	4) remove it from ets, add a new entry
					%% 
					%%		  	4')else nothing (this step should not happen,
					%%							 but who knows)
					%% 1)
					[TablePid] = actor_contract:get_option(Config, ets),
					ListEntry = ets:match_object(
									test, {product, awaiting_processing, '$1'}
								),
					FirstEntry = actor_contract:first(ListEntry),
					{product, awaiting_processing, Prod} = FirstEntry,
					%% 2)
					WS ! {self(), {actor_product, Prod, transformation}},
					%% 3)
					receive
							{WS, {control, acknowledged, actor_product}} ->
								%% 4)
								ets:delete_object(TablePid, FirstEntry),
								ets:insert(TablePid, {product, sent, Prod});
							{WS, _} ->
								ets:insert(TablePid, {product, error, d})
					end;
				{WS, no, {}} ->
					%% no -> do nothing, let's wait a bit before asking again
					ok
			end,
			?MODULE:processing(Config)
	end.


worker_loop(Master, MasterConfig, Request) ->
	FullAnswer = ?MODULE:answer(MasterConfig, Request),
	Master ! {end_of_work, FullAnswer}.

%% Internal API

make_up_wait_time(Config) ->
	actor_contract:get_work_time(Config)*1000.

%% Tests