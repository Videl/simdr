-module(actor_basic_queue).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, off, 0).

%% Possible answer: a new product arriving
answer(BasicQueueConfig, {actor_product, ProductConfig}) ->
	%% Work time here means 'deplacement time' of the product, when the queue
	%% is used as a conveyor
	actor_contract:work(actor_contract:get_work_time(BasicQueueConfig)),
	{NewBasicQueueConfig, NewProductConfig} = {BasicQueueConfig, ProductConfig},
	% actor_contract:add_to_list_data(
	% 	{BasicQueueConfig, ProductConfig}, 
	% 	{ProductConfig, BasicQueueConfig}),
	Destination = actor_contract:get_out(BasicQueueConfig),
	% Empty to notify the container there is nothing to send, 
	% not even to supervisor.
	{NewBasicQueueConfig, {actor_product, NewProductConfig, buffered}, Destination};
answer(BasicQueueConfig, Request) ->
	actor_contract:answer(BasicQueueConfig, Request).

%% Tests