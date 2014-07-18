-module(actor_basic_queue).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	create/1,
	answer/2]).

%% Behavior implementation

create() ->
	create(simdr_actor_contract:random_id()).

create(Name ) ->
	simdr_actor_contract:create(?MODULE, Name, [], off, 1, []).


%% Possible answer: a new product arriving
answer(BasicQueueConfig, {actor_product, ProductConfig}) ->
	%% Work time here means 'deplacement time' of the product, when the queue
	%% is used as a conveyor
	simdr_actor_contract:work(simdr_actor_contract:get_work_time(BasicQueueConfig)),
	{NewBasicQueueConfig, NewProductConfig} = simdr_actor_contract:add_to_list_data(
		BasicQueueConfig, {{product,entered,queue},{ProductConfig}}, 
		ProductConfig, {{entered,queue},{BasicQueueConfig}}),
	Destination = simdr_actor_contract:get_out(BasicQueueConfig),
	% Empty to notify the container there is nothing to send, 
	% not even to supervisor.
	{NewBasicQueueConfig, {actor_product, NewProductConfig, buffered}, Destination};
answer(BasicQueueConfig, Request) ->
	simdr_actor_default:answer(BasicQueueConfig, Request).

%% Tests