%%% @doc Actor Basic Queue
%%% 
%%% This module provides you with a queue Actor: it queues products it
%%% receives then send them accordingly to the capacity of the actor in `out'
%%% field.
%%%
%%% Suitable options: capacity = infinity, only one actor entering in this actor,
%%% only one other actor exiting this actor.
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
-module(simdr_actor_basic_queue).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2
	]).

%% User functions export

-export([
	create/1
	]).

%% Behavior implementation

create() ->
	create(simdr_actor_contract:random_id()).

create(Name) ->
	simdr_actor_contract:create(?MODULE, Name, [], off, 1, []).


%% Possible answer: a new product arriving
answer(BasicQueueConfig, {actor_product, ProductConfig}) ->
	%% Work time here means 'deplacement time' of the product, when the queue
	%% is used as a conveyor
	simdr_actor_contract:work(BasicQueueConfig),
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