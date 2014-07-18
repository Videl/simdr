-module(simdr_supervisor_onedirection).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_supervisor_contract).

-export([
	 create/0,
	 timer_time/1,
	 timer_action/1,
	 action_on_request/3
	]).

-export([
	create/1
	]).

create() ->
    Ac1 = simdr_supervisor_contract:create(?MODULE),
    Ac1.    

create(Actor) ->
    Ac1 = simdr_supervisor_contract:create(?MODULE),
    Ac2 = Ac1#supervisor{actors = [Actor]},
    Ac2.

%%% Disable timer.
timer_time(_Config) ->
    666.

timer_action(Config) ->
	Config.



action_on_request(Config, Sender, {ActorConfig, {actor_product, Product, prob_out}}) ->
	[H|_Rest] = simdr_actor_contract:get_out(ActorConfig),
 	Sender ! {self(), {prob_out, Product, H}},
 	Config;
 action_on_request(Config, Sender, {ActorConfig, prob_in}) ->
 	[Head|_Rest] = simdr_actor_contract:get_in(ActorConfig),
	Sender ! {self(), {prob_in, Head}},
	Config;
%%% Default behaviour
action_on_request(Config, Sender, Request) ->
	simdr_supervisor_contract:action_on_request(Config, Sender, Request).