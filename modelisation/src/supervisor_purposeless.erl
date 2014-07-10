-module(supervisor_purposeless).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	 create/1,
	 timer_time/1,
	 timer_action/1,
	 action_on_request/3
	]).

create(Actor) ->
    Ac1 = supervisor_contract:create(?MODULE),
    Ac2 = Ac1#supervisor{actors = [Actor]},
    Ac2.

%%% Disable timer.
timer_time(_Config) ->
    666.

timer_action(Config) ->
	Config.



action_on_request(Config, Sender, {ActorConfig, {actor_product, Product, prob_out}}) ->
	[H|_Rest] = actor_contract:get_out(ActorConfig),
 	Sender ! {self(), {prob_out, Product, H}},
 	Config;
 action_on_request(Config, Sender, {ActorConfig, prob_in}) ->
 	[Head|_Rest] = actor_contract:get_in(ActorConfig),
	Sender ! {self(), {prob_in, Head}},
	Config;
%%% Default behaviour
action_on_request(Config, Sender, Request) ->
	supervisor_contract:action_on_request(Config, Sender, Request).