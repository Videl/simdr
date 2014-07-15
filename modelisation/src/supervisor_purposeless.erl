-module(supervisor_purposeless).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(supervisor_contract).

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
    Ac1 = supervisor_contract:create(?MODULE),
    Ac1.

create(Actor) ->
    Ac1 = supervisor_contract:create(?MODULE),
    Ac2 = Ac1#supervisor{actors = [Actor]},
    Ac2.

%%% Disable timer.
timer_time(_Config) ->
    666.

timer_action(Config) ->
	Config.

action_on_request(Config, Sender, {state, [Value], status}) ->
	%%% 1) Fetch the previous values about the actor saved in ETS table,
	%%%    with value 'Sender', mix the new value and the old.
	%%% 2) Remove the previous saved value.
	%%% 3) Insert the new value given in the variable Value.
	io:format("YOLO new data from actor! Received ~w.~n", [Value]),
	ToSave = case supervisor_contract:get_option(Config, Sender) of
				unknown_option ->
					[Value];
				List when is_list(List) ->
					List ++ [Value]
			 end,
	%%% 2) and 3)
	supervisor_contract:set_option(Config, {Sender, ToSave}),
	Config;
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