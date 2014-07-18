-module(simdr_supervisor_purposeless).
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

create(Name) ->
    Ac1 = simdr_supervisor_contract:create(?MODULE, Name),
    Ac1.

%%% Disable timer.
timer_time(_Config) ->
    666.

timer_action(Config) ->
	Config.

action_on_request(Config, Sender, {out, Out, added})->
	%%% Base behaviour
	Config2 = simdr_supervisor_contract:action_on_request(Config, 
													Sender, 
													{out, Out, added}),
	%%% 1) Fetch the previous values about the actor saved in ETS table,
	%%%    with value 'Sender', mix the new value with the old.
	%%% 2) Set the new value.
	ToSave = case simdr_supervisor_contract:get_option(Config2, Sender) of
				unknown_option ->
					[Out];
				List when is_list(List) ->
					List ++ [Out]
			 end,
	%%% 2)
	ToSaveFlat = lists:flatten(ToSave),
	io:format("~nNew list is: ~w.~n", [ToSaveFlat]),
	Config3 = simdr_supervisor_contract:set_option(Config2, Sender, ToSaveFlat),
	Config3;
action_on_request(Config, Sender, {ActorConfig, {actor_product, Product, prob_out}}) ->
	%%% 1) Fetch what I know from this Actor (through its pid)
	%%%    If I know nothing, I just take all the out info in ActorConfig
	%%% 2) Fetch the head and send it back as solution
	%%% 3) Update what I know about the actor.
	%%% 1)
	ToUse = lists:flatten(case simdr_supervisor_contract:get_option(Config, Sender) of
				unknown_option ->
					simdr_actor_contract:get_out(ActorConfig);
				List when is_list(List) ->
					List
			end), 
	%%% 2)

	[H|Tail] = ToUse, 
 	Sender ! {self(), {prob_out, Product, H}},
 	NewList = Tail ++ [H],
 	Config2 = simdr_supervisor_contract:set_option(Config, Sender, NewList),
 	Config2;
 action_on_request(Config, Sender, {ActorConfig, prob_in}) ->
 	[Head|_Rest] = simdr_actor_contract:get_in(ActorConfig),
	Sender ! {self(), {prob_in, Head}},
	Config;
%%% Default behaviour
action_on_request(Config, Sender, Request) ->
	simdr_supervisor_contract:action_on_request(Config, Sender, Request).