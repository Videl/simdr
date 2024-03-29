-module(simdr_supervisor_end).
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
    ]).

create() ->
    Ac1 = simdr_supervisor_contract:create(?MODULE),
    Ac1.

%%% Frequence of new product
timer_time(_Config) ->
    666.

timer_action(Config) ->
    Config.

action_on_request(Config, Sender, awaiting_product) ->
    Sender ! {self(), {control, ok}},
    Config;
action_on_request(Config, _Sender, {actor_product, Product}) ->
    simdr_actor_contract:add_data(Product, {{product,arrived},{end_of_path}}),
    Config;
%%% Nothing special to do
action_on_request(Config, Sender, Request) ->
    simdr_supervisor_default:action_on_request(Config, Sender, Request).
