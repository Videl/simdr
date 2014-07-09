-module(supervisor_start).
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

%%% Frequence of new product
timer_time(_Config) ->
    5.

timer_action(Config) ->
    List = Config#supervisor.actors,
    % Fetch head
    [H|_T] = List,
    % Create new product
    Product = actor_product:create(),
    % Log the product
    supervisor_contract:add_data(Config, {created, product}, Product),
    H ! {self(), {actor_product, Product}},
    Config.

%%% Nothing special to do
action_on_request(Config, Sender, Request) ->
	supervisor_contract:action_on_request(Config, Sender, Request).