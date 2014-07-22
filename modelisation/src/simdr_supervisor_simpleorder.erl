-module(simdr_supervisor_simpleorder).
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
	create/2
	]).

create() ->
    Ac1 = simdr_supervisor_contract:create(?MODULE),
    Ac1.

create(Name, Order) ->
    Ac1 = simdr_supervisor_contract:create(?MODULE, Name),
    Ac2 = simdr_supervisor_contract:set_option(Ac1, order, Order),
    Ac2.

%%% Disable timer.
timer_time(_Config) ->
    666.

timer_action(Config) ->
	Config.

action_on_request(Config, Sender, {out, Out, added})->
	%%% Base behaviour
	Config2 = simdr_supervisor_default:action_on_request(Config, 
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

	Order = get_first_order(Config),
	{_Quality, _Assembly} = Order,
	Outputs = simdr_actor_contract:get_out(ActorConfig),

	Actor = simdr_supervisor_contract:get_actor(Config, Outputs),
	
	ProductState =simdr_actor_contract:get_state(Product), %%processed, assembled or finished
	% case ProductState of
	% 	processed -> 
	% 	assembled ->
	% 	finished -> 
	% end.
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
	simdr_supervisor_default:action_on_request(Config, Sender, Request).


get_first_order(Config) -> 
	Orders = simdr_supervisor_contract:get_option(Config, order),
	first(Orders).

first([]) -> 
	[];

first([H|_Rest]) ->
 	H.

lookup_module(Config, Actor, Module)->
	case simdr_actor_contract:get_module(Actor) of
			Module -> {Actor, 0};
			_ -> Outputs = simdr_actor_contract:get_out(Actor),
				 [H|_Rest] = Outputs,
				Actor2= simdr_supervisor_contract:get_actor(Config, H),
				lookup_module_helper(Config, Actor2, Module, simdr_actor_contract:get_work_time(Actor))
	end.

lookup_module_helper(_Config, [], _Module, _Time) ->
	{[], 9999};

lookup_module_helper(Config, Actor, Module, Time) ->
	case simdr_actor_contract:get_module(Actor) of
			Module -> {Actor, Time};
			_ -> Outputs = simdr_actor_contract:get_out(Actor),
				case   simdr_supervisor_contract:list_size(Outputs) of
					1 ->[H|_Rest] = Outputs,
					Actor2= simdr_supervisor_contract:get_actor(Config, H),
					lookup_module_helper(Config, H, Module, Time+simdr_actor_contract:get_work_time(Actor));
					_-> loop(Outputs, simdr_supervisor_contract:list_size(Outputs), Config, Module, Time)
				end
	end.


loop(ListOut, 2, Config, Module, Time)->
[H| Rest] =ListOut,
[H2|Rest2] = Rest,
{Ac, Time} = lookup_module_helper(Config, H, Module, Time),
{Ac2, Time2} = lookup_module_helper(Config, H2, Module, Time),
	case Time<Time2 of 
		 true -> {Ac, Time};
		 false -> {Ac2, Time2}
	end;

loop(ListOut, _Size, Config, Module, Time) ->
[H| Rest] =ListOut,
[H2|Rest2] = Rest,
{_Ac, Time} = lookup_module_helper(Config, H, Module, Time),
{_Ac2, Time2} = lookup_module_helper(Config, H2, Module, Time),
	case Time<Time2 of 
		 true -> NewList = [H]++Rest2,
		 	loop(NewList, simdr_supervisor_contract:list_size(NewList), Config, Module, Time);
		 false-> loop(Rest, simdr_supervisor_contract:list_size(Rest), Config, Module, Time2)
	end.