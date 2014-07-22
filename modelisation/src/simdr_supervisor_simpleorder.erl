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

lookup_module(Config, Out, Module)->
	Actor = simdr_supervisor_contract:get_actor(Config, Out),
	case simdr_actor_contract:get_module(Actor) of
			Module -> {Actor, 0};
			_ -> Outputs = simdr_actor_contract:get_out(Actor),
				 [H|_Rest] = Outputs,
				Actor2= simdr_supervisor_contract:get_actor(Config, H),
				lookup_module_helper(Config, Actor2, Module, simdr_actor_contract:get_work_time(Actor))
	end.


lookup_module_helper(Config, Actor, Module, Time) ->
	case simdr_actor_contract:get_module(Actor) of
			Module -> {Actor, Time};
			_ -> Outputs = simdr_actor_contract:get_out(Actor),
				case   simdr_supervisor_contract:list_size(Outputs) of
					1 ->[H|_Rest] = Outputs,
					Actor2= simdr_supervisor_contract:get_actor(Config, H),
					lookup_module_helper(Config, Actor2, Module, Time+simdr_actor_contract:get_work_time(Actor));
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


%% ===================================================================
%% Tests
%% ===================================================================
	-ifdef(TEST).
	 lookup_module_test_() ->
	 	Sup = create(),
	 	C11 =  simdr_actor_conveyor:create('C11'),
	 	C12 =  simdr_actor_conveyor:create('C12'),
	 	WS1 = simdr_actor_workstation:create('WS1'),
	 	C21 =  simdr_actor_conveyor:create('C21'),
	 	C22 =  simdr_actor_conveyor:create('C22'),
	 	C23 =  simdr_actor_conveyor:create('C23'),
	 	WS2 = simdr_actor_workstation:create('WS2'),

	 	C31 =  simdr_actor_conveyor:create('C31'),
	 	C32 =  simdr_actor_conveyor:create('C32'),
	 	C33 =  simdr_actor_conveyor:create('C33'),

		PidC11 = simdr_actor_contract:get_pid(C11),
		PidC12 = simdr_actor_contract:get_pid(C12),
		PidWS1 = simdr_actor_contract:get_pid(WS1),
		PidC21 = simdr_actor_contract:get_pid(C21),
		PidC22 = simdr_actor_contract:get_pid(C22),
		PidC23 = simdr_actor_contract:get_pid(C23),
		PidWS2 = simdr_actor_contract:get_pid(WS2),

	 	C11bis = simdr_actor_contract:add_out(C11, PidC12),
	 	C12bis = simdr_actor_contract:add_out(C12, PidWS1),

	 	C21bis = simdr_actor_contract:add_out(C21, PidC22),
	 	C22bis = simdr_actor_contract:add_out(C22, PidC23),
	 	C23bis = simdr_actor_contract:add_out(C23, PidWS2),



	 	Sup1 = simdr_supervisor_contract:add_actor(Sup, {PidC11, C11bis}),
	 	Sup2 = simdr_supervisor_contract:add_actor(Sup1, {PidC12, C12bis}),
	 	Sup3 = simdr_supervisor_contract:add_actor(Sup2, {PidWS1, WS1}),
	 	Sup4 = simdr_supervisor_contract:add_actor(Sup3, {PidWS2, WS2}),
	 	Sup5 = simdr_supervisor_contract:add_actor(Sup4, {PidC21, C21bis}),
	 	Sup6 = simdr_supervisor_contract:add_actor(Sup5, {PidC22, C22bis}),
	 	Sup7 = simdr_supervisor_contract:add_actor(Sup6, {PidC23, C23bis}),

	 	Outputs = simdr_actor_contract:get_out(C11bis),
				 [H|_Rest] = Outputs,
		%Actor2= simdr_supervisor_contract:get_actor(Config, H),
		% lookup_module_helper(Config, Actor2, Module, simdr_actor_contract:get_work_time(Actor))
	 	[
	 	?_assertMatch(C11bis,
	 	simdr_supervisor_contract:get_actor(Sup7, PidC11)),
	 	?_assertMatch(simdr_actor_conveyor, simdr_actor_contract:get_module(C11bis)),
	 	?_assertMatch(PidC12, H),
	 	?_assertMatch(C12bis, simdr_supervisor_contract:get_actor(Sup7, H)),
	 	?_assertMatch(simdr_actor_conveyor, simdr_actor_contract:get_module(C12bis)),
	 	?_assertMatch(
	 		{WS1, 2.0 },lookup_module(Sup7, PidC11, simdr_actor_workstation)), 
		?_assertMatch(
	 		{WS2, 3.0 },lookup_module(Sup7, PidC21, simdr_actor_workstation))%,
		% ?_assertMatch(
	 % 		{[], 999},lookup_module(Sup7, simdr_actor_contact:get_pid(C31bis), simdr_actor_workstation))
	 	].

	-endif. 