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
	{Quality, _Assembly} = Order,
	Outputs = simdr_actor_contract:get_out(ActorConfig),
	[Out1|R]=Outputs,
	[Out2|_R2]=R, 
	ProductState =simdr_actor_contract:get_state(Product),
	QualityProduct = simdr_actor_contract:get_option(Product, quality),
	 %%processed, assembled or finished
	Decision = case ProductState of
		raw -> { WS1, Time1} = lookup_module(Config, Out1, simdr_actor_workstation),
	 			{ WS2, Time2} = lookup_module(Config, Out2, simdr_actor_workstation), 
	 			[{Q1, Luck1}] = simdr_actor_contract:get_option(WS1, workstation_luck),
	 			[{Q2, Luck2}] = simdr_actor_contract:get_option(WS2, workstation_luck),
	 			case Q1 =:= Q2 of 
	 				true -> case Luck1>Luck2 of 
	 							true -> Out1;
	 							false -> Out2
	 						end;
	 				false -> case difference_quality(Q1, Quality)<difference_quality(Q2, Quality) of 
			 					true -> Out2;
			 					false -> Out1
			 				end
	 			end;

	 	processed -> { _Actor1, Time1} = lookup_module(Config, Out1, simdr_actor_workstation_assembly),
	 				{ _Actor2, Time2} = lookup_module(Config, Out2, simdr_actor_workstation_assembly), 
	 				case difference_quality(QualityProduct, Quality)>1 of 
			 				true ->  case Time1<Time2 of 
						 					true -> Out1;
						 					false -> Out2
					 					end;
					 		false -> case Time1<Time2 of 
						 					true -> Out2;
						 					false -> Out1
					 					end
			 		end;
	 	assembled -> { _Actor1, Time1} = lookup_module(Config, Out1, simdr_actor_workstation_finish),
	 				{ _Actor2, Time2} = lookup_module(Config, Out2, simdr_actor_workstation_finish), 
	 				case difference_quality(QualityProduct, Quality)<3 of 
			 				true ->  case Time1<Time2 of 
						 					true -> Out1;
						 					false -> Out2
					 					end;
					 		false -> case Time1<Time2 of 
						 					true -> Out2;
						 					false -> Out1
					 					end
			 		end;
	 	_ -> Out1
	 end,
 
 	Sender ! {self(), {prob_out, Product, Decision}},
 	Config;

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
lookup_module_helper(_Config, unknown_actor, _Module, _Time) ->
{unknown_actor, 9999};

lookup_module_helper(Config, Actor, Module, Time) ->
	case simdr_actor_contract:get_module(Actor) of
			Module -> {Actor, Time};
			_ -> Outputs = simdr_actor_contract:get_out(Actor),
				case   simdr_supervisor_contract:list_size(Outputs) of
					0 -> lookup_module_helper(Config, unknown_actor, Module, Time);
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

difference_quality(Q1,Q2) ->
	case Q1 of 
		'Q1' -> 
			case Q2 of 
				['Q1']-> 3;
				['Q2']-> 2;
				['Q3']-> 1;
				_ -> 0
			end;
		'Q2' -> 
			case Q2 of 
				['Q1']-> 2;
				['Q2']-> 3;
				['Q3']-> 2;
				_ -> 0
			end;
		'Q3' -> case Q2 of
				['Q1']-> 1;
				['Q2']-> 2;
				['Q3']-> 3;
				_ -> 0
			end;
		_ -> 0
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

	 	C41 =  simdr_actor_conveyor:create('C41'),
	 	C421 =  simdr_actor_conveyor:create('C421'),	
	 	C422=  simdr_actor_conveyor:create('C422'),
	 	WS422 = simdr_actor_workstation:create('WS422'),
	 	C4212=  simdr_actor_conveyor:create('C4212'),
	 	WS4212 = simdr_actor_workstation:create('WS4212'),

		PidC11 = simdr_actor_contract:get_pid(C11),
		PidC12 = simdr_actor_contract:get_pid(C12),
		PidWS1 = simdr_actor_contract:get_pid(WS1),
		PidC21 = simdr_actor_contract:get_pid(C21),
		PidC22 = simdr_actor_contract:get_pid(C22),
		PidC23 = simdr_actor_contract:get_pid(C23),
		PidWS2 = simdr_actor_contract:get_pid(WS2),
		PidC31 = simdr_actor_contract:get_pid(C31),
		PidC32 = simdr_actor_contract:get_pid(C32),
		PidC41 = simdr_actor_contract:get_pid(C41),
		PidC421 = simdr_actor_contract:get_pid(C421),
		PidC422= simdr_actor_contract:get_pid(C422),
		PidWS422 = simdr_actor_contract:get_pid(WS422),
		PidC4212= simdr_actor_contract:get_pid(C4212),
		PidWS4212 = simdr_actor_contract:get_pid(WS4212),

	 	C11bis = simdr_actor_contract:add_out(C11, PidC12),
	 	C12bis = simdr_actor_contract:add_out(C12, PidWS1),

	 	C21bis = simdr_actor_contract:add_out(C21, PidC22),
	 	C22bis = simdr_actor_contract:add_out(C22, PidC23),
	 	C23bis = simdr_actor_contract:add_out(C23, PidWS2),

	 	C31bis=  simdr_actor_contract:add_out(C31, PidC32),

	 	C41b = simdr_actor_contract:add_out(C41, PidC421),
	 	C41bis = simdr_actor_contract:add_out(C41b, PidC422),
	 	C422bis = simdr_actor_contract:add_out(C422, PidWS422),
	 	C421bis = simdr_actor_contract:add_out(C421, PidC4212),
	 	C4212bis = simdr_actor_contract:add_out(C4212, PidWS422),

	 	Sup1 = simdr_supervisor_contract:add_actor(Sup, {PidC11, C11bis}),
	 	Sup2 = simdr_supervisor_contract:add_actor(Sup1, {PidC12, C12bis}),
	 	Sup3 = simdr_supervisor_contract:add_actor(Sup2, {PidWS1, WS1}),
	 	Sup4 = simdr_supervisor_contract:add_actor(Sup3, {PidWS2, WS2}),
	 	Sup5 = simdr_supervisor_contract:add_actor(Sup4, {PidC21, C21bis}),
	 	Sup6 = simdr_supervisor_contract:add_actor(Sup5, {PidC22, C22bis}),
	 	Sup7 = simdr_supervisor_contract:add_actor(Sup6, {PidC23, C23bis}),
	 	Sup8 = simdr_supervisor_contract:add_actor(Sup7, {PidC31, C31bis}),
	 	Sup9 = simdr_supervisor_contract:add_actor(Sup8, {PidC32, C32}),
	 	Sup10 = simdr_supervisor_contract:add_actor(Sup9, {PidC41, C41bis}),
		Sup11 = simdr_supervisor_contract:add_actor(Sup10, {PidC421, C421}),
		Sup12 = simdr_supervisor_contract:add_actor(Sup11, {PidC422, C422bis}),
		Sup13 = simdr_supervisor_contract:add_actor(Sup12, {PidWS422, WS422}),
		Sup14 = simdr_supervisor_contract:add_actor(Sup13, {PidC4212, C4212bis}),
		Sup15 = simdr_supervisor_contract:add_actor(Sup14, {PidWS4212, WS4212}),


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
	 		{WS2, 3.0 },lookup_module(Sup7, PidC21, simdr_actor_workstation)),
		 ?_assertMatch(
	  		{unknown_actor, 9999},lookup_module(Sup9, PidC31, simdr_actor_workstation)),
		 ?_assertMatch(
	 		{WS422, 2.0 },lookup_module(Sup13, PidC41, simdr_actor_workstation)),
		 ?_assertMatch(
	 		{WS422, 2.0 },lookup_module(Sup15, PidC41, simdr_actor_workstation))
	 	].

	-endif. 