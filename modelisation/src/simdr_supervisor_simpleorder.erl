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
	create/1,
	create/2
	]).

create() ->
    create(simdr_actor_contract:random_id()).
 
create(Name) ->
	create(Name, {'Q1',{1,0,1,0}}).

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
	 %%processed, assembled or finished
	Decision = case ProductState of
		raw -> out_raw(Config, Out1, Out2, Quality);
	 	processed -> out_processed (Config, Out1, Out2, Quality, Product, Order);
	 	assembled -> out_assembled(Config, Out1, Out2, Quality, Product, Order);
	 	_ -> Out2
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
	case Orders of 
		{_Q,_A} ->first(Orders);
		_ -> {'Q1',{1,0,1,0}}
	end.

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

lookup_module_helper(_Config, _Act, _Module, Time) when (Time>40)->
{unknown_actor, 999};
lookup_module_helper(_Config, unknown_actor, _Module, _Time) ->
{unknown_actor, 998};

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
[H2|_Rest2] = Rest,
A1 = simdr_supervisor_contract:get_actor(Config, H),
A2 = simdr_supervisor_contract:get_actor(Config, H2),
{Ac, Time1} = lookup_module_helper(Config, A1, Module, Time),
{Ac2, Time2} = lookup_module_helper(Config, A2, Module, Time),
	case Time1<Time2 of 
		 true -> {Ac, Time1};
		 false -> {Ac2, Time2}
	end;

loop(ListOut, _Size, Config, Module, Time) ->
[H| Rest] =ListOut,
[H2|Rest2] = Rest,
A1 = simdr_supervisor_contract:get_actor(Config, H),
A2 = simdr_supervisor_contract:get_actor(Config, H2),
{_Ac, Time1} = lookup_module_helper(Config, A1, Module, Time),
{_Ac2, Time2} = lookup_module_helper(Config, A2, Module, Time),
	case Time1<Time2 of 
		 true -> NewList = [H]++Rest2,
		 	loop(NewList, simdr_supervisor_contract:list_size(NewList), Config, Module, Time1);
		 false-> loop(Rest, simdr_supervisor_contract:list_size(Rest), Config, Module, Time2)
	end.

difference_quality(Q1,Q2) when is_list(Q1) ->
	case Q1 of 
		['Q1'] -> 
			case Q2 of 
				'Q1'-> 0;
				'Q2'-> -1;
				'Q3'-> -2;
				_ -> -3
			end;
		['Q2'] -> 
			case Q2 of 
				'Q1'-> 1;
				'Q2'-> 0;
				'Q3'-> -1;
				_ -> -3
			end;
		['Q3'] -> case Q2 of
				'Q1'-> 2;
				'Q2'-> 1;
				'Q3'-> 0;
				_ -> -3
			end;
		_ -> 0
	end; 
difference_quality(Q1,Q2) ->
	case Q1 of 
		'Q1' -> 
			case Q2 of 
				'Q1'-> 0;
				'Q2'-> -1;
				'Q3'-> -2;
				_ -> -3
			end;
		'Q2' -> 
			case Q2 of 
				'Q1'-> 1;
				'Q2'-> 0;
				'Q3'-> -1;
				_ -> -3
			end;
		'Q3' -> case Q2 of
				'Q1'-> 2;
				'Q2'-> 1;
				'Q3'-> 0;
				_ -> -3
			end;
		_ -> 0
	end.

out_raw(Config, Out1, Out2, Quality)->
		{WS1, _Time1} = lookup_module(Config, Out1, simdr_actor_workstation),
	 	{WS2, _Time2} = lookup_module(Config, Out2, simdr_actor_workstation), 
	 	[{Q1, Luck1}] = simdr_actor_contract:get_option(WS1, workstation_luck),
	 	[{Q2, Luck2}] = simdr_actor_contract:get_option(WS2, workstation_luck),
	 	case Q1 =:= Q2 of 
	 		true -> case Luck1>Luck2 of 
	 							true -> Out1;
	 							false -> Out2
	 						end;
	 		false -> case difference_quality(Q1, Quality)<difference_quality(Q2, Quality) of 
			 					true -> Out1;
			 					false -> Out2
			 				end
	 	end.

out_processed(Config, Out1, Out2, Quality, Product, Order)->
		IdProduct = simdr_actor_contract:get_name(Product),
		QualityProduct = simdr_actor_contract:get_option(Product, quality),
		{ Actor1, Time1} = lookup_module(Config, Out1, simdr_actor_workstation_assembly),
	 	{ Actor2, Time2} = lookup_module(Config, Out2, simdr_actor_workstation_assembly), 
	 	case abs(difference_quality(QualityProduct, Quality))<2 of 
			 true ->  case Time1<Time2 of 
						 	true -> case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor1}) of 
						 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor1)),
						 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor1});
						 			true -> ok
						 			end,
						 			Out1;
						 	false -> case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor2}) of 
						 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor2)),
						 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor2});
						 			true-> ok
						 			end,
						 			Out2
					 end;
			 false -> case Time1<Time2 of 
						 	true -> case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor2}) of 
						 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor2)),
						 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor2});
						 			true -> ok
						 			end,
						 			Out2;
						 	false ->  case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor1}) of 
						 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor1)),
						 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor1});
						 			true -> ok
						 			end,
						 			Out1 
					end
		end.

out_assembled(Config, Out1, Out2, Quality, Product, Order)->
			IdProduct = simdr_actor_contract:get_name(Product),
			QualityProduct = simdr_actor_contract:get_option(Product, quality),
			{ Actor1, Time1} = lookup_module(Config, Out1, simdr_actor_workstation_finish),
	 		{ Actor2, Time2} = lookup_module(Config, Out2, simdr_actor_workstation_finish), 
	 				case difference_quality(QualityProduct, Quality)>0 of 
			 				true ->  case Time1<Time2 of 
						 					true -> case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor1}) of 
												 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor1)),
												 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor1});
												 			true -> ok
												 			end,
												 			Out1 ;
						 					false -> case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor2}) of 
												 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor2)),
												 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor2});
												 			true -> ok
												 			end,
												 			Out2
					 					end;
					 		false -> case Time1<Time2 of 
						 					true -> case simdr_supervisor_contract:contain_decision(Config,{IdProduct, Order,Actor2}) of 
												 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor2)),
												 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor2});
												 			true -> ok
												 			end,
												 			Out2;
						 					false -> case simdr_supervisor_contract:contain_decision(Config, {IdProduct, Order,Actor1}) of 
												 			false -> send_message({add, option, {order, Order}}, simdr_actor_contract:get_pid(Actor1)),
												 					simdr_supervisor_contract:add_decision(Config,{IdProduct, Order,Actor1});
												 			true -> ok
												 			end,
												 			Out1 
					 					end
			 		end.


send_message(LittleAnswer, Destination) when is_pid(Destination)->
	Destination ! {self(), LittleAnswer};
send_message(_LittleAnswer, Destination)->
	io:format("~w COULDN'T send  message to ~w because of BAD FORMAT. " ++ 
		"Message not sent.~n~n", [self(), Destination]).

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
		Sup11 = simdr_supervisor_contract:add_actor(Sup10, {PidC421, C421bis}),
		Sup12 = simdr_supervisor_contract:add_actor(Sup11, {PidC422, C422bis}),
		Sup13 = simdr_supervisor_contract:add_actor(Sup12, {PidWS422, WS422}),
		Sup14 = simdr_supervisor_contract:add_actor(Sup13, {PidC4212, C4212bis}),
		Sup15 = simdr_supervisor_contract:add_actor(Sup14, {PidWS4212, WS4212}),


	 	Outputs = simdr_actor_contract:get_out(C11bis),
				 [H|_Rest] = Outputs,
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
	  		{unknown_actor, 998},lookup_module(Sup9, PidC31, simdr_actor_workstation)),
		 ?_assertMatch(
	 		{WS422, 2.0 },lookup_module(Sup13, PidC41, simdr_actor_workstation)),
		 ?_assertMatch(
	 		{WS422, 2.0 },lookup_module(Sup15, PidC41, simdr_actor_workstation))
	 	].

 out_raw_test_() ->
	 	Sup = create('S', {'Q1',{1,0,0,1}}),
	 	Product= simdr_actor_product:create(),
	 	R = simdr_actor_railway:create(),
	 	C11 =  simdr_actor_conveyor:create('C11'),
	 	C12 =  simdr_actor_conveyor:create('C12'),
	 	WS1 = simdr_actor_workstation:create({'Q2', 33}),
	 	C21 =  simdr_actor_conveyor:create('C21'),
	 	C22 =  simdr_actor_conveyor:create('C22'),
	 	C23 =  simdr_actor_conveyor:create('C23'),
	 	WS2 = simdr_actor_workstation:create({'Q1', 33}),



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
	 	Rb = simdr_actor_contract:add_out(R, PidC11),
	 	Rbis = simdr_actor_contract:add_out(Rb, PidC21),

	 	Sup1 = simdr_supervisor_contract:add_actor(Sup, {PidC11, C11bis}),
	 	Sup2 = simdr_supervisor_contract:add_actor(Sup1, {PidC12, C12bis}),
	 	Sup3 = simdr_supervisor_contract:add_actor(Sup2, {PidWS1, WS1}),
	 	Sup4 = simdr_supervisor_contract:add_actor(Sup3, {PidWS2, WS2}),
	 	Sup5 = simdr_supervisor_contract:add_actor(Sup4, {PidC21, C21bis}),
	 	Sup6 = simdr_supervisor_contract:add_actor(Sup5, {PidC22, C22bis}),
 		Sup7 = simdr_supervisor_contract:add_actor(Sup6, {PidC23, C23bis}),

 		[{_Q1, _Luck1}] = simdr_actor_contract:get_option(WS1, workstation_luck),
	 	[{Q2, _Luck2}] = simdr_actor_contract:get_option(WS2, workstation_luck),

		Order = get_first_order(Sup7),
		{Quality, _Assembly} = Order,
		Outputs = simdr_actor_contract:get_out(Rbis),
		[Out1,Out2]=Outputs,
 
 		[
 		?_assertMatch({ WS1, _Time1},lookup_module(Sup7, PidC11, simdr_actor_workstation)),
 		?_assertMatch({ WS2, _Time2},lookup_module(Sup7, PidC21, simdr_actor_workstation)),
 		?_assertMatch(raw, simdr_actor_contract:get_state(Product)),
 		?_assertMatch(0,difference_quality(Q2, Quality)),
 		?_assertMatch([PidC21, PidC11], simdr_actor_contract:get_out(Rbis)),
 		?_assertMatch(C11bis, simdr_supervisor_contract:get_actor(Sup7, PidC11)),
 		?_assertMatch(C11bis,simdr_supervisor_contract:get_actor(Sup7, Out2)),
	 	?_assertMatch(PidC21, out_raw(Sup7, Out1, Out2, Quality))

	 	].

 out_processed_test_() ->
	 	Sup = create('S', {'Q1',{1,0,0,1}}),
	 	R = simdr_actor_railway:create(),
	 	C11 =  simdr_actor_conveyor:create('C11'),
	 	C12 =  simdr_actor_conveyor:create('C12'),
	 	WS1 = simdr_actor_workstation_assembly:create(),
	 	C21 =  simdr_actor_conveyor:create('C21'),
	 	C22 =  simdr_actor_conveyor:create('C22'),
	 	C23 =  simdr_actor_conveyor:create('C23'),



	 	PidC11 = simdr_actor_contract:get_pid(C11),
		PidC12 = simdr_actor_contract:get_pid(C12),
		PidWS1 = simdr_actor_contract:get_pid(WS1),
		PidC21 = simdr_actor_contract:get_pid(C21),
		PidC22 = simdr_actor_contract:get_pid(C22),
		PidC23 = simdr_actor_contract:get_pid(C23),


		C11bis = simdr_actor_contract:add_out(C11, PidC12),
	 	C12bis = simdr_actor_contract:add_out(C12, PidWS1),
	 	C21bis = simdr_actor_contract:add_out(C21, PidC22),
	 	C22bis = simdr_actor_contract:add_out(C22, PidC23),

	 	Rb = simdr_actor_contract:add_out(R, PidC11),
	 	Rbis = simdr_actor_contract:add_out(Rb, PidC21),

	 	Sup1 = simdr_supervisor_contract:add_actor(Sup, {PidC11, C11bis}),
	 	Sup2 = simdr_supervisor_contract:add_actor(Sup1, {PidC12, C12bis}),
	 	Sup3 = simdr_supervisor_contract:add_actor(Sup2, {PidWS1, WS1}),
	 	Sup4 = simdr_supervisor_contract:add_actor(Sup3, {PidC21, C21bis}),
	 	Sup5 = simdr_supervisor_contract:add_actor(Sup4, {PidC22, C22bis}),
 		Sup7 = simdr_supervisor_contract:add_actor(Sup5, {PidC23, C23}),


		Order = get_first_order(Sup7),
		{Quality, _Assembly} = Order,
		Outputs = simdr_actor_contract:get_out(Rbis),
		[Out1,Out2]=Outputs,
		Prod = simdr_actor_product:create(),
		Product=simdr_actor_contract:add_option(Prod, quality, 'Q2'),
		Prod2= simdr_actor_product:create(),
 		Product2=simdr_actor_contract:add_option(Prod2, quality, 'Q3'),

 		[
 		?_assertMatch({ WS1, _Time1},lookup_module(Sup7, PidC11, simdr_actor_workstation_assembly)),
 		?_assertMatch({ unknown_actor, _Time2},lookup_module(Sup7, PidC21, simdr_actor_workstation_assembly)),
 		?_assertMatch([PidC21, PidC11], simdr_actor_contract:get_out(Rbis)),
 		?_assertMatch(C11bis, simdr_supervisor_contract:get_actor(Sup7, PidC11)),
 		?_assertMatch(C11bis,simdr_supervisor_contract:get_actor(Sup7, Out2)),
	 	?_assertMatch(PidC11, out_processed(Sup7, Out1, Out2, Quality, Product, Order)),
	 	?_assertMatch(PidC21, out_processed(Sup7, Out1, Out2, Quality, Product2, Order))

	 	].

	  out_assembled_test_() ->
	 	Sup = create('S', {'Q1',{1,0,0,1}}),
	 	R = simdr_actor_railway:create(),
	 	C11 =  simdr_actor_conveyor:create('C11'),
	 	C12 =  simdr_actor_conveyor:create('C12'),
	 	WS1 = simdr_actor_workstation_finish:create(),
	 	C21 =  simdr_actor_conveyor:create('C21'),
	 	C22 =  simdr_actor_conveyor:create('C22'),
	 	C23 =  simdr_actor_conveyor:create('C23'),



	 	PidC11 = simdr_actor_contract:get_pid(C11),
		PidC12 = simdr_actor_contract:get_pid(C12),
		PidWS1 = simdr_actor_contract:get_pid(WS1),
		PidC21 = simdr_actor_contract:get_pid(C21),
		PidC22 = simdr_actor_contract:get_pid(C22),
		PidC23 = simdr_actor_contract:get_pid(C23),


		C11bis = simdr_actor_contract:add_out(C11, PidC12),
	 	C12bis = simdr_actor_contract:add_out(C12, PidWS1),
	 	C21bis = simdr_actor_contract:add_out(C21, PidC22),
	 	C22bis = simdr_actor_contract:add_out(C22, PidC23),

	 	Rb = simdr_actor_contract:add_out(R, PidC11),
	 	Rbis = simdr_actor_contract:add_out(Rb, PidC21),

	 	Sup1 = simdr_supervisor_contract:add_actor(Sup, {PidC11, C11bis}),
	 	Sup2 = simdr_supervisor_contract:add_actor(Sup1, {PidC12, C12bis}),
	 	Sup3 = simdr_supervisor_contract:add_actor(Sup2, {PidWS1, WS1}),
	 	Sup4 = simdr_supervisor_contract:add_actor(Sup3, {PidC21, C21bis}),
	 	Sup5 = simdr_supervisor_contract:add_actor(Sup4, {PidC22, C22bis}),
 		Sup7 = simdr_supervisor_contract:add_actor(Sup5, {PidC23, C23}),


		Order = get_first_order(Sup7),
		{Quality, _Assembly} = Order,
		Outputs = simdr_actor_contract:get_out(Rbis),
		[Out1,Out2]=Outputs,
		Prod = simdr_actor_product:create(),
		Product=simdr_actor_contract:add_option(Prod, quality, 'Q2'),
		Prod2= simdr_actor_product:create(),
 		Product2=simdr_actor_contract:add_option(Prod2, quality, 'Q1'),
 
 		[
 	
 		?_assertMatch([PidC21, PidC11], simdr_actor_contract:get_out(Rbis)),
 		?_assertMatch(C11bis, simdr_supervisor_contract:get_actor(Sup7, PidC11)),
 		?_assertMatch(C11bis,simdr_supervisor_contract:get_actor(Sup7, Out2)),
	 	?_assertMatch(PidC11, out_assembled(Sup7, Out1, Out2, Quality, Product, Order)),
	 	?_assertMatch(PidC21, out_assembled(Sup7, Out1, Out2, Quality, Product2, Order))

	 	].



	-endif. 