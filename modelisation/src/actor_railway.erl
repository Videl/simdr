-module(actor_railway).
-behaviour(actor_contract).
-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(),[{capacity,1}], undefined, 0, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	MesOut = case actor_contract:list_size(actor_contract:get_option(RailwayConfig, out)) of 
		1 ->
			{[no_prob_out], actor_contract:get_option(RailwayConfig,out)}; 
		_ ->
			{[prob_out], supervisor}
		end,
	MesIn = case actor_contract:list_size(actor_contract:get_option(RailwayConfig, in)) of 
		1 ->
			{Info, Rec} = MesOut,
			{[no_prob_in] ++ Info, Rec};
		_ -> {Info, _Rec} = MesOut,
			{[prob_in ]++ Info, supervisor}
				end,
	{InfoProb, Dest} = MesIn,

	case Dest =:= supervisor of
		true ->	
			{RailwayConfig, {actor_product, ProductConfig, InfoProb}, Dest};
		false -> 
			{RailwayConf, ProductConf} = 
				actor_contract:add_to_list_data(
					{RailwayConfig, 
						{ProductConfig,	
							{actor_contract:get_option(RailwayConfig, in),
							actor_contract:get_option(RailwayConfig, out)}
						}
					}, 
				{ProductConfig, RailwayConfig}),
			{RailwayConf, {actor_product, ProductConf, InfoProb}, Dest}
	end;

answer(RailwayConfig, {supervisor, ProductConfig, Decision}) ->
	{In, Out} = Decision,
	{Conf, Prod} = actor_contract:add_to_list_data(
		{RailwayConfig, 
			{ProductConfig, 
			{[In],[Out]}}}, 
		{ProductConfig, RailwayConfig}),
	case Decision =/= actor_contract:get_state(Conf) of
		true ->	
			RailwayConf = actor_contract:set_state(Conf, Decision),
			actor_contract:work(actor_contract:get_work_time(RailwayConf)),
			{RailwayConf,{actor_product, Prod,switched}, Out};
		false -> 
			actor_contract:work(actor_contract:get_work_time(RailwayConfig)/2),
			{Conf,{actor_product, Prod,switched}, Out}
	end;
	
answer(RailwayConfig, Request) ->
	actor_contract:answer(RailwayConfig, Request).


% idling(Config) ->
%  	actor_contract : idling(Config).

% processing(Config, NbWorker) ->
% 	receive
% 		{Sender, {actor_product,ProdConf}} ->
% 			Request= {actor_product,ProdConf},
% 			[N] = actor_contract:get_option(Config, capacity),
% 			case NbWorker+1> N of
% 				false -> Sender ! { self(), {control, ok, Request}},
% 						spawn(?MODULE, send_rfid, [Config, ProdConf]),
% 						spawn(?MODULE, physical_work, [self(), Config, Request]),
% 						?MODULE:processing(actor_contract:set_state(Config, processing), NbWorker+1);

% 				_-> Sender ! { self(), {control, full,{actor_contract : get_work_time(Config), Request}}},
% 						?MODULE:processing(Config, NbWorker)
				
% 			end;
% 		{Sender, {control, full, {Wait_time, Request}}} ->
% 			spawn(?MODULE, wait, [self(), Wait_time,{Request, Sender}]),
% 			?MODULE:processing(Config, NbWorker);

% 		{_Sender, Request} ->
% 			spawn(?MODULE, logical_work, [self(), Config, Request]),
% 			?MODULE:processing(Config, NbWorker);

% 		{_Worker, end_physical_work, {NewConfig, LittleAnswer, Destination}} ->
% 			% Find destination in 'out' pool
% 			% Send LittleAnswer
% 		 	{actor_product, ConfProd, _} = LittleAnswer,
% 			send_message({{actor_product, ConfProd}, Destination}),
% 			?MODULE:processing(actor_contract:set_state(NewConfig, free), NbWorker-1);
	
% 		{_Worker, end_logical_work, {NewConfig, LittleAnswer, Destination}} ->
% 			send_message({LittleAnswer, Destination}),
% 			%io:format(" Nouvelle config ~w ~n",[NewConfig]),
% 			?MODULE:processing(NewConfig, NbWorker);
% 		_ ->
% 			?MODULE:processing(Config, NbWorker)
% 	end.

% send_rfid( Conf, ProdConf) ->
% 	case actor_contract:get_option(Conf, rfid) of 
% 		[RFID] -> RFID ! {self(), {actor_product, ProdConf}};
% 		_ -> {nothing}
% 	end.

% send_message( {Ans, [Dest]}) when is_pid(Dest) -> 
% io:format("Conveyor Sending: ~w to ~w.~n", [ Ans, Dest]),
% 	Dest ! {self(), Ans};
% send_message( {Ans, Dest}) when is_pid(Dest) -> 
% 	Dest ! {self(), {Ans}};
% send_message({Ans, Dest}) ->
% 	%% @TODO: decider de la destination
% 	io:format("Conveyor Sending: ~w to ~w.~n", [Ans, Dest]).

% wait(Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
% 	actor_contract:work(Wait_time),
% 	Dest ! {Pid, {Ans}};

% wait(_Pid ,Wait_time, {Ans, Dest}) when is_pid(Dest)->
% 	actor_contract:work(Wait_time),
% 	io:format("Conveyor Sending: ~w to ~w.~n", [Ans, Dest]).

% physical_work(Master, MasterConfig, Request) ->
% 	io:format("Conveyor work ~w.~n", [{MasterConfig,Request}]),
% 	FullAnswer = ?MODULE:answer(MasterConfig, Request),
% 	Master ! {self(), end_physical_work, FullAnswer}.

% logical_work(Master, MasterConfig, Request) ->
% 	FullAnswer = ?MODULE:answer(MasterConfig, Request),
% 	Master ! {self(), end_logical_work, FullAnswer}.


%Internal API

%% ===================================================================
%% Tests
%% ===================================================================

answer_test_() ->
	Rail = create(),
	Prod = actor_product:create(),
	NewRail = actor_contract:add_option(Rail, in, 1),
	NewRail1 = actor_contract:add_option(NewRail, out, 2),
	NewRail2 = actor_contract:add_option(NewRail1, in, 2),
	NewRail3 = actor_contract:add_option(NewRail2, out, 2),
	NewRail4 = actor_contract:add_option(NewRail1, out, 2),
	{NewRail1bis, Prodbis}= actor_contract:add_to_list_data({NewRail1, 
					{Prod,{[1],[2]}}},{Prod,NewRail1}),
	
	{RailwayConf, Product}= actor_contract:add_to_list_data({NewRail2, 
					{Prod,{[2],[3]}}},{Prod,NewRail2}),
	RailwayConfig=actor_contract: set_state(RailwayConf, {2,3}),
	[
	?_assertEqual(
		{ NewRail2, {actor_product, Prod, [prob_in,no_prob_out]}, supervisor},
		answer(NewRail2, {actor_product, Prod})),
	?_assertEqual(
		{ NewRail1bis, {actor_product, Prodbis, [no_prob_in,no_prob_out]}, actor_contract:get_option(RailwayConfig,out)},
		answer(NewRail1, {actor_product, Prod})),
	?_assertEqual(
		{ NewRail3, {actor_product, Prod, [prob_in,prob_out]}, supervisor},
		answer(NewRail3, {actor_product, Prod})),
	?_assertEqual(
		{ NewRail4, {actor_product, Prod, [no_prob_in,prob_out]}, supervisor},
		answer(NewRail4, {actor_product, Prod})),
	?_assertEqual(
		{RailwayConfig, {actor_product, Product, switched}, 3},
		answer(NewRail2, {supervisor, Prod, {2,3}})),
	?_assertEqual(
		{Rail, {supervisor, pong}}, 
		answer(Rail, {supervisor, ping}))
	].