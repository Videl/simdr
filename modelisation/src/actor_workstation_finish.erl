-module(actor_workstation_finish).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	create/1,
	create/2,
	answer/2
	]).


%% Behavior implementation

create() ->
	%%% module, state, work_time
	create(actor_contract:random_id()).

create(Name) ->
	create(Name, {3,8,1}).

create(Name, {Stop, Manip, Evac}) ->
	Ac1 = actor_contract:create(?MODULE, Name, [], off, 1, []),
	actor_contract:set_option(Ac1, stop, Stop),
	actor_contract:set_option(Ac1, manipulation, Manip),
	actor_contract:set_option(Ac1, evacuation, Evac),
	Ac2 =actor_contract:set_work_time(Ac1, Stop+Manip+Evac),
	Ac3 = actor_contract:add_option(Ac2, order, {'Q1',{1,0,1,0}}),
	Ac3.

answer(WSConfig, {actor_product, ProductConfig}) ->

	[Order] = actor_contract:get_option(WSConfig, order),
	{Quality, _Assembly} = Order,
	QualityAct = actor_contract:get_option(ProductConfig, quality),
	 io : format( " ordre ~w, quality ~w ~n", [Quality,QualityAct] ),
	case improve(Quality, QualityAct) of 
	 true -> Finish = case QualityAct of 
							['Q1']-> 'Q1';
							['Q2']-> 	actor_contract:work(actor_contract:get_work_time(WSConfig)),
									actor_contract:set_option(ProductConfig, quality, {'Q1', pastille}),
									{'Q1', pastille};
							['Q3']-> 	actor_contract:work(actor_contract:get_work_time(WSConfig)),
									actor_contract:set_option(ProductConfig, quality, {'Q2', pastille}),
									{'Q2', pastille};
							_ -> {pastille}
						end;
	false -> Finish =Quality
		%%% @TODO: case 'unknown_option'
	end,
	NewProductConfig = actor_contract:set_state(ProductConfig, finished),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = actor_contract:add_to_list_data(
		WSConfig, {finish,'of',product, {ProductConfig, for, Finish}}, 
		NewProductConfig, {quality,became,Finish,because,'of',{WSConfig}}),
	 io : format( " nouvelle qualité ~w ~n", [Finish] ),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Finish}, 
	actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	actor_contract:answer(WSConfig, Request).

%improve Q2 ? 
improve (Q1, Q2) ->

case Q1 of 
	'Q1' -> case Q2 of 
				['Q1']-> false;
				['Q2']-> true;
				['Q3']-> true;
				_ -> false
			end;
	'Q2' -> case Q2 of 
				['Q1']-> false;
				['Q2']-> false;
				['Q3']-> true;
				_ -> false
				
			end;
	'Q3' -> false;
	_ -> false
end.

% ===================================================================
% Tests
% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = actor_contract:set_work_time(actor_workstation_finish:create(),1),
	ActorProductOne = actor_product:create(),
	ProductConf =actor_contract:set_option(ActorProductOne, quality, 'Q2'),
	{_, {actor_product, ActorProductTwo, Quality}, _Destination} = 
		answer(ActorWS, {actor_product, ProductConf}),
	[
		%%% Test: quality of a product is different
		?_assert(
			assembled =/= actor_contract:get_state(ActorProductTwo)
		),
		?_assertEqual(
			{'Q1', pastille},Quality
		)
	].

data_filler_test_() ->
	BaseWS = create(),
	BasePO = actor_product:create(),
	ProductConf =actor_contract:set_option(BasePO, quality, 'Q2'),
	{NewWS, {actor_product, NewPO, Finish}, _} = 
	 	answer(BaseWS, {actor_product, ProductConf}),
	LastDataWS = actor_contract:get_data(NewWS),
	LastDataPO = actor_contract:get_data(NewPO),
	[
		%%% Test: last data exists in product
		?_assertMatch(
			{_ErlangNow, _Time, _Actor, {quality,became,Finish,because,'of',{BaseWS}}},
			LastDataPO),
		%%% Test: last data exists in workstation
		?_assertMatch(
			{_ErlangNow, _Time, _Actor, {finish,'of',product, {BasePO, for, Finish}}}, 
			LastDataWS)
	].
create_test_() ->
	BaseWS = create('CA1', {3,2,1}),
	[
		?_assertEqual(
			6 , actor_contract: get_work_time(BaseWS)
			)
	].
-endif.