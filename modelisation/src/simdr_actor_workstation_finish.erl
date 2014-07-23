-module(simdr_actor_workstation_finish).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

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
	create(simdr_actor_contract:random_id()).

create(Name) ->
	create(Name, {3,8,1}).

create(Name, {Stop, Manip, Evac}) ->
	Ac1 = simdr_actor_contract:create(?MODULE, Name, [], off, 1, []),
	simdr_actor_contract:set_option(Ac1, stop, Stop),
	simdr_actor_contract:set_option(Ac1, manipulation, Manip),
	simdr_actor_contract:set_option(Ac1, evacuation, Evac),
	Ac2 =simdr_actor_contract:set_work_time(Ac1, Stop+Manip+Evac),
%	Ac3 = simdr_actor_contract:add_option(Ac2, order, {'Q1',{1,0,1,0}}),
	Ac2.

answer(WSConfig, {actor_product, ProductConfig}) ->
	case simdr_actor_contract:get_option(WSConfig, order) of 
		unknown_option -> Order = {'Q2',{1,0,1,0}};
		_ -> [Order] = simdr_actor_contract:get_option(WSConfig, order)
	end,
	{Quality, _Assembly} = Order,
	QualityAct = simdr_actor_contract:get_option(ProductConfig, quality),
	 io : format( " ordre ~w, quality ~w ~n", [Quality,QualityAct] ),
	case improve(Quality, QualityAct) of 
		true -> 
			Finish = case QualityAct of 
						['Q1'] -> 
							'Q1';
						['Q2'] ->
							simdr_actor_contract:work(simdr_actor_contract:get_work_time(WSConfig)),
							simdr_actor_contract:set_option(ProductConfig, quality, {'Q1', pastille}),
							{'Q1', pastille};
						['Q3'] ->
							simdr_actor_contract:work(simdr_actor_contract:get_work_time(WSConfig)),
							simdr_actor_contract:set_option(ProductConfig, quality, {'Q2', pastille}),
							{'Q2', pastille};
						_ ->
							{pastille}
					 end;
		false -> 
			Finish = Quality
		%%% @TODO: case 'unknown_option'
	end,
	NewProductConfig = simdr_actor_contract:set_state(ProductConfig, finished),
	%%% List data fillers
	{NewWSConfig, NewProductConfigBis} = simdr_actor_contract:add_to_list_data(
		WSConfig, {{finish, for, Finish, 'of',product}, {ProductConfig}}, 
		NewProductConfig, {{quality,became,Finish,because,'of'},{WSConfig}}),
	 io : format( " nouvelle qualité ~w ~n", [Finish] ),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Finish}, 
	simdr_actor_contract:get_out(NewWSConfig)};
	
answer(WSConfig, Request) ->
	simdr_actor_default:answer(WSConfig, Request).

%improve Q2 ? 
improve (Q1, Q2) ->
	case Q1 of 
		'Q1' -> 
			case Q2 of 
				['Q1']-> false;
				['Q2']-> true;
				['Q3']-> true;
				_ -> false
			end;
		'Q2' -> 
			case Q2 of 
				['Q1']-> false;
				['Q2']-> false;
				['Q3']-> true;
				_ -> false
			end;
		'Q3' -> 
			false;
		_ -> false
	end.

% ===================================================================
% Tests
% ===================================================================
-ifdef(TEST).

answer_test_() ->
	ActorWS = simdr_actor_contract:set_work_time(simdr_actor_workstation_finish:create(),1),
	ActorProductOne = simdr_actor_product:create(),
	ProductConf =simdr_actor_contract:set_option(ActorProductOne, quality, 'Q2'),
	{_, {actor_product, ActorProductTwo, Quality}, _Destination} = 
		answer(ActorWS, {actor_product, ProductConf}),
	[
		%%% Test: quality of a product is different
		?_assert(
			assembled =/= simdr_actor_contract:get_state(ActorProductTwo)
		),
		?_assertEqual(
			{'Q1', pastille},Quality
		)
	].

create_test_() ->
	BaseWS = create('CA1', {3,2,1}),
	[
		?_assertEqual(
			6 , simdr_actor_contract: get_work_time(BaseWS)
			)
	].
-endif.