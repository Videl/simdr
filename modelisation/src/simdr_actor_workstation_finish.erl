%%% @doc Actor Workstation
%%% 
%%% This module provides you with a 'Finish' Workstation Actor. 
%%%
%%% When the actor simdr_actor_product is in this actor, the workstation
%%% will raise (or not) the quality of the product according to the
%%% order sent by the supervisor.
%%% 
%%% Suitable options: capacity = limited, only one actor entering in this actor,
%%% only one actor exiting this actor. (One actor in `in'/`out'.)
%%%
%%% @author Andre THOMAS <andre.thomas@univ-lorraine.fr>
%%% @author Hind BRIL EL HAOUZI <hind.el-haouzi@univ-lorraine.fr>
%%% @author Arnould GUIDAT <arnould.guidat@univ-lorraine.fr>
%%% @author Marion LY <marion.ly@telecomnancy.net>
%%% @author Thibaut SMITH <videl@protonmail.ch>
%%% @see 'overview-summary'
%%% @end
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

create(Name, Orders) when is_list(Orders)->
    Ac1 = create(Name, {3,8,1}),
    Ac2 = simdr_actor_contract:set_options(Ac1, order, Orders),
    Ac2;

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
		unknown_option -> Order = {'Q1',{1,0,1,0}},
						R=[];
		 [Order|R] ->simdr_actor_contract:get_option(WSConfig, order)
	end,
	{Quality, _Assembly} = Order,
	QualityAct = simdr_actor_contract:get_option(ProductConfig, quality),
	?DFORMAT("Upgrade Workstation: ordre ~w, quality ~w ~n", [Quality,QualityAct]),
	case improve(Quality, QualityAct) of 
		true -> 
			Finish = case QualityAct of 
						['Q1'] -> 
							'Q1';
						['Q2'] ->
							simdr_actor_contract:work(WSConfig),
							simdr_actor_contract:set_option(
								ProductConfig, 
								quality, 
								'Q1'),
							%%% Set chip so we know it has been enhanced
							simdr_actor_contract:set_option(
								ProductConfig,
								chip,
								true),
							{'Q1', with_chip};
						['Q3'] ->
							simdr_actor_contract:work(WSConfig),
							simdr_actor_contract:set_option(
								ProductConfig, 
								quality, 
								'Q2'),
							%%% Set Pastille
							simdr_actor_contract:set_option(
								ProductConfig,
								chip,
								true),
							{'Q2', with_chip};
						_ ->
						weird_state
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
	case simdr_actor_contract:get_option(WSConfig, order) of 
	 	unknown_option -> nothing;
	 	_-> simdr_actor_contract:set_options(WSConfig, order, R)
	end,
	% end,
	% io:format("Order après delete: ~w ~n",[simdr_actor_contract:get_option(WSConfig, order)] ),
	%%% Answer
	{NewWSConfig, 
	{actor_product, NewProductConfigBis, Finish}, 
	simdr_actor_contract:get_out(NewWSConfig)};
answer(WSConfig, Request) ->
	simdr_actor_default:answer(WSConfig, Request).

improve('Q1', 'Q1') ->
	false;
improve('Q1', _) -> %%% Q1 is the best.
	true;
improve('Q2', 'Q1') ->
	false;
improve('Q2', 'Q2') ->
	false;
improve('Q2', _) ->
	true;
improve('Q3', _) -> %%% Q3 is lowest, so if we want it, anything will be fine.
	false.

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
			{'Q1', with_chip},Quality
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