-module(simdr_actor_product).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

%% simdr_Actor Contract Behaviors Callbacks

-export([
	answer/2,
	create/0
	]).

%% External API

-export([
	create/1,
	create/2
	]).

%% Behavior implementation

create() ->
	create(simdr_actor_contract:random_id()).

create(Name) ->
	create(Name, random()).

create(Name, Quality) ->
	simdr_actor_contract:create(?MODULE, Name, [{initial_quality, Quality}], raw, 0, []).


answer(ProdConfig, {change, Data,_}) ->
	{ProdConfig, Data, no_change};
answer(ProdConfig, Request) ->
	simdr_actor_default:answer(ProdConfig, Request).


%% Internal API

random() ->
	random_weighted(33, 'Q1').

random_weighted(Luck, Objective) ->
    Rnd = random:uniform(100),
    %%% 0 can't arise in Rnd, so we go up to 100
    Result = case Luck >= Rnd of
				true ->
					Objective;
				 _ ->
				    random(Objective)
	     	 end,
    Result.	

random('Q1') ->
    Result = case random:uniform(2) of
		 1 -> % Medium quality
		     'Q2';
		 2 -> % Bad quality
		     'Q3'
	     end,
    Result;
random('Q2') ->
    Result = case random:uniform(2) of
		 1 -> % Good quality
		     'Q1';
		 2 -> % Bad quality
		     'Q3'
	     end,
    Result;
random('Q3') ->
    Result = case random:uniform(2) of
		 1 -> % Good quality
		     'Q1';
		 2 -> % Medium quality
		     'Q2'
	     end,
    Result.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).


-endif.