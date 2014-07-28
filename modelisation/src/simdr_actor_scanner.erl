-module(simdr_actor_scanner).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(simdr_actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/1,
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	create(simdr_actor_contract:random_id()).

create(Name) ->
	simdr_actor_contract:create(?MODULE, Name, [], off, 2, []).

answer(ScannerConfig, {actor_product, ProductConfig}) ->
	simdr_actor_contract:work(ScannerConfig),
	case simdr_actor_contract:get_option(ProductConfig, quality) =:= unknown_option  of 
		true -> 
			Transfo = simdr_actor_contract:get_option(ProductConfig, processed),
			case Transfo of
				unknown_option ->
					FinalQuality = simdr_actor_contract:get_option(ProductConfig, initial_quality),
					ProductConf = simdr_actor_contract:set_option(
						ProductConfig, 
						quality, 
						FinalQuality);
				_ ->
					[{Quality, Luck}] = Transfo,
					?DFORMAT("Qualite : ~w, Luck :~w ~n", [Quality, Luck]),
					FinalQuality = random_weighted(Luck, Quality),
					?DFORMAT("Qualite du produit : ~w ~n", [FinalQuality]),
					ProductConf = simdr_actor_contract:set_option(
						ProductConfig, 
						quality, 
						FinalQuality)
			end;
		false -> 
			FinalQuality = simdr_actor_contract:get_option(ProductConfig, quality),
			ProductConf =  ProductConfig
	end,

	{NewScannerConfig, NewProductConfig} = simdr_actor_contract:add_to_list_data(
	ScannerConfig, {{scanned,product, quality, is, FinalQuality, for}, {ProductConf}},
	ProductConf, {{'of',quality, FinalQuality, was,scanned,by},{ScannerConfig}}),
	% Answer
	{NewScannerConfig, 
	{actor_product, NewProductConfig, simdr_actor_contract:get_name(NewProductConfig)}, 
	supervisor};

answer(ScannerConfig, Request) ->
	simdr_actor_default:answer(ScannerConfig, Request).


%% ===================================================================
%% Internal API
%% ===================================================================

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

random_list_maker(List, _, 0) ->
    List;
random_list_maker(List, {Objective, Luck}, X) ->
    NewList = [random_weighted(Luck, Objective)] ++ List,
    random_list_maker(NewList, {Objective, Luck}, X-1).

random_list_maker({Objective, Luck}, X) ->
	random_list_maker([], {Objective, Luck}, X).

%%% List: list to traverse
%%% Chr: character to count
%%% Returns the number of times the character has been found.
%%% @end
occurences_in_a_list(List, Chr) ->
    Fonc = fun(X, N) when X =:= Chr ->
		   N+1;
	      (_, N) -> N
	   end,
    lists:foldl(Fonc, 0, List).

percent(Max, No) ->
    A = (No/Max)*100,
    B = float_to_list(A, [{decimals, 0}]),
    {C, _} = string:to_integer(B),
    C.

random_test_() ->
	random:seed(erlang:now()),
	ListQ1_100 = random_list_maker({'Q1', 100}, 10000),
	ListQ1_20  = random_list_maker({'Q1', 20}, 10000),
	ListQ1_50  = random_list_maker({'Q1', 50}, 10000),
	ListQ3_80  = random_list_maker({'Q3', 80}, 10000),
	ListQ3_30  = random_list_maker({'Q3', 30}, 10000),
	ListQ3_60  = random_list_maker({'Q3', 60}, 10000),
	ListQ2_85  = random_list_maker({'Q2', 85}, 10000),
	ListQ2_25  = random_list_maker({'Q2', 25}, 10000),
	ListQ2_55  = random_list_maker({'Q2', 55}, 10000),
	%%% Answer
	ResultQ1_100 = percent(10000, occurences_in_a_list(ListQ1_100, 'Q1')),
	ResultQ1_20  = percent(10000, occurences_in_a_list(ListQ1_20, 'Q1')),
	ResultQ1_50  = percent(10000, occurences_in_a_list(ListQ1_50, 'Q1')),
	ResultQ3_80  = percent(10000, occurences_in_a_list(ListQ3_80, 'Q3')),
	ResultQ3_30  = percent(10000, occurences_in_a_list(ListQ3_30, 'Q3')),
	ResultQ3_60  = percent(10000, occurences_in_a_list(ListQ3_60, 'Q3')),
	ResultQ2_85  = percent(10000, occurences_in_a_list(ListQ2_85, 'Q2')),
	ResultQ2_25  = percent(10000, occurences_in_a_list(ListQ2_25, 'Q2')),
	ResultQ2_55  = percent(10000, occurences_in_a_list(ListQ2_55, 'Q2')),
	MaxError = 3,
	[
		?_assert(ResultQ1_100 =:= 100),
		?_assert(ResultQ1_20 < (20 + MaxError)),
		?_assert(ResultQ1_20 > (20 - MaxError)),
		?_assert(ResultQ1_50 < (50 + MaxError)),
		?_assert(ResultQ1_50 > (50 - MaxError)),
		?_assert(ResultQ3_80 < (80 + MaxError)),
		?_assert(ResultQ3_80 > (80 - MaxError)),
		?_assert(ResultQ3_30 < (30 + MaxError)),
		?_assert(ResultQ3_30 > (30 - MaxError)),
		?_assert(ResultQ3_60 < (60 + MaxError)),
		?_assert(ResultQ3_60 > (60 - MaxError)),
		?_assert(ResultQ2_85 < (85 + MaxError)),
		?_assert(ResultQ2_85 > (85 - MaxError)),
		?_assert(ResultQ2_25 < (25 + MaxError)),
		?_assert(ResultQ2_25 > (25 - MaxError)),
		?_assert(ResultQ2_55 < (55 + MaxError)),
		?_assert(ResultQ2_55 > (55 - MaxError))

	].

-endif.