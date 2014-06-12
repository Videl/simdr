-module(actor_contract).
-include_lib("eunit/include/eunit.hrl").

% config record
-include("config.hrl").

-export([create/6, create/3, get_data/1, get_previous_data/2, add_data/2, get_id/1, get_opt/1, get_work_time/1, get_state/1, set_work_time/2, start/1, stop/1]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().


%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Id, Work_time) ->
	Actor = #config{module=Module, id=Id, opt=undefined, state=off, work_time=Work_time, list_data=[]},
	{ok, Actor}.

create(Module, Id, Opt, State, Work_time, List_data ) ->
	Actor = #config { module=Module, id=Id, opt=Opt, state=State, work_time=Work_time, list_data=List_data },
	{ok,Actor}.
	
get_data(Actor) ->
	get_head_data(Actor#config.list_data).

get_previous_data(Config, N) ->
	get_previous_data_helper(Config#config.list_data, N).

add_data(Actor, new_data) -> 
	[new_data] ++ Actor#config.list_data.

get_id(Actor) ->
	Actor#config.id.

get_opt(Actor) ->
	Actor#config.opt.

get_work_time(Actor) ->
	Actor#config.work_time.

get_state(Actor) ->
	Actor#config.state.

set_work_time(Actor, Work_time)->
	Actor = #config { work_time =Work_time},
 	{ok, time}.

start(Actor) -> 
	Actor = #config {state = on},
	on.

stop(Actor) -> 
	Actor = #config {state = off},
	off. 
	
%% ===================================================================
%% Internal API
%% ===================================================================

get_head_data([]) ->
	undefined;
get_head_data([Head|_Tail]) ->
	Head.

get_previous_data_helper(_List, N) when N < 0 ->
	out_of_range;
get_previous_data_helper([Head|_Tail], 1) ->
	Head;
get_previous_data_helper([], 1) ->
	out_of_range;
get_previous_data_helper([_Head|Tail], N) ->
	get_previous_data_helper(Tail, N-1).

%% ===================================================================
%% Tests
%% ===================================================================

get_head_1_test() ->
	3 = get_head_data([3,2,1]).

get_head_2_test() ->
	undefined = get_head_data([]).

get_head_3_test() ->
	1 = get_head_data([1]).

get_previous_data_1_test() ->
	2 = get_previous_data_helper([1,2,3], 2).

get_previous_data_2_test() ->
	1 = get_previous_data_helper([1,2,3], 1).

get_previous_data_3_test() ->
	3 = get_previous_data_helper([1,2,3], 3).

get_previous_data_4_test() ->
	out_of_range = get_previous_data_helper([1,2,3], 4).

get_previous_data_5_test() ->
	out_of_range = get_previous_data_helper([1,2,3], -3).

get_previous_data_6_test() ->
	4 = get_previous_data_helper([1,2,3, 4], 4).
