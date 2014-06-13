-module(actor_contract).
-include_lib("eunit/include/eunit.hrl").

% config record
-include("config.hrl").

-export([create/6, 
		 create/3, 
		 get_list_data/1,
		 get_data/1, 
		 get_previous_data/2, 
		 add_data/2, 
		 get_id/1, 
		 get_opt/1, 
		 get_work_time/1, 
		 get_state/1, 
		 set_work_time/2,
		 set_state/2, 
		 start/1, 
		 stop/1,
		 work/1]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback create() -> Actor :: tuple(atom(), term()).

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().


%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Id, Work_time) ->
	Actor = #config{module=Module, id=Id, opt=undefined, state=off, work_time=Work_time, list_data=[]},
	{ok, Actor}.

create(Module, Id, Opt, State, Work_time, List_data) ->
	Actor = #config { module=Module, id=Id, opt=Opt, state=State, work_time=Work_time, list_data=List_data },
	{ok,Actor}.

get_list_data(Actor) ->
	Actor#config.list_data.
	
get_data(Actor) ->
	get_head_data(Actor#config.list_data).

get_previous_data(Config, N) ->
	get_previous_data_helper(Config#config.list_data, N).

add_data(Actor, X) -> 
	Actor#config{list_data = [X] ++ Actor#config.list_data}.


get_id(Actor) ->
	Actor#config.id.

get_opt(Actor) ->
	Actor#config.opt.

get_work_time(Actor) ->
	Actor#config.work_time.

get_state(Actor) ->
	Actor#config.state.

set_work_time(Actor, Work_time)->
	 Actor#config{work_time =Work_time}.

start(Actor) -> 
	set_state(Actor, on).

stop(Actor) ->
	set_state(Actor, off).

set_state(Actor, State) ->
	Actor#config {state = State}.


work(N) ->
	timer:sleep(N*1000).
	
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

mock_actor() ->
	{ok, Actor} = create(mod, test, {opt1, opt2}, busy, 3, [1,2,3]),
	Actor.

get_list_data_test() ->
{ok,Actor} = create(mod, test, 0),
	[]= get_list_data(Actor).

get_data_1_test() ->
	Actor = mock_actor(),
	1 = get_data(Actor).

get_previous_data_1_test() ->
	Actor = mock_actor(),
	2 = get_previous_data(Actor, 2).

get_previous_data_2_test() ->
	Actor = mock_actor(),
	1 = get_previous_data(Actor, 1).

get_previous_data_3_test() ->
	Actor = mock_actor(),
	3 = get_previous_data(Actor, 3).

get_head_1_test() ->
	3 = get_head_data([3,2,1]).

get_head_2_test() ->
	undefined = get_head_data([]).

get_head_3_test() ->
	1 = get_head_data([1]).

get_previous_data_helper_1_test() ->
	2 = get_previous_data_helper([1,2,3], 2).

get_previous_data_helper_2_test() ->
	1 = get_previous_data_helper([1,2,3], 1).

get_previous_data_helper_3_test() ->
	3 = get_previous_data_helper([1,2,3], 3).

get_previous_data_helper_4_test() ->
	out_of_range = get_previous_data_helper([1,2,3], 4).

get_previous_data_helper_5_test() ->
	out_of_range = get_previous_data_helper([1,2,3], -3).

get_previous_data_helper_6_test() ->
	4 = get_previous_data_helper([1,2,3, 4], 4).

add_data_test() ->
	{ok,Actor} = create(mod, test, undefined, on, 0, [1,2]),
	NewActor = add_data(Actor, 3),
	[3,1,2] = NewActor#config.list_data.


get_id_test() ->
{ok,Actor} = create(mod, test, 0),
	test = get_id(Actor).

get_opt_1_test() ->
	{ok,Actor} = create(mod, test, opt1, on, 0, [1,2]),
	opt1= get_opt(Actor).

get_opt_2_test() ->
	{ok,Actor} = create(mod, test, 0),
	undefined= get_opt(Actor).

get_opt_3_test() ->
	{ok,Actor} = create(mod, test, [opt1, opt2], on, 0, [1,2]),
	[opt1, opt2] = get_opt(Actor).

get_work_time_test() ->
	{ok,Actor} = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	42 = get_work_time(Actor).

get_state_1_test() ->
	{ok,Actor} = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	on = get_state(Actor).

get_state_2_test() ->
	{ok,Actor} =create(mod, test, 0),
	off = get_state(Actor).

set_work_time_test()->
	{ok,Actor} = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	NewActor = set_work_time(Actor, 12),
	12 = NewActor#config.work_time.

get_start_test() ->
	{ok,Actor} = create(mod, test, [opt1, opt2], off, 42, [1,2]),
	NewActor = start(Actor),
	on = NewActor#config.state.

get_stop_test() ->
	{ok,Actor} = create(mod, test, [opt1, opt2], on, 42, [1,2]),
	NewActor = stop(Actor),
	off = NewActor#config.state.
