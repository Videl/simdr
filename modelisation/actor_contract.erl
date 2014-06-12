-module(actor_contract).
-include("config.hrl").

-export([create/6, create/3, get_data/1, get_previous_data/2]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().

%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Name, Work_time) ->
	Actor = #config{module=Module, name=Name, opt=undefined, state=off, work_time=Work_time, list_data=[]},
	{ok, Actor}.

create(Module, Name, Opt, State, Work_time, List_data ) ->
	Actor = #config { module=Module, name=Name, opt=Opt, state=State, work_time=Work_time, list_data=List_data },
	{ok,Actor}.
	
get_data(Actor) ->
	get_head_data(Actor#config.list_data).

get_previous_data(Actor, N) ->
	not_implemented.

get_name(Actor) ->
	Actor#config.name.

get_opt(Actor) ->
	Actor#config.opt.

get_work_time(Actor) ->
	Actor#config.work_time.

get_state(Actor) ->
	Actor#config.state.

get_head_data() ->
	Actor#config.list_data.

set_work_time(Actor, Wtime:: non_neg_integer())->
	Actor#config { wtime =Wtime},
 	{ok, time}.

start(Actor) -> 
	Actor#config {state = on},
	on.

stop(Actor) -> 
	Actor#config { state = off},
	off. 
	
%% ===================================================================
%% Internal API
%% ===================================================================

get_head_data([]) ->
	undefined;

get_head_data([Head|_Tail]) ->
	Head.