-module(actor_contract).
-include("config.hrl").

-export([create/6, create/3, get_data/1, get_previous_data/2, get_id/1, get_opt/1, get_work_time/1, get_state/1, set_work_time/1, start/1, stop/1]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback answer(Config :: term(), Entering :: term()) ->
	Exiting :: term().


%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Id, Work_time) ->
	Actor = #config{module=Module, id=Id opt=undefined, state=off, work_time=Work_time, list_data=[]},
	{ok, Actor}.

create(Module, Id, Opt, State, Work_time, List_data ) ->
	Actor = #config { module=Module,, id=Id, opt=Opt, state=State, work_time=Work_time, list_data=List_data },
	{ok,Actor}.
	
get_data(Actor) ->
	get_head_data(Actor#config.list_data).

get_previous_data(Actor, N) ->
	not_implemented.

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