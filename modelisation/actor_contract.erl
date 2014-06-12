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
	
get_data(Config) ->
	get_head_data(Config#config.list_data).

get_previous_data(Config, N) ->
	not_implemented.

%% ===================================================================
%% Internal API
%% ===================================================================

get_head_data([]) ->
	undefined;

get_head_data([Head|_Tail]) ->
	Head.