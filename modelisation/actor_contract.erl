-module(actor_contract).
-include("config.hrl").

-export([create/6, create/3, get_data/1, get_previous_data/2]).

%% ===================================================================
%% Contract for Actors
%% ===================================================================

-callback get_name(Config :: term()) -> 
	Name :: string() |
	atom().

-callback get_opt(Config :: term()) -> 
	list(tuple(Name :: string(), Parameters :: list(atom()))) |
	undefined.

-callback start(Config :: term()) -> 
	ok |
	undefined.

-callback stop(Config :: term()) -> 
	ok |
	undefined.

-callback work_time(Config :: term()) ->
	non_neg_integer() |
	undefined.

-callback get_data(Config :: term()) ->
	term().

-callback get_previous_data(Config :: term(), Step :: non_neg_integer()) ->
	term().

-callback get_state(Config :: term()) ->
	term().

%% ===================================================================
%% Helper functions
%% ===================================================================

create(Module, Name, Work_time) ->
	Actor = #config{module=Module, name=Name, opt=undefined, state=off, work_time=Work_time}.

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