-module(supervisor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	 create/1,
	 add_data/3
	]).

%% ===================================================================
%% Contract for Supervisors
%% ===================================================================

-callback create() -> 
    Supervisor :: term().

-callback timer_time(SupervisorConfig :: term()) ->
    integer().

-callback timer_action(SupervisorConfig :: term()) ->
    term().

-callback action_on_request(SupervisorConfig :: term(), 
			    Sender :: pid(), 
			    tuple(atom(), term(), atom())) ->
    term().


create(Module) ->
    #supervisor{module = Module,
		id = actor_contract:random_id(),
		master_supervisor = void,
		supervisors = [],
	        actors = [],
		data_pool = [],
		decisions_history = ets:new(history, [ordered_set, 
											  {write_concurrency, true}, 
											  {read_concurrency, true},
											  public])
	    }.

add_data(_Supervisor, _Message, _Object) ->
	%%% @TODO
	ok.