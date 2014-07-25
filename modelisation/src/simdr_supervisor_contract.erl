-module(simdr_supervisor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	 create/1,
	 create/2,
	% contain_decision/2,
	 get_destination/2,
	 add_decision/2,
	 get_actors/1,
	 get_actor/2,
	 add_data/3,
	 add_actor/2,
	 get_option/2,
	 set_option/3,
	 set_options/3,
	 update_actor/3,
	 delete_option/2,
	 add_option/3,
	 list_size/1
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
create(Module,simdr_actor_contract:random_id()).

 create(Module, Name) ->
  Id= Name,
    #supervisor{module = Module,
		id = Id,
		options = ets:new(
			list_to_atom(lists:concat(["supervisor_", Module, Id])), 
			[duplicate_bag,
			{write_concurrency, true},
			{read_concurrency, true},
			public]),
		master_supervisor = void,
		supervisors = [],
	        actors = [],
		data_pool = [],
		decisions_history = ets:new(history, [ordered_set, 
											  {write_concurrency, true}, 
											  {read_concurrency, true},
											  public])
	    }.

add_decision(Supervisor, Decision) -> 
	Table = Supervisor#supervisor.decisions_history,
	Data = {erlang:now(), erlang:localtime(), Decision},
	simdr_tools:add_data_in_ets(Table, Data).

% contain_decision(Supervisor, Prod)->
% 	Table = Supervisor#supervisor.decisions_history,
% 	List = ets:match_object(Table, {'_','_', {Prod, '$1','_'}}),
% 	case List of 
% 		[] -> false;
% 		_-> true
% 	end.
get_destination(Supervisor, Prod)->
	Table = Supervisor#supervisor.decisions_history,
	List = ets:match_object(Table, {'_','_', {Prod, '_','$1'}}),
	case List of 
		[] -> {nothing, nothing, {nothing, nothing, nothing}};
		_-> [H]=List,
			H
	end.

add_data(_Supervisor, _Message, _Object) ->
	%%% @TODO
	ok.


update_actor(Supervisor, Pid,  NewActor) ->
	Supervisor1 =  delete_actor(Supervisor, Pid),
	Supervisor2 = add_actor(Supervisor1, {Pid, NewActor}),
	Supervisor2.
	
delete_actor(Config, Pid) ->
	Actors = Config#supervisor.actors,
	NewConfig = Config#supervisor{actors = delete_actor_helper(Actors,[], Pid)},
	NewConfig.
	%%(ets:delete(Opts, Key)=:= true) orelse ?DLOG("Deletion failed"),


add_actor(Config, Actor) ->
	% io:format("Supervisors > Adding actor ~w to list of actors.~n", [Actor]),
	CurrentActors = get_actors(Config),
	NewConfig = Config#supervisor{actors = CurrentActors ++ [Actor]},
	NewConfig.

get_actors(Config) -> 
	Config#supervisor.actors.

get_actor(Config, Pid) ->
	Actors = get_actors(Config),
 	% io:format("~n<|><|> ~w <|><|>~n", [Actors]),
	Actor = get_actor_helper(Actors, Pid),
	Actor.

get_option(Supervisor, Key) ->
	Table = Supervisor#supervisor.options,
	simdr_tools:get_option_from_ets(Table, Key).

set_option(Supervisor, Key, Value) ->
	Table = Supervisor#supervisor.options,
	% ?DLOG(
	% 	simdr_actor_contract:get_name(Supervisor),
	% 	{lists:concat(["Inserting option to ", Table]), {Key, Value}}),
	true = simdr_tools:set_option_in_ets(Table, Key, Value),
	Supervisor.
	

set_options(Sup, Key, List) ->
	Table = Sup#supervisor.options,
	true = simdr_tools:set_options_in_ets(Table, Key, List),
	Sup.

delete_option(Supervisor, Key) ->
	Table = Supervisor#supervisor.options,
	simdr_tools:delete_option_in_ets(Table, Key),
	%%(ets:delete(Table, Key)=:= true) orelse ?DLOG("Deletion failed"),
	Supervisor.

add_option(Supervisor, Key, Value) ->
	Table = Supervisor#supervisor.options,
	simdr_tools:add_option_in_ets(Table, Key, Value),
	%%(ets:insert(Table, {Key, Value})=:= true) orelse ?DLOG("Insertion failed"),
	Supervisor.

list_size(List) ->
	list_size_helper(List, 0).

list_size_helper([], Acc) ->
	Acc;
list_size_helper(unknown_option, 0) ->
	0;
list_size_helper([_H|T], Acc) ->
	list_size_helper(T, Acc+1).
	
%% ===================================================================
%% Internal API
%% ===================================================================

delete_actor_helper([], Result, _Pid) ->
	Result;

delete_actor_helper(Actors, Result, Pid) ->
	[Head| Rest] = Actors,
	{Key, _Actor} = Head,
	case Key =:= Pid of
		true -> delete_actor_helper(Rest, Result, Pid);
		false -> Result2 = Result ++ [Head],
				delete_actor_helper(Rest, Result2 , Pid)
	end.

get_actor_helper([], _Pid) ->
	unknown_actor;

get_actor_helper(Actors, Pid) -> 
	[Head| Rest] = Actors,
	{Key, Actor} = Head,
	case Key =:= Pid of
		true -> Actor;
		false ->get_actor_helper( Rest, Pid)
	end.


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

delete_actor_test_() ->
	Sup = create(mod),
	Sup2 = add_actor(Sup, {1,'C1'}),
	Sup3 = add_actor(Sup2, {2,'C2'}),
	Sup4 = add_actor(Sup3, {3,'C3'}),
	Actor = simdr_actor_conveyor:create('C1'),
	Pid = simdr_actor_contract:get_pid(Actor),
	Sup5 = add_actor(Sup, {Pid,Actor}),
	[
	?_assertEqual(
			[{Pid,Actor}],
			get_actors(Sup5)
		),
	?_assertEqual(
			[{1,'C1'},{3,'C3'}],
			get_actors(delete_actor(Sup4, 2))
		)
	].

	get_actor_test_() ->
	Sup = create(mod),
	[
	?_assertEqual(unknown_actor,get_actor(Sup, 3))
	].

-endif.