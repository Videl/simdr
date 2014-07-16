-module(supervisor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	 create/1,
	 add_data/3,
	 action_on_request/3,
	 get_option/2,
	 set_option/3,
	 delete_option/2,
	 add_option/3
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
	Id = actor_contract:random_id(),
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

add_data(_Supervisor, _Message, _Object) ->
	%%% @TODO
	ok.

action_on_request(Config, Sender, {add, actor, Actor}) ->
	% io:format("<><> Adding actor ~w.~n", [Actor]),
	NewConfig = add_actor(Config, {Sender, Actor}),
	NewConfig;

action_on_request(Config, Sender, {work_time, N, changed})->
	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:set_work_time(Actor, N),
 	update_actor(Config, Sender, NewActor);

action_on_request(Config, Sender, {distance, N, changed})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:set_distance(Actor, N),
 	update_actor(Config, Sender, NewActor);

action_on_request(Config, Sender,  {speed, N, changed})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:set_speed(Actor, N),
 	update_actor(Config, Sender, NewActor);

 action_on_request(Config, Sender, {state, State, changed})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:set_state(Actor, State),
 	update_actor(Config, Sender, NewActor);

 action_on_request(Config, Sender, {capacity, Capacity, changed})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:set_capacity(Actor, Capacity),
 	update_actor(Config, Sender, NewActor);

 action_on_request(Config, Sender, {in_out, {In, Out}, changed})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:set_in_out(Actor, {In, Out}),
 	update_actor(Config, Sender, NewActor);

  action_on_request(Config, Sender, {in, In, added})->
 	Actor = get_actor(Config, Sender),
 	% io:format("~n<><> ~w <><>~n", [Actor]),
	NewActor = actor_contract:add_in(Actor, In),
	{_In, Out} = actor_contract:get_in_out(NewActor),
	NewActor2 = actor_contract:set_in_out(NewActor, {In, Out}),
 	update_actor(Config, Sender, NewActor2);
 
  action_on_request(Config, Sender, {out, Out, added})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:add_out(Actor, Out),
	{In, _Out} = actor_contract:get_in_out(NewActor),
	NewActor2 = actor_contract:set_in_out(NewActor, {In, Out}),
 	update_actor(Config, Sender, NewActor2);

   action_on_request(Config, Sender, {add, option, Opt})->
 	Actor = get_actor(Config, Sender),
	NewActor = actor_contract:add_option(Actor, Opt),
 	update_actor(Config, Sender, NewActor);

 	

action_on_request(Config, Sender, Request) ->
	io:format("SUPERVISOR <><> UNKNOWN REQUEST ~w (from ~w).~n", [Request, Sender]),
	Config.

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
	?DLOG(
		actor_contract:get_name(Supervisor),
		{lists:concat(["Inserting option to ", Table]), {Key, Value}}),
	true = simdr_tools:set_option_in_ets(Table, Key, Value),
	Supervisor.
	
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
	[];

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
	Actor = actor_conveyor:create('C1'),
	Pid = actor_contract:get_pid(Actor),
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

 action_on_request_test_() -> 
	Sup = create(mod),
	Actor = actor_conveyor:create('C1'),
	Pid = actor_contract:get_pid(Actor),
	Actor2 = actor_contract:set_state(Actor,on),
	Sup2 = add_actor(Sup, {Pid,Actor}),
 	NewSup = action_on_request(Sup2, Pid, 
 		{ state , on, changed}),
 	NewSup2 = action_on_request(Sup2, Pid, 
 		{ out , 3, added}),
 	[{_P,Actor4}] = get_actors(NewSup2),
 	[
	?_assertEqual(
 			[{Pid,Actor2}],
 			get_actors(NewSup)
		), 
	?_assertEqual(
 			[3], actor_contract: get_out(Actor4)
		)

 	].

-endif.