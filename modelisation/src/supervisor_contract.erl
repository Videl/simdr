-module(supervisor_contract).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	 create/1,
	 add_data/3,
	 action_on_request/3
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

action_on_request(Config, _Sender, {add, actor, Actor}) ->
	CurrentActors = Config#supervisor.actors,
	NewConfig = Config#supervisor{actors = CurrentActors ++ [Actor]},
	NewConfig;

action_on_request(Config, Sender, {Former, _Type , _NewValue, changed})->
 	update_actor(Config, Sender, Former);


action_on_request(Config, Sender, Request) ->
	io:format("SUPERVISOR <><> UNKNOWN REQUEST ~w (from ~w).~n", [Request, Sender]),
	Config.

update_actor(Supervisor, NewActor, FormerActor) ->
	Supervisor1 = delete_actor(Supervisor, FormerActor),
	Supervisor2 = add_actor(Supervisor1, NewActor),
	Supervisor2.
	
delete_actor(Config, Actor) ->
	Actors = Config#supervisor.actors,
	delete_helper_actor(Actors,[], Actor).
	%%(ets:delete(Opts, Key)=:= true) orelse ?DLOG("Deletion failed"),

delete_helper_actor([], Result, _Actor) ->
	Result;

delete_helper_actor(Actors, Result, Actor) ->
	[Head| Rest] = Actors,
	case Head =:= Actor of
		true -> delete_helper_actor(Rest, Result, Actor);
		false -> Result2 = Result ++ [Head],
				delete_helper_actor(Rest, Result2 , Actor)
	end.

add_actor(Config, Actor) ->
	CurrentActors = Config#supervisor.actors,
	NewConfig = Config#supervisor{actors = CurrentActors ++ [Actor]},
	NewConfig.

get_actor(Config) -> 
	Config#supervisor.actors.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

delete_actor_test_() ->
	Sup = create(mod),
	Sup2 = add_actor(Sup, 'C1'),
	Sup3 = add_actor(Sup2, 'C2'),
	Sup4 = add_actor(Sup3, 'C3'),
	Actor = actor_conveyor:create('C1'),
	Sup5 = add_actor(Sup, Actor),
	%NewSup = action_on_request(Sup2, Actor2, {Actor, state , on, changed}),
	[
	?_assertEqual(
			[Actor],
			get_actor(Sup5)
		),
	?_assertEqual(
			['C1','C3'],
			delete_actor(Sup4, 'C2')
		)
	].

action_on_request_test_() -> 
	Sup = create(mod),
	Actor = actor_conveyor:create('C1'),
	Actor2 = actor_contract:set_state(Actor,on),
	Sup2 = add_actor(Sup, Actor),
	%NewSup = action_on_request(Sup2, Actor2, {Actor, state , on, changed}),
	[
	?_assertEqual(
			[Actor],
			get_actor(Sup2)
		)
	].

-endif.


