-module(simdr_supervisor_default).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-export([	 
	action_on_request/3]).


action_on_request(Config, Sender, {add, actor, Actor}) ->
	% io:format("<><> Adding actor ~w.~n", [Actor]),
	NewConfig = simdr_supervisor_contract:add_actor(Config, {Sender, Actor}),
	NewConfig;

action_on_request(Config, Sender, {work_time, N, changed})->
	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
	NewActor = simdr_actor_contract:set_work_time(Actor, N),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor);

 action_on_request(Config, Sender, {state, State, changed})->
 	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
	NewActor = simdr_actor_contract:set_state(Actor, State),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor);

 action_on_request(Config, Sender, {capacity, Capacity, changed})->
 	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
	NewActor = simdr_actor_contract:set_capacity(Actor, Capacity),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor);

 action_on_request(Config, Sender, {in_out, {In, Out}, changed})->
 	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
	NewActor = simdr_actor_contract:set_in_out(Actor, {In, Out}),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor);

  action_on_request(Config, Sender, {in, In, added})->
 	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
 	% io:format("~n<><> ~w <><>~n", [Actor]),
	NewActor = simdr_actor_contract:add_in(Actor, In),
	{_In, Out} = simdr_actor_contract:get_in_out(NewActor),
	NewActor2 = simdr_actor_contract:set_in_out(NewActor, {In, Out}),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor2);
 
  action_on_request(Config, Sender, {out, Out, added})->
 	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
	NewActor = simdr_actor_contract:add_out(Actor, Out),
	{In, _Out} = simdr_actor_contract:get_in_out(NewActor),
	NewActor2 = simdr_actor_contract:set_in_out(NewActor, {In, Out}),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor2);

   action_on_request(Config, Sender, {add, option, Opt})->
 	Actor = simdr_supervisor_contract:get_actor(Config, Sender),
	NewActor = simdr_actor_contract:add_option(Actor, Opt),
 	simdr_supervisor_contract:update_actor(Config, Sender, NewActor);

 	

action_on_request(Config, Sender, Request) ->
	io:format("SUPERVISOR <><> UNKNOWN REQUEST ~w (from ~w).~n", [Request, Sender]),
	Config.

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

action_on_request_test_() -> 
	Sup = simdr_supervisor_contract:create(mod),
	Actor = simdr_actor_conveyor:create('C1'),
	Pid = simdr_actor_contract:get_pid(Actor),
	Actor2 = simdr_actor_contract:set_state(Actor,on),
	Sup2 = simdr_supervisor_contract:add_actor(Sup, {Pid,Actor}),
 	NewSup = action_on_request(Sup2, Pid, 
 		{ state , on, changed}),
 	NewSup2 = action_on_request(Sup2, Pid, 
 		{ out , 3, added}),
 	[{_P,Actor4}] = simdr_supervisor_contract:get_actors(NewSup2),
 	[

	?_assertEqual(
 			[{Pid,Actor2}],
 			simdr_supervisor_contract:get_actors(NewSup)
		),
	 ?_assertEqual(
 			[3], simdr_actor_contract:get_out(Actor4)
		)

 	].
 	-endif.