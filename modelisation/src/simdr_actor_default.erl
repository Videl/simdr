-module(simdr_actor_default).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-export([
	answer/2,
	basic_work_time_method/1
	]).

answer(ActorConfig, {supervisor, ping}) ->
	{ActorConfig, {supervisor, pong}};

answer(ActorConfig, {change, mode, NewMode}) ->
	PreviousMode = simdr_actor_contract:get_mode(ActorConfig),
	{Actor, Mode} = helper_answer_mode_change(ActorConfig, PreviousMode, NewMode),
	{Actor, {mode, Mode, changed}, supervisor};

answer(ActorConfig, {change, work_time, N}) ->
	NewConfig = simdr_actor_contract:set_work_time(ActorConfig, N),
	{NewConfig, {work_time, N, changed}, supervisor};

answer(ActorConfig, {change, state, State}) ->
	NewConfig = simdr_actor_contract:set_state(ActorConfig, State),
	{NewConfig, {state, State, changed}, supervisor};

answer(ActorConfig, {change, capacity, Capacity}) ->
	NewConfig = simdr_actor_contract:set_capacity(ActorConfig, Capacity),
	{NewConfig, {capacity, Capacity, changed}, supervisor};

answer(ActorConfig, {change, in_out, {In, Out}}) ->
	NewConfig = simdr_actor_contract:set_in_out(ActorConfig, {In, Out}),
	{NewConfig, {in_out, {In, Out}, changed}, supervisor};

answer(ActorConfig, {add, in, In}) ->
	NewConfig = simdr_actor_contract:add_in(ActorConfig, In),
	{_In, Out} = simdr_actor_contract:get_in_out(NewConfig),
	NewConfig2 = simdr_actor_contract:set_in_out(NewConfig, {In, Out}),
	{NewConfig2, {in, In, added}, supervisor};

answer(ActorConfig, {add, out, Out}) ->
	NewConfig = simdr_actor_contract:add_out(ActorConfig, Out),
	{In, _Out} = simdr_actor_contract:get_in_out(NewConfig),
	NewConfig2 = simdr_actor_contract:set_in_out(NewConfig, {In, Out}),
	{NewConfig2, {out, Out, added}, supervisor};

answer(ActorConfig, {add, option, Opt}) ->
	{Key, Desc}=Opt,
	NewConfig = simdr_actor_contract:add_option(ActorConfig, Key, Desc),
	{NewConfig, {option, Opt, added}, supervisor};

answer(ActorConfig, {status, work_time}) ->
	{ActorConfig, 
	{work_time, simdr_actor_contract:get_work_time(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, state}) ->
	{ActorConfig, 
	{state, simdr_actor_contract:get_state(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, in}) ->
	{ActorConfig, 
	{in, simdr_actor_contract:get_in(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, out}) ->
	{ActorConfig, 
	{out, simdr_actor_contract:get_out(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, in_out}) ->
	{ActorConfig, 
	{in_out, simdr_actor_contract:get_in_out(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, capacity}) ->
	{ActorConfig, 
	{capacity, simdr_actor_contract:get_capacity(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, option, Key}) ->
	{ActorConfig, 
	{option, simdr_actor_contract:get_option(ActorConfig, Key), status}, 
	supervisor};

answer(ActorConfig, {status, module}) ->
	{ActorConfig, 
	{module, simdr_actor_contract:get_module(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {status, pid}) ->
	{ActorConfig, 
	{pid, simdr_actor_contract:get_pid(ActorConfig), status}, 
	supervisor};

answer(ActorConfig, {io_export, list_data}) ->
	TablePid =  simdr_actor_contract:get_list_data(ActorConfig),
	Fun = export_to(io),
	ets:foldl(Fun, ok, TablePid),
	{ActorConfig, 
	{io_export, simdr_actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {file_export, list_data}) ->
	TablePid = simdr_actor_contract:get_list_data(ActorConfig),
	%% File creation
	{ok, F} = file:open(lists:concat(["data_", 
					 				 simdr_actor_contract:get_module(ActorConfig),
					 				 "_",
					 				 simdr_actor_contract:get_name(ActorConfig),
					 				 ".log"]), [append, delayed_write, unicode]),
	Fun = export_to(file),
	ets:foldl(Fun, F, TablePid),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, simdr_actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {csv_export, list_data}) ->
	TablePid = simdr_actor_contract:get_list_data(ActorConfig),
	%% File creation
	{ok, F} = file:open(lists:concat(["data_", 
					 				 simdr_actor_contract:get_module(ActorConfig),
					 				 "_",
					 				 simdr_actor_contract:get_name(ActorConfig),
					 				 ".csv"]), [append, delayed_write, unicode]),
	Fun = export_to(csv),
	R = io_lib:format("~s;~s;~s;~s;~s\n",["Year,Month,Day", 
									   "Hour,Minutes,Seconds", 
									   "Source Actor", 
									   "Message",
									   "Destination Actor"]),
	%RX = erlang:iolist_to_binary(R),
	%RXF = lists:flatten(RX),
	ok = file:write(F,  R),
	ets:foldl(Fun, F, TablePid),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, simdr_actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {io_export, debug}) ->
	?CREATE_DEBUG_TABLE,
	Fun = export_to(io),
	ets:foldl(Fun, ok, ?DEBUG_TABLE),
	{ActorConfig, 
	{io_export, simdr_actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {file_export, debug}) ->
	?CREATE_DEBUG_TABLE,
	%% File creation
	{ok, F} = file:open("debug.log", [append, delayed_write, unicode]),
	Fun = export_to(file),
	ets:foldl(Fun, F, ?DEBUG_TABLE),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, simdr_actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(ActorConfig, {csv_export, debug}) ->
	?CREATE_DEBUG_TABLE,
	%% File creation
	{ok, F} = file:open("debug.csv", [append, delayed_write, unicode]),
	Fun = export_to(csv),
	ets:foldl(Fun, F, ?DEBUG_TABLE),
	ok = file:close(F),
	{ActorConfig, 
	{file_export, simdr_actor_contract:get_module(ActorConfig), format}, 
	supervisor};

answer(_, Request) ->
	io:format(">>>UNKNOWN ANSWER<<< (~w) (~w:~w)~n", [Request, ?MODULE, ?LINE]),
	exit(unknown_request).

%% ===================================================================
%% Internal API
%% ===================================================================

helper_answer_mode_change(ActorConfig, discrete, discrete) ->
	io:format("Warning: mode change to ~w is *redundant* " ++
		"because same as previous mode..~n", [discrete]),
	Conf = simdr_actor_contract:set_mode(ActorConfig, discrete),
	{Conf, discrete};
helper_answer_mode_change(ActorConfig, rt, rt) ->
	io:format("Warning: mode change to ~w is *redundant* " ++
		"because same as previous mode.~n", [discrete]),
	Conf = simdr_actor_contract:set_mode(ActorConfig, rt),
	{Conf, rt};
helper_answer_mode_change(ActorConfig, _, rt) ->
	Conf = simdr_actor_contract:set_mode(ActorConfig, rt),
	{Conf, rt};
helper_answer_mode_change(ActorConfig, _, discrete) ->
	Conf = simdr_actor_contract:set_mode(ActorConfig, discrete),
	{Conf, discrete}.

export_to(file) ->
	fun(X, FileDescriptor) -> 
		R = io_lib:format("~w\n",[X]),
		%RX = erlang:iolist_to_binary(R),
		%RXF = lists:flatten(RX),
		ok = file:write(FileDescriptor,  R),
		FileDescriptor 
	end;
export_to(csv) ->
	fun(X, FileDescriptor) ->
		%%% Value decomposition
		% R = io_lib:format("~w\n", [X]),
		 {_Now, {YearMonthDate, HourMinSecs}, Source, Message, Dest} = X,
		 ShortSource = actor_sumup(Source),
		 ShortDest = actor_sumup(Dest),
		 R = io_lib:format("~w;~w;~w;~w;~w\n",[YearMonthDate, 
										   HourMinSecs, 
										   ShortSource, 
										   Message,
										   ShortDest]),
		%RX = erlang:iolist_to_binary(R),
		%RXF = lists:flatten(RX),
		ok = file:write(FileDescriptor,  R),
		FileDescriptor 
	end;
export_to(_) ->
	fun(X, Y) ->
		R = io_lib:format("~w\n",[X]),
		%RX = erlang:iolist_to_binary(R),
		%RXF = lists:flatten(RX),
		io:format("~w~n", [R]), Y
	end.

actor_sumup(Actor) when is_record(Actor, actor) ->
	{simdr_actor_contract:get_module(Actor), 
	 simdr_actor_contract:get_name(Actor),
	 simdr_actor_contract:get_state(Actor)};
actor_sumup({Actor}) when is_record(Actor, actor) ->
	{simdr_actor_contract:get_module(Actor), 
	 simdr_actor_contract:get_name(Actor),
	 simdr_actor_contract:get_state(Actor)};
actor_sumup(Actor) ->
	Actor.

basic_work_time_method(Actor) ->
	simdr_actor_contract:get_work_time(Actor).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).

answer_test_() ->
Actor = simdr_actor_contract:create(mod, test, [{out,1},{in,2},{out,3}], on, 42, []),
NewState = simdr_actor_contract:set_state(Actor,off),
NewWTime = simdr_actor_contract:set_work_time(Actor,10),
NewOpt = simdr_actor_contract:add_option(Actor,in,4),
	[
	 ?_assertEqual(
		{Actor, {state,on, status}, supervisor},
	 	answer(Actor, {status, state})),
	?_assertEqual(
		{NewState, {state, off, changed}, supervisor},
	 	answer(Actor, {change, state, off})),
	?_assertEqual(
		{Actor, {module, mod, status}, supervisor},
		answer(Actor,{status, module})),
	?_assertEqual(
		{Actor, {work_time, 42, status}, supervisor},
		answer(Actor, {status, work_time})),
	 ?_assertEqual(
	 	{NewWTime, {work_time, 10, changed}, supervisor},
	 	answer(Actor,{change, work_time, 10})),
	 ?_assertEqual(
	 	{NewOpt, {option,{in,4}, added}, supervisor},
	 	answer(Actor, {add, option,{in,4}}))
	].

-endif.