-module(simdr_timemachine).

%% User functions export

-export([start/0, get_time/0, forward_time/1]).

%% Spawn export

-export([loop/2, clockwork/0]).

start() ->
	case whereis(simdr_timemachine) of
		undefined ->
			register(
				simdr_timemachine, 
				spawn_link(simdr_timemachine, loop, [now(), 0])
			),
			spawn(?MODULE, clockwork, []),
			ok;
		_ ->
			ok
	end.

get_time() ->
	start(),
	MyPid = self(),
	simdr_timemachine ! {MyPid, time},
	receive
		{simdr_timemachine, MyPid, Value} ->
			Value
	end.

forward_time(Seconds) ->
	start(),
	simdr_timemachine ! {self(), forward, Seconds}.

loop({MegaSecs, Secs, MicroSecs}, IniDiff) ->
	Time = {
	  erlang:trunc(MegaSecs), 
	  erlang:trunc(Secs), 
	  erlang:trunc(MicroSecs)},
	receive
		{Pid, time} ->
			io:format("TIMEMACHINE > ~w~n", [Time]),
			Pid ! {simdr_timemachine, Pid, calendar:now_to_local_time(Time)},
			loop(Time, IniDiff);
		{_Pid, forward, Value} ->
			NewTime = {MegaSecs, Secs + Value, MicroSecs},
			loop(NewTime, IniDiff + Value);
		{_Pid, clock_forward, Value} ->
			NewTime = {MegaSecs, Secs + Value, MicroSecs},
			loop(NewTime, IniDiff)
	end.

clockwork() ->
	receive
		_ ->
			nothing_to_do,
			clockwork()
		after 1000 ->
			simdr_timemachine ! {self(), clock_forward, 1},
			clockwork()
	end.
