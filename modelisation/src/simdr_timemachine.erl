%%% @doc Module that store time and stays up to date, even in discrete mode.
%%% 	 A normal node stores time and answer calls of time and forward requests
%%% 	 while another node (`clockwork') tells the first node to go forward 
%%% 	 every second.
%%% @end
-module(simdr_timemachine).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% User functions export

-export([start/0, get_time/0, forward_time/1]).

%% Spawn export

-export([loop/2, clockwork/0]).

%%% @doc Launch the timemachine.
%%% 	 This function can be used to start the timemachine.
%%%      It has been conceived in a way that everyone can call
%%% 	 this function a lot of time.
%%% 	 Just use get_time/0 and forward_time/1 though, they 
%%%		 start the timemachine if they need to.
%%% 	 This function registers the node with the name
%%%      `simdr_timemachine', with the right time.
%%% @see get_time/0
%%% @see forward_time/1
%%% @end
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

%%% @doc Get the right time, even in discrete mode.
%%% 	 Asks the timemachine the time.
%%% @spec () -> Time :: term()
%%% @see loop/2
%%% @end
get_time() ->
	start(),
	MyPid = self(),
	simdr_timemachine ! {MyPid, time},
	receive
		{simdr_timemachine, MyPid, Value} ->
			Value
	end.

%%% @doc Forward the time of the timemachine.
%%% 	 Asks the timemachine to forward `Seconds' seconds.
%%% @spec (Seconds) -> ok
%%% @see loop/2
%%% @end
forward_time(Seconds) ->
	start(),
	simdr_timemachine ! {self(), forward, Seconds}.

%%% @doc Timemachine. Listen for requests of time and forward time.
%%% 	 Heavily depends on clockwork/0 to advance time in real-time mode
%%% 	 or when even discrete mode takes some time.
%%% @see clockwork/0
%%% @end
loop({MegaSecs, Secs, MicroSecs}, IniDiff) ->
	Time = {
	  erlang:trunc(MegaSecs), 
	  erlang:trunc(Secs), 
	  erlang:trunc(MicroSecs)},
	receive
		{Pid, time} ->
			?DFORMAT("TIMEMACHINE > ~w~n", [Time]),
			Pid ! {simdr_timemachine, Pid, calendar:now_to_local_time(Time)},
			loop(Time, IniDiff);
		{_Pid, forward, Value} ->
			NewTime = {MegaSecs, Secs + Value, MicroSecs},
			loop(NewTime, IniDiff + Value);
		{_Pid, clock_forward, Value} ->
			NewTime = {MegaSecs, Secs + Value, MicroSecs},
			loop(NewTime, IniDiff)
	end.

%%% @doc The one-second timer.
%%% 	 Keeps the timemachine up to date with the flow of time.
%%% @see loop/2
%%% @end
clockwork() ->
	receive
		_ ->
			nothing_to_do,
			clockwork()
		after 1000 ->
			simdr_timemachine ! {self(), clock_forward, 1},
			clockwork()
	end.
