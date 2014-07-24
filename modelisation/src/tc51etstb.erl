%% Simulation/emulation time base and calendar using ets
%% Authors: Paul Valckenaers (K.U.Leuven Association, Belgium), 
%%          Matias Novias    (Intec, Argentina) 
-module(tc51etstb).
-include("tc51eventrecord.hrl").   %% -record (tc51event, {time_ref, notifyPid, load})
-include("app_configuration.hrl").

-export([init/0, init/1, insert/2, first/1, nextEventTime/1, removefirst/1]). 

init() ->
	init(tc51eventCalendar).

init(C) ->
	?CREATE_DEBUG_TABLE,
	?DLOG(lists:concat(["Initialising calendar ", C])),
	(ets:info(C) =:= undefined) orelse ets:delete(C), 
	ets:new(C, [ordered_set, public, named_table, {keypos, #tc51event.time_ref}]), 
	insert(C, #tc51event{time_ref = {infinity, make_ref()} } ),
	?DLOG(lists:concat(["Initialised calendar ", C])),
	{ok, C}.
	%% ToDo: add the Pid and a load to "wrap up" when this event is fetched from the "tb". 
	
insert(Calendar, Event ) ->
	?DLOG({lists:concat(["Inserting event to calendar ", Calendar]), Event}),
	(ets:insert_new(Calendar, Event) =:= true) orelse ?DLOG("Insertion failed").

first(Calendar) -> 
	Key = ets:first(Calendar),
	[Event] = ets:lookup(Calendar, Key),
	?DLOG({lists:concat(["First element from ", Calendar]), Event}),
	Event.

nextEventTime(Calendar) -> 
	Key = ets:first(Calendar),
	[#tc51event{time_ref = {Time, _ }}] = ets:lookup(Calendar, Key),
	?DLOG({lists:concat(["Next event time in ", Calendar]), Time}),
	Time. 
	
removefirst(Calendar) ->
	?DLOG(lists:concat(["Deleting first event to calendar ", Calendar])),
	Key = ets:first(Calendar),
	(ets:delete(Calendar, Key) =:= true) orelse ?DLOG("Deletion failed").