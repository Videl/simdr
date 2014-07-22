-module(tc51phyres). %% physical resource non-aggregated/non-composite. 
-include("tc51eventrecord.hrl").
-include("debug.hrl").
%% -record (tc51event, {time_ref, notifyPid, load}). 
%% time_ref = {time, reference} where the reference ensures uniqueness 
-import(tc51eventmgr, [post/3, postincr/3, returntoken/2, gettime/0, gettoken/1]). 
-import(tc51phylnk, [get_to/1]).

-export([install/2, connect/3, disconnect/2, decommission/1]).
-export([influence/2, getinfo/2, subscribe/2]). 

-export([initiate/6]). %% for internal use  - spawn - only. 

install(Name, Parent) ->
    ?DLOG({installing, Name, Parent}), 
	spawn_link(tc51phyres, initiate, [Name, Parent, idle, sets:new(), sets:new(), dict:new()]).


connect(PidPhyres, PidPort, input) -> 
	PidPhyres ! {connect, input, PidPort};
	
connect(PidPhyres, PidPort, output) -> 
	PidPhyres ! {connect, output, PidPort}.
	
disconnect(PidPhyres, PidPort) -> 
	PidPhyres ! {disconnect, PidPort}.
	
decommission(PidPhyres) -> 
	PidPhyres ! decommissioned.   

	
influence(PidPhyres, Command) -> PidPhyres ! {execute, Command}.

getinfo(PidPhyres, Query) -> 
	PidPhyres ! {self(), answer, Query},
	receive
	 Info -> Info
	end.
	
subscribe(PidPhyres, EventSpec) -> PidPhyres ! {self(), notify_when, EventSpec}.


initiate(Name, Parent, idle, InPortSet, OutPortSet, VisitorSet) -> 
	?DLOG({initiating, self(), Name, Parent, idle, InPortSet, OutPortSet, VisitorSet}),
	idling(Name, Parent, InPortSet, OutPortSet, VisitorSet). 

idling(Name, Parent, InPortSet, OutPortSet, VisitorSet) ->
    ?DLOG({idle, Name, Parent, InPortSet, OutPortSet, VisitorSet}), 
	receive 
	{connect, input, PortPid}    -> 
		idling(Name, Parent, sets:add_element(PortPid, InPortSet), OutPortSet, VisitorSet);	
	{connect, output, PortPid}   -> 
		idling(Name, Parent, InPortSet, sets:add_element(PortPid, OutPortSet), VisitorSet);
	{disconnect, PortPid} ->
		idling(Name, Parent, sets:del_element(PortPid, inPortSet), 
				                sets:del_element(PortPid, outPortSet), VisitorSet);
	{execute, power_up} -> 
		powered(Name, Parent, InPortSet, OutPortSet, VisitorSet);
	decommissioned ->
		ok; %% to do : archive the resource, possibly with a reinstatement option
	_Any ->  %% flush and ignore messages that are irrelevant when idling 
		idling(Name, Parent, InPortSet, OutPortSet, VisitorSet)
	end. 

powered(Name, Parent, InPortSet, OutPortSet, VisitorSet) -> 
    ?DLOG({power_on, Name, Parent, InPortSet, OutPortSet, VisitorSet}), 
	receive
	{execute, power_down} -> 
		idling(Name, Parent, InPortSet, OutPortSet, VisitorSet);
	{execute, {processing_step, Param}} -> 
		processing(Name, Parent, InPortSet, OutPortSet, VisitorSet, Param);
	{execute, {transfer_out, Param}} -> 
		transfering(Name, Parent, InPortSet, OutPortSet, VisitorSet, Param );
	{accept, RequesterPid, Link, _ } -> 
		RequesterPid ! {ready, Link, self()},
		powered(Name, Parent, InPortSet, OutPortSet, VisitorSet);
	{arrived, _Time, _Link, Param} -> 
		powered(Name, Parent, InPortSet, OutPortSet, dict:store(visitor_id(Param), nil, VisitorSet) );		
	_Any ->  %% flush and ignore other messages  
		powered(Name, Parent, InPortSet, OutPortSet, VisitorSet)	
	end. 

transfering(Name, Parent, InPortSet, OutPortSet, VisitorSet, Param ) -> 
    ?DLOG({transfer, Name, Parent, Param, InPortSet, OutPortSet, VisitorSet}), 
	Link = port_id(Param),
	tc51phylnk:get_to(Link) ! {accept, self(), Link, Param},
	receive
	{ready, Link, NotifyPid} -> 
		tc51eventmgr:postincr(duration(Param), self(), {transferred_out, Link, NotifyPid, Param})
	end,
	receive
	{notify, Time, Token, {transferred_out, Link, ReceiverPid, Param}} -> 
		ReceiverPid ! {arrived, Time, Link, Param}, 
		tc51eventmgr:returntoken(Token, self()),
		powered(Name, Parent, InPortSet, OutPortSet, dict:erase(visitor_id(Param), VisitorSet) )
	end. 

port_id( { _ , _ , PortPid, _ }) -> PortPid. 	
duration( { _ ,ProcessingTime, _ } ) -> ProcessingTime. % = a placeholder for more realistic models. 
visitor_id( { Pid, _ , _ } ) -> Pid. 
	
processing(Name, Parent, InPortSet, OutPortSet, VisitorSet, Param) -> 
    ?DLOG({process, Name, Parent, Param, InPortSet, OutPortSet, VisitorSet}), 
	ValidNotification = make_ref(),
	tc51eventmgr:postincr(duration(Param), self(), {ValidNotification, Param}),	
	receive
	{notify, Time, Token, {ValidNotification, Param}} -> 
		visitor_id(Param) ! {done, Time, Param}, 
		tc51eventmgr:returntoken(Token, self()),
		powered(Name, Parent, InPortSet, OutPortSet, VisitorSet)
	end. 
	
	
	

