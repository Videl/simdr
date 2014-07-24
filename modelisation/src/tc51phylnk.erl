-module(tc51phylnk). %% ports linking phyres. 
-include("app_configuration.hrl").

-export([create/2, get_from/1, get_to/1, set_from_port/2, set_to_port/2]).
-export([loop/4]). %% for internal use - spawn - only. 

create(Name, Parent) -> 
	C = spawn(tc51phylnk, loop, [Name, Parent, unconnected, unconnected]),	
	?DLOG({"Creating physical link:", C, "named:", Name , "parent is:", Parent}), 
	C.
	
	
get_from(LnkPid) ->
	LnkPid ! {get_from, self()}, 	
	receive
		{ResPid, ok} -> ResPid
	end.	

get_to(LnkPid) ->
	LnkPid ! {get_to, self()},
	receive
		{ResPid, ok} -> ResPid
	end.	
	
set_from_port(LnkPid, ResPid) -> LnkPid ! {set_from, ResPid}.

set_to_port(LnkPid, ResPid) -> LnkPid ! {set_to, ResPid}. 
	
loop(Name, Parent, FromRes, ToRes) -> 
	receive 
	{set_from, From} -> 
		?DLOG({"Setting From :", From, "named:", Name , self(), " parent is:", Parent}),
		?MODULE:loop(Name, Parent, From, ToRes); 
	{set_to, To}     -> 
		?DLOG({"Setting   To :",   To, "named:", Name , self(), " parent is:", Parent}),
		loop(Name, Parent, FromRes, To); 
	{get_from, Pid}  ->
		Pid ! {FromRes, ok},
		?MODULE:loop(Name, Parent, FromRes, ToRes); 
	{get_to, Pid} ->
		Pid ! {ToRes, ok},
		?MODULE:loop(Name, Parent, FromRes, ToRes)	
	end. 



