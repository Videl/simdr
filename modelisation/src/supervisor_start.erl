-module(supervisor_start).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-export([
	supervisor/2,
	loop/2]).

supervisor(N , Actor) -> 
	 spawn(?MODULE, loop, [N,Actor]).

loop(0, _Actor) -> 
	io: format(" end of product sending ~n");

loop(N,Actor) -> 
	Actor ! {self(), {actor_product, actor_product:create()}},
	timer:sleep(2*1000),
	loop(N-1, Actor).
