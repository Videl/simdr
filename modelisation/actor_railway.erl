-module(actor_conveyor).
-behaviour(actor_contract).
-include(config.hlr).

-export([
	get_name/1, 
	get_opt/1, 
	start/1, 
	stop/1, 
	work_time/1, 
	get_data/1, 
	get_previous_data/2, 
	get_state/1
	]).

create() ->
   actor_contract:create(?MODULE, actor_railway, off, undefined, 10, []).

answer(RailwayConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(RailwayConfig)),
	{answer, actor_contract:get_state(RailwayConfig), actor_contract:get_id(ProductConfig)}.

