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
   actor_contract:create(?MODULE, actor_conveyor, off, undefined, 42, []).

answer(ConveyorConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(ConveyorConfig)),
	{actor_contract:get_id(ConveyorConfig), finish, actor_contract:get_id(ProductConfig)}.

