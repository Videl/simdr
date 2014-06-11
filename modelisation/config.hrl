
-behaviour(actor_contract).
-record(config, {
	name, 
	opt, 
	state, 
	work_time, 
	list_data
	}).

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