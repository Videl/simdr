-record(actor, {
	module :: atom(),
	name :: any(),
	pid :: integer() | pid(),
	opt :: number(),  %% ETS table
	state :: atom(),
	in :: list(pid()),
	out :: list(pid()),
	in_out :: undefined | list(pid()), 
	capacity :: infinity | non_neg_integer(),
	work_time :: non_neg_integer(), 
	list_data :: number()  %% ETS table
	}).
