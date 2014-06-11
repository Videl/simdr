-module(actor_contract).

-callback get_name(Config :: term()) -> 
	Name :: string() |
	Name :: atom().

-callback get_opt(Config :: term()) -> 
	list(tuple(Name :: string(), Parameters :: list(atom()))) |
	undefined.

-callback start(Config :: term()) -> 
	ok |
	undefined.

-callback stop(Config :: term()) -> 
	ok |
	undefined.

-callback work_time(Config :: term()) ->
	non_neg_integer() |
	undefined.

-callback get_data(Config :: term()) ->
	term().

-callback get_previous_data(Config :: term(), Step :: non_neg_integer()) ->
	term().

-callback get_state(Config :: term()) ->
	term().

-callback loop(Config :: term(), ListData :: list(term())).

create(Module) ->
	Pid = spawn(Module, loop, []).
	