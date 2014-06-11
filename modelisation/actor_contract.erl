-module(actor_contract).

-callback get_name() -> 
	Name :: string().

-callback get_opt() -> 
	list(tuple(Name :: string(), Parameters :: list(atom()))).

-callback start() -> 
	ok |
	undefined.

-callback stop() -> 
	ok |
	undefined.

-callback work_time() ->
	non_neg_integer() |
	undefined.

-callback get_data() ->
	term().

-callback get_previous_data(Step :: non_neg_integer()) ->
	term().

-callback get_state() ->
	term().