-module(simdr_tools).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	get_option_from_ets/2,
	add_data_in_ets/2,
	set_option_in_ets/3,
	delete_option_in_ets/2,
	add_option_in_ets/3
	]).

get_option_from_ets(Table, Key) ->
	Var = ets:lookup(Table, Key),
	Option = get_option_helper(Var, Key),
	Option.

add_data_in_ets(Table, Data) ->
	true = ets:insert_new(Table, Data),
	true.

set_option_in_ets(Table, Key, Value) ->
	delete_option_in_ets(Table, Key),
	add_option_in_ets(Table, Key, Value).

delete_option_in_ets(Table, Key) ->
	ets:delete(Table, Key).

add_option_in_ets(Table, Key, Value) ->
	ets:insert(Table, {Key, Value}).

%%
%% Internal API
%%

get_option_helper(AllOptions, Key) ->
	get_option_helper_two(AllOptions, [], Key).

get_option_helper_two([], [], _Key) ->
	unknown_option;
get_option_helper_two([], Result, _Key) ->
	Result;
get_option_helper_two([{Key, Value}|RestOfOptions], Result, Key) ->
	get_option_helper_two(RestOfOptions, Result ++ [Value], Key);
get_option_helper_two([_BadHead|RestOfOptions], Result, Key) ->
	get_option_helper_two(RestOfOptions, Result, Key).
