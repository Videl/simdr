-module(usage).
 
-behaviour(some_behaviour).
 
-export([init/1, handle/1, sync/2]).
-export([foo/0]).
 
init(Config) ->
    Config.
 
handle(Message) ->
    Message.
 
sync(_Entry, Config) ->
    Config.
 
foo() ->
    foooooooooooooooooooooooooo.