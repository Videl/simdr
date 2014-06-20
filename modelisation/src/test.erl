-module(test).

-export([
	lancer/0,
	rec/0]).

%% Behavior implementation
lancer() ->
 RFID= actor_rfid:create(),
  ProdConf = actor_product:create(),
  ProdConf2 = actor_product:create(),
   ProdConf3 = actor_product:create(),
  Ok=spawn(actor_rfid, idling, [RFID]),
   Ok ! {start},
 Ok ! {self(), {actor_product,ProdConf, id}},
  Ok ! {self(), {actor_product,ProdConf2, id}},
  Ok ! {self(), {actor_product,ProdConf3, id}},
 Ok ! {self(), {actor_product,ProdConf, id}},
 Ok ! {self(), {actor_product,ProdConf, id}},
 Ok ! {self(), {actor_product,ProdConf, id}},
 Ok ! {self(), {actor_product,ProdConf, id}},
 rec().

 rec()->
 receive 
 	V ->io:format("~w~n",[V]),
 		?MODULE:rec()
 %	after 25000 -> 
 %		bye
  end.