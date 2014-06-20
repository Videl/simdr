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
   Conv = actor_conveyor:create(),
  RF=spawn(actor_rfid, idling, [RFID]),
  Co=spawn(actor_conveyor, idling, [Conv]),
   Co ! {start},
  Co! {self(), {add, option, {out, RF}}},
  RF! {start},
 Co!{self(), {actor_product, ProdConf, id}},
 Co!{self(), {actor_product, ProdConf2, id}},
 Co!{self(), {actor_product, ProdConf3, id}},
 rec().

%% Ok ! {self(), {actor_product,ProdConf, id}},
 %% Ok ! {self(), {actor_product,ProdConf2, id}},
 %% Ok ! {self(), {actor_product,ProdConf3, id}},
 %% Ok ! {self(), {actor_product,ProdConf, id}},
 %% Ok ! {self(), {actor_product,ProdConf, id}},
 %% Ok ! {self(), {actor_product,ProdConf, id}},
 %% Ok ! {self(), {actor_product,ProdConf, id}},
  %% Ok ! {self(), {status, list_data}},


 rec()->
 receive 
 	V ->io:format("~w~n",[V]),
 		?MODULE:rec()
 	after 60000 -> 
 		bye
  end.