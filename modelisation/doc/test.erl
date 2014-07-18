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
  
  RF! {start},
  Co! {self(), {add, option, {out, RF}}},
    actor_contract:work(1),
 Co!{self(), {actor_product, ProdConf}},
     actor_contract:work(1),
  Co!{self(), {actor_product, ProdConf2}},
%%      actor_contract:work(1),
%% Co!{self(), {actor_product, ProdConf3}},
% RF!{self(), {actor_product, ProdConf2}},
 %Co!{self(), {actor_product, ProdConf2, id}},
 %Co!{self(), {actor_product, ProdConf3, id}},
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
 	after 10000 -> 
 		bye
  end.