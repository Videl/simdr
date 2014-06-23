NW = spawn(actor_workstation, idling, [actor_workstation:create()]).
NQ = spawn(actor_basic_queue, idling, [actor_basic_queue:create(NW)]).
NQ ! {start}.
NW ! {start}.
                            
NQ ! {self(), {actor_product, actor_product:create(), register}}.
