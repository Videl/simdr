 NW = spawn(actor_workstation, idling, [actor_workstation:create()]).
<0.45.0>
2> NQ = spawn(actor_basic_queue, idling, [actor_basic_queue:create(NW)]).
<0.47.0>
3> NQ ! {start}
3> .
{start}
4> NW ! {start}.
{start}
Testing... []
Nothing to send!!!
Testing... []      
Nothing to send!!! 
Testing... []                    
Nothing to send!!!               
Testing... []                    
Nothing to send!!!               
Testing... []                                  
Nothing to send!!!                             
5> NQ ! {self(), {actor_product, actor_product:create(), register}}.