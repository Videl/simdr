-module(geocoder).
-export([behaviour_info/1, create/2, geocode/2]).

-record(geocoder, {module, state}).


behaviour_info(callbacks) -> [{geocode,2}];
behaviour_info(_Other) -> undefined.

create(Module, State) ->
    Geocoder = #geocoder { module = Module, state = State },
    {ok, Geocoder}.

geocode(Geocoder, AddressString) ->
    (Geocoder#geocoder.module):geocode(Geocoder#geocoder.state, AddressString).


