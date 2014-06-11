-module(geocoder_mock).
-export([create/0, geocode/2]).
-behaviour(geocoder).
-record(geocode_coordinates, {latitude, longitude}).



 
% Create a new instance 
create() ->
    geocoder:create(?MODULE, undefined).
 
geocode(_State, _AddressString) ->
    {ok, #geocode_coordinates{latitude = "43.162523", longitude = "-87.915512"}}.