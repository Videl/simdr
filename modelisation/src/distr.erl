-module(distr).
-export([t/1]). 

t(From) -> From ! self().