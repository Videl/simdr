%% Author: jphilips
%% Created: 17-mei-2012
%% Adapted: 28-december 2012 (pvalckenaers)

-module(tc51debug).

-export([wait/0, stop/0]).

stop() -> tc51debug ! die. 

wait() ->
    receive
        die -> void
    end.