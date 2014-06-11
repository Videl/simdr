-module(some_behaviour).

%
% Taken from 
% http://erlangcentral.org/wiki/index.php?title=Defining_Your_Own_Behaviour
%

-callback init(Args :: list(term())) -> 
	'ok' | 
	tuple('error', Reason :: string()).

-callback handle(Event :: atom()) -> 
	NextEvent :: atom().

-callback sync(Node :: node(), Timeout :: non_neg_integer()) -> 
	'ok' |
	tuple('error', Reason :: string()).
