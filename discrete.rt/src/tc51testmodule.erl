-module(tc51testmodule).

-compile(export_all).

loop() ->
    receive
	{stop} -> bye;
	{user, posti, Time, Load} ->
	    tc51eventmgr:postincr(Time, self(), Load),
	    loop();
	{notify, _Time, Token, Load} ->
	    io:format("Received notify! (Load: ~w).~n", [Load]),
	    io:format("Sending token back in five seconds.~n"),
	    timer:sleep(5000),
	    tc51eventmgr:returntoken(Token, self()),
	    loop();
	V ->
	    io:format("Received: ~w.~n", [V]),
	    loop()
    end.
