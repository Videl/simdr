-module(tc51testmodule).

-compile(export_all).

loop() ->
    receive
	{stop} -> bye;
	{user, posti, Time, Load} ->
	    tc51eventmgr:postincr(Time, self(), Load),
	    loop();
	{notify, _Time, Token, Load} ->
		spawn(?MODULE, receiver, [Token, Load]),
	    loop();
	V ->
	    io:format("Received: ~w.~n", [V]),
	    loop()
    end.

receiver(Token, {coucou}) ->
    io:format("Received notify! (Load: ~w).~n", [{coucou}]),
    Secs = 30,
    io:format("Sending token back in ~w seconds.~n", [Secs]),
    timer:sleep(Secs * 1000),
    io:format("Sending token. ({coucou})~n"),
    tc51eventmgr:returntoken(Token, self());
receiver(Token, Load) ->
    io:format("Received notify! (Load: ~w).~n", [Load]),
    % io:format("Sending token back in five seconds.~n"),
    % timer:sleep(5000),
    tc51eventmgr:returntoken(Token, self()).