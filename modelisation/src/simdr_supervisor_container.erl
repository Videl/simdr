-module(simdr_supervisor_container).
-include("app_configuration.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
		 idling/1,
		 processing/1
		]).


%% ===================================================================
%% Loops functions
%% ===================================================================

idling(Config) ->
	receive
		{start} ->
			processing(Config);
		_ ->
			idling(Config)
	end.

processing(Config) ->
	receive
		{stop} ->
			idling(Config);
		{Sender, Request} ->
			NewConfig = (Config#supervisor.module):action_on_request(
				Config, 
				Sender, 
				Request),
			processing(NewConfig)
		after (Config#supervisor.module):timer_time(Config)*1000 ->
			NewConfig = (Config#supervisor.module):timer_action(Config),
			processing(NewConfig)
	end.

