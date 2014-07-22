%% Simulation/emulation time base and calendar for the ets-based implementation.
%% Authors:	Paul Valckenaers (K.U.Leuven Association, Belgium), 
%%			Matias Novias (Intec, Argentina)

  -record (tc51event, {	time_ref, 	% {integer(), reference()}
						notifyPid, 	% pid()
						load		% any()
					  }
		   ). 
%% time_ref >>> time plus unique reference, which turns the ordered ets:set into a duplicate_bag. 