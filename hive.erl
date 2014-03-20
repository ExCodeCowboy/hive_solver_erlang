-module(hive).
-export([start_hive/1]).
-import(bee,[start_bee/4]).

-record(hiveState,{bestSolution=0,
				   bestSolutionScore=0,
				   inactiveBees=queue:new(),
				   livingBees}).

-define(BeeLifetime, 30000).
-define(NumActive, 50).
-define(NumScouts, 30).
-define(NumInactive,15).



start_hive(Prob) ->
	
	TotalBees = ?NumInactive + ?NumScouts + ?NumActive,

	MyPid = self(),
	CallBack = fun(Solution) ->
		MyPid ! {solution,Solution}
	end,

	HiveStart =  fun() -> run_hive(#hiveState{
								   livingBees=TotalBees},
								   CallBack) end, 

	HivePid = spawn(HiveStart),

	lists:map(
		fun(A)->
			Status = 
				if A =< ?NumInactive ->
					initializeInactive;
				A =< ?NumInactive+?NumScouts ->
					scouting;
				true->
					active
				end,
			spawn(
				fun()->
					start_bee(HivePid,Prob,Status,?BeeLifetime)
				end) 
		end,lists:seq(1, TotalBees)),
	receive
		{solution,Solution}->
			io:format("Finishing Hive run. ~p is best solution",[Solution])
	end.


run_hive(State,
		 CallBack)->

		receive 
			{newBest, _BeePid, BeeBest, BeeBestScore} ->
				%io:format("Bee ~p reports ~p as new personal best of ~p vs ~p ~n",
				%	[BeePid,BeeBest, BeeBestScore,BestSolutionScore]),
					lists:map(fun(I) ->
						I ! {newBest,BeeBest,BeeBestScore}
					end,queue:to_list(State#hiveState.inactiveBees)),

				if BeeBestScore >= State#hiveState.bestSolutionScore ->
					io:format("Saving ~p as new hive best of ~p ~n",
						[BeeBest, BeeBestScore]),
					%Notify the inactive bees
					run_hive(State#hiveState{
						bestSolution=BeeBest,
						bestSolutionScore=BeeBestScore},
						CallBack);
				true->
					run_hive(State,CallBack)
				end;
			{startLanded, BeePid} ->
				io:format("Bee ~p starts landed ~n",[BeePid]),
				run_hive(State#hiveState{
						inactiveBees=queue:in(BeePid,State#hiveState.inactiveBees)},
						CallBack);						
			{comingHome, BeePid} ->
				%io:format("Bee ~p lands ~n",[BeePid]),
				case queue:out(State#hiveState.inactiveBees) of
					{{_,LeavingBeePid},RemainingInactive} ->
						LeavingBeePid ! {forage},
						%io:format("Bee ~p takes off ~n",[LeavingBeePid]),
						run_hive(State#hiveState{
							inactiveBees=queue:in(BeePid,RemainingInactive)},
							CallBack);						
					{empty,RemainingInactive} ->
						BeePid ! {forage},
						run_hive(State#hiveState{
							inactiveBees=RemainingInactive},
							CallBack)
				end;
			{dead,BeePid} ->
				RemainingBees = State#hiveState.livingBees-1,
				io:format("Bee ~p dies, ~p remaining ~n",[BeePid,RemainingBees]),
				if RemainingBees > 0 ->
						
					RemainingInactive = 
						queue:filter(fun(I)-> I/=BeePid end,State#hiveState.inactiveBees),
								
					NumInactive = queue:len(RemainingInactive),
					io:format("Current Idle bee count ~p ~n",[NumInactive]),

					%if only inactive are left, release them.
					if NumInactive*2 > RemainingBees ->
						io:format("Releasing remaining idle bees ~n"),
						lists:map(fun(I) ->
							%io:format("Bee ~p takes off ~n",[I]),
							I ! {forage}
						end,queue:to_list(RemainingInactive)),
						run_hive(State#hiveState{
						inactiveBees=queue:new(),
						livingBees=RemainingBees},
						CallBack);	
					true->
						run_hive(State#hiveState{
							inactiveBees=RemainingInactive,
							livingBees=RemainingBees},
							CallBack)
					end;

				true ->
					CallBack(State#hiveState.bestSolution)
				end
		end.















