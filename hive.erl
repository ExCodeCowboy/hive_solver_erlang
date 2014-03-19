-module(hive).
-export([start_hive/1]).

-record(probDef, {evalFunc,ranSolFunc,neighborFunc}).
-record(hiveState,{bestSolution=0,
				   bestSolutionScore=0,
				   inactiveBees=queue:new(),
				   livingBees}).

-define(BeeLifetime, 30000).
-define(NumActive, 50).
-define(NumScouts, 30).
-define(NumInactive,15).
-define(ActiveTime, 100).
-define(Accuracy,95).
-define(WagglePersuation,70).


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
					{active,?ActiveTime}
				end,
			BeeSol = (Prob#probDef.ranSolFunc)(),
			spawn(
				fun()->
					random:seed(crypto:rand_uniform(1, 200000000),
						crypto:rand_uniform(1, 200000000),
						crypto:rand_uniform(1, 200000000)),
					bee(HivePid,
			     	BeeSol,
			     	(Prob#probDef.evalFunc)(BeeSol),
				 	Status,
				 	Prob,
					?BeeLifetime)
				end) 
		end,lists:seq(1, TotalBees)),
	receive
		{solution,Solution}->
			io:format("Finishing Hive run. ~p is best solution",[Solution])
	end.


run_hive(State,
		 CallBack)->

		receive 
			{newBest, BeePid, BeeBest, BeeBestScore} ->
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

bee(HivePid,
	BestSolution,
	BestSolutionScore,
	State,
	Prob,
	LifeTime) ->
		ShortCall = fun(BestSolution_S,
						BestSolutionScore_S,
						State_S) ->
			bee(HivePid,
				BestSolution_S,
				BestSolutionScore_S,
				State_S,
				Prob,
				LifeTime-1)
		end,
	
	BeePid = self(),
	
	if LifeTime == 0 ->
		HivePid ! {dead,BeePid};
	true ->
		case State of
			initializeInactive->
				%signal the hive you are coming home
				HivePid!{startLanded,BeePid},
				%then go inactive
				ShortCall(
					BestSolution,
					BestSolutionScore,
					inactive);
			{active,RemainingIterations}->
				if RemainingIterations == 0 ->
					%then go inactive
					HivePid!{comingHome,BeePid},
					ShortCall(
						BestSolution,
						BestSolutionScore,
						inactive);
				true->
					%try neighboor
					Neighbor = (Prob#probDef.neighborFunc)(BestSolution),
					NeighborScore = (Prob#probDef.evalFunc)(Neighbor),
					%if neghbor is better
					BetterChoice = fuzzy_choice(NeighborScore>BestSolutionScore),
					if BetterChoice ->
						%need to insert possible mistakes that bee makes
						HivePid!{newBest, BeePid, Neighbor, NeighborScore},
						ShortCall(Neighbor,
								  NeighborScore,
								  {active,RemainingIterations});
					true->
						ShortCall(BestSolution,
								  BestSolutionScore,
								  {active,RemainingIterations-1})
					end
				end;
			inactive->
				receive
					{forage}->
						ShortCall(BestSolution,
							BestSolutionScore,
							{active,?ActiveTime});
					{newBest,BeeBest,BeeBestScore}->
						Persuaded = ?WagglePersuation =< random:uniform(100),
						BetterChoice = BeeBestScore>BestSolutionScore,
						if BetterChoice and Persuaded ->
							io:format("~p learned new best ~p ~n",[BeePid,BeeBest]),
							ShortCall(BeeBest,
								BeeBestScore,
								inactive);
						true->
							ShortCall(BestSolution,
							BestSolutionScore,
							inactive)
					end
				after 50->
					ShortCall(BestSolution,
							BestSolutionScore,
							inactive)
				end;
			scouting->
				%try neighboor
				RanSol = (Prob#probDef.ranSolFunc)(),
				RanSolScore = (Prob#probDef.evalFunc)(RanSol),
				%if neghbor is better
				BetterChoice = fuzzy_choice(RanSolScore>BestSolutionScore),
				if BetterChoice ->
					%need to insert possible mistakes that bee makes
					HivePid!{newBest, BeePid, RanSol, RanSolScore},
					ShortCall(RanSol,
							  RanSolScore,
							  scouting);
				true->
					ShortCall(BestSolution,
							  BestSolutionScore,
							  scouting)
				end
		end
	end.


fuzzy_choice(Truth)->
	Chance = random:uniform(100),
	if Chance > ?Accuracy -> 
		%io:format("Bee ~p made a mistake. ~n",[self()]),
		not Truth;
	true->
		Truth
	end.













