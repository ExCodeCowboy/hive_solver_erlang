-module(hive).
-export([start_hive/1]).

-record(probDef, {evalFunc,ranSolFunc,neighborFunc}).

-define(BeeLifetime, 30000).
-define(NumActive, 50).
-define(NumScouts, 300).
-define(NumInactive,150).
-define(ActiveTime, 100).
-define(Accuracy,95).
-define(WagglePersuation,70).


start_hive(Prob) ->
	
	TotalBees = ?NumInactive + ?NumScouts + ?NumActive,

	MyPid = self(),
	CallBack = fun(Solution) ->
		MyPid ! {solution,Solution}
	end,

	HiveStart =  fun() -> run_hive(Prob,
								   0,
								   0,
								   queue:new(),
								   TotalBees,
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


run_hive(Prob,
		 BestSolution,
		 BestSolutionScore,
		 InactiveBees,
		 LivingBees,
		 CallBack)->
		
		ShortCall = fun(BestSolution_S,
						BestSolutionScore_S,
						InactiveBees_S,
						LivingBees_S)->
			run_hive(Prob,
					 BestSolution_S,
					 BestSolutionScore_S,
		 			 InactiveBees_S,
					 LivingBees_S,
					 CallBack)
		end,

		receive 
			{newBest, BeePid, BeeBest, BeeBestScore} ->
				%io:format("Bee ~p reports ~p as new personal best of ~p vs ~p ~n",
				%	[BeePid,BeeBest, BeeBestScore,BestSolutionScore]),
					lists:map(fun(I) ->
						I ! {newBest,BeeBest,BeeBestScore}
					end,queue:to_list(InactiveBees)),

				if BeeBestScore >= BestSolutionScore ->
					io:format("Saving ~p as new hive best of ~p ~n",
						[BeeBest, BeeBestScore]),
					%Notify the inactive bees
					ShortCall(BeeBest,
							  BeeBestScore,
							  InactiveBees,
							  LivingBees);
				true->
					ShortCall(BestSolution,
							  BestSolutionScore,
							  InactiveBees,
							  LivingBees)
				end;
			{startLanded, BeePid} ->
				io:format("Bee ~p starts landed ~n",[BeePid]),
				ShortCall(BestSolution,
					BestSolutionScore,
					queue:in(BeePid,InactiveBees),
					LivingBees);						
			{comingHome, BeePid} ->
				%io:format("Bee ~p lands ~n",[BeePid]),
				case queue:out(InactiveBees) of
					{{_,LeavingBeePid},RemainingInactive} ->
						LeavingBeePid ! {forage},
						%io:format("Bee ~p takes off ~n",[LeavingBeePid]),
						ShortCall(BestSolution,
								 BestSolutionScore,
								 queue:in(BeePid,RemainingInactive),
								 LivingBees);						
					{empty,RemainingInactive} ->
						BeePid ! {forage},
						ShortCall(BestSolution,
								 BestSolutionScore,
								 RemainingInactive,
								 LivingBees)
				end;
			{dead,BeePid} ->
				RemainingBees = LivingBees-1,
				io:format("Bee ~p dies, ~p remaining ~n",[BeePid,RemainingBees]),
				if RemainingBees > 0 ->
						
					RemainingInactive = 
						queue:filter(fun(I)-> I/=BeePid end,InactiveBees),
								
					NumInactive = queue:len(RemainingInactive),
					io:format("Current Idle bee count ~p ~n",[NumInactive]),

					%if only inactive are left, release them.
					if NumInactive*2 > RemainingBees ->
						io:format("Releasing remaining idle bees ~n"),
						lists:map(fun(I) ->
							%io:format("Bee ~p takes off ~n",[I]),
							I ! {forage}
						end,queue:to_list(RemainingInactive)),
						ShortCall(BestSolution,
								  BestSolutionScore,
								  queue:new(),
								  RemainingBees);
					true->
						ShortCall(BestSolution,
								  BestSolutionScore,
								  RemainingInactive,
								  RemainingBees)
					end;

				true ->
					CallBack(BestSolution)
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













