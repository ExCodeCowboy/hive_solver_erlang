-module(hive).
-export([start_hive/3]).

start_hive(EvalFunc,RanSolFunc,NeighborFunc) ->
	
	BeeLifetime = 60,
	NumActive = 30,
	NumScouts = 20,
	NumInactive = 10,
	TotalBees = NumInactive + NumScouts + NumActive,

	MyPid = self(),
	CallBack = fun(Solution) ->
		MyPid ! {solution,Solution}
	end,

	HiveStart =  fun() -> run_hive(EvalFunc,
								   RanSolFunc,
								   NeighborFunc,
								   0,
								   0,
								   queue:new(),
								   TotalBees,
								   CallBack) end, 

	HivePid = spawn(HiveStart),

	lists:map(
		fun(A)->
			BeeSol = RanSolFunc(),
			spawn(
				fun()->bee(HivePid,
			     	BeeSol,
			     	EvalFunc(BeeSol),
				 	inactive,
				 	NeighborFunc,
				 	RanSolFunc,
					EvalFunc,
					BeeLifetime)
				end) 
		end,lists:seq(1, 100)),
	receive
		{solution,Solution}->
			io:format("Finishing Hive run.")
	end.


run_hive(EvalFunc,
		 RanSolFunc,
		 NeighborFunc,
		 BestSolution,
		 BestSolutionScore,
		 InactiveBees,
		 LivingBees,
		 CallBack)->
		
		ShortCall = fun(BestSolution_S,
						BestSolutionScore_S,
						InactiveBees_S,
						LivingBees_S)->
			run_hive(EvalFunc,
					 RanSolFunc,
					 NeighborFunc,
					 BestSolution_S,
					 BestSolutionScore_S,
		 			 InactiveBees_S,
					 LivingBees_S,
					 CallBack)
		end,

		receive 
			{newBest, BeePid, BeeBest, BeeBestScore} ->
				if BeeBestScore > BestSolutionScore ->
					%Notify the inactive bees
					lists:map(fun(I) ->
						I ! {newBest,BeeBest,BeeBestScore}
					end,queue:to_list(InactiveBees)),
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
			{comingHome, BeePid} ->
				case queue:out(InactiveBees) of
					{{_,LeavingBeePid},RemainingInactive} ->
						LeavingBeePid ! {forage},
						ShortCall(BestSolution,
								 BestSolutionScore,
								 queue:in(BeePid,RemainingInactive),
								 LivingBees);						
					{empty,RemainingInactive} ->
						ShortCall(BestSolution,
								 BestSolutionScore,
								 queue:in(BeePid,RemainingInactive),
								 LivingBees)
				end;
			{dead,BeePid} ->
				RemainingBees = LivingBees-1,
				io:format("Bee ~p dies, ~p remaining ~n",[BeePid,RemainingBees]),
				if RemainingBees > 0 ->
						RemainingInactive = 
							queue:filter(fun(I)-> I/=BeePid end,InactiveBees),
						ShortCall(BestSolution,
								  BestSolutionScore,
								  RemainingInactive,
								  RemainingBees);
				true ->
					CallBack(BestSolution)
				end
		end.

bee(HivePid,
	BestSolution,
	BestSolutionScore,
	State,
	NeighborFunc,
	RanSolFunc,
	EvalFunc,
	LifeTime) ->
		ShortCall = fun(BestSolution_S,
						BestSolutionScore_S,
						State_S) ->
			bee(HivePid,
				BestSolution_S,
				BestSolutionScore_S,
				State_S,
				NeighborFunc,
				RanSolFunc,
				EvalFunc,
				LifeTime-1)
		end,
	
	BeePid = self(),
	
	if LifeTime == 0 ->
		HivePid ! {dead,BeePid};
	true ->
		case State of
			{active,RemainingIterations}->
				if RemainingIterations == 0 ->
					%signal the hive you are coming home
					HivePid!{comingHome,BeePid},
					%then go inactive
					ShortCall(
						BestSolution,
						BestSolutionScore,
						inactive);
				true->
					%try neighboor
					Neighbor = NeighborFunc(BestSolution),
					NeighborScore = EvalFunc(Neighbor),
					%if neghbor is better
					if NeighborScore>BestSolutionScore->
						%need to insert possible mistakes that bee makes
						HivePid!{newBest, BeePid, Neighbor, NeighborScore},
						ShortCall(Neighbor,
								  NeighborScore,
								  {active,RemainingIterations-1});
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
							{active,100});
					{newBest,BeeBest,BeeBestScore}->
						ShortCall(BeeBest,
							BeeBestScore,
							inactive)
				after 500->
					ShortCall(BestSolution,
							BestSolutionScore,
							inactive)
				end;
			scouting->
				%try neighboor
				RanSol = RanSolFunc(),
				RanSolScore = EvalFunc(RanSol),
				%if neghbor is better
				if RanSolScore>BestSolutionScore->
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













