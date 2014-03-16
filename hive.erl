-module(hive).
-export([start_hive/3]).

start_hive(EvalFunc,RanSolFunc,NeighborFunc) ->
	
	BeeLifetime = 1000,
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

	lists:map(fun(A)->bee(HivePid,
						 inactive,
						 NeighborFunc,
						 RanSolFunc,
						 EvalFunc) end,lists:seq(1, 100)),
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
					lists:map(fun(I)->
						I!{newBest,BeeBest,BeeBestScore}
					end,InactiveBees),
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

			{dead,BeePid} ->
				RemainingBees = LivingBees-1,
				io:format(" ~p pings~n",[RemainingBees]),
				if RemainingBees > 0 ->
						ShortCall(BestSolution,
								  BestSolutionScore,
								 InactiveBees,
								 RemainingBees);
				true ->
					CallBack(BestSolution)
				end
		end.

bee(HivePid,
	State,
	NeighborFunc,
	RanSolFunc,
	EvalFunc) ->

	HivePid ! {dead,self()}.
