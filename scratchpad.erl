{inNest,BeePid} ->
		 		ShortCall(BestSolution,
						 queue:in(BeePid,InactiveBees),
						 LivingBees));
			
			{returning,BeePid} -> 
				case queue:out(InactiveBees) of
					{{_,LeavingBeePid},RemainingInactive} ->
						LeavingBeePid ! {forage},
						ShortCall(BestSolution,
								 queue:in(BeePid,RemainingInactive),
								 TotalBees);						
					{empty,RemainingInactive} ->
						ShortCall(BestSolution,
								 queue:in(BeePid,RemainingInactive),
								 TotalBees)
				end;

				 hive:start_hive(simpleprob:eval(),simpleprob:ranSol(),simpleprob:neighSol()).

				 hive:start_hive(wordprob:eval(),wordprob:ranSol(),wordprob:neighSol()).

				  c(hive). c(wordprob). hive:start_hive(wordprob:eval(),wordprob:ranSol(),wordprob:neighSol()).