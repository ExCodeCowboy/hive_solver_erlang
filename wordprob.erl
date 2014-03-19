-module(wordprob).
-export([eval/0,ranSol/0,neighSol/0,getProbDef/0]).
-record(probDef, {evalFunc,ranSolFunc,neighborFunc}).


getProbDef()->
    #probDef{evalFunc=eval(),ranSolFunc=ranSol(),neighborFunc=neighSol()}.

eval()->
	fun(PossibleSolution)->
        
        RealSolution = "THISISTHESOLUTIONSTRING",
        Distance = lists:foldl(fun(X,A)-> X+A end, 0, lists:zipwith(fun(X,Y)->
        	if (X == Y)->
        		0;
        	true->
        		Dif = X - Y,
        		abs(Dif)
        	end
        end, RealSolution, PossibleSolution)),
        %io:format("~p to ~p (Distance ~p)~n",[PossibleSolution,RealSolution,Distance]),
				
        1/(1+Distance)
	end.

ranSol()->
	fun()->
		get_random_string(23)
	end.

neighSol()->
	fun(N)->
		Pos = random:uniform(length(N)),
		Increment = (random:uniform(3)-2),
		{Pre,[Selected|Tail]} = lists:split(Pos-1,N),
		Pre ++ [(Selected+Increment)|Tail]
	end.


get_random_string(Length) ->
	AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).