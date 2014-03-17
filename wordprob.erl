-module(wordprob).
-export([eval/0,ranSol/0,neighSol/0]).

eval()->
	fun(PossibleSolution)->
        
        RealSolution = "THISISHARDERTHANITLOOKS",
        Distance = lists:foldl(fun(X,A)-> X+A end, 0, lists:zipwith(fun(X,Y)->
        	if (X == Y)->
        		0;
        	true->
        		1
        	end
        end, RealSolution, PossibleSolution)),
        %io:format("~p to ~p (Distance ~p)~n",[PossibleSolution,RealSolution,Distance]),
				
        23-Distance
	end.

ranSol()->
	fun()->
		get_random_string(23)
	end.

neighSol()->
	fun(N)->
		Pos = random:uniform(length(N)),
		{Pre,[_|Tail]} = lists:split(Pos-1,N),
		Pre ++ [get_random_string(1)|Tail]
	end.


get_random_string(Length) ->
	AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).