-module(simpleprob).
-export([eval/0,ranSol/0,neighSol/0,getProbDef/0]).
-record(probDef, {evalFunc,ranSolFunc,neighborFunc}).


getProbDef()->
    #probDef{evalFunc=eval(),ranSolFunc=ranSol(),neighborFunc=neighSol()}.
    
eval()->
    fun(PossibleSolution)->
        math:pow(10,-(math:pow(PossibleSolution-746500,2)/8000000))
    end.

ranSol()->
    fun()->
        random:uniform(100000000)
    end.

neighSol()->
    fun(N)->
        Increment = random:uniform(3)-2,
        %io:format("trying to add ~p to ~p ~n",[N,Increment]),
        N+(Increment)
    end.