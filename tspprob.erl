-module(tspprob).
-export([eval/0,ranSol/0,neighSol/0,getProbDef/0]).
-record(probDef, {evalFunc,ranSolFunc,neighborFunc}).


getProbDef()->
    #probDef{evalFunc=eval(),ranSolFunc=ranSol(),neighborFunc=neighSol()}.
    
eval()->
    fun(PossibleSolution)->
        [Head|Tail] = PossibleSolution,
        eval_impl(0,Head,Tail)
    end.

ranSol()->
    fun()->
        permute("ABCDEFGHIJKLMNOPQRSTUV")
    end.

neighSol()->
    fun(N)->
        SwapPos = random:uniform(length(N)-1),
        swap(N,SwapPos,SwapPos+1)
    end.

eval_impl(TotalDist, _Last, [])->
    300-TotalDist;

eval_impl(TotalDist, Last, [Head|Tail])->
    LetterDist = distance(Last,Head),
    %io:format("Distancd between ~p and ~p is ~p ~n",[Last,Head,LetterDist]),
    NewDist = TotalDist + LetterDist,
    eval_impl(NewDist,Head,Tail).

distance(From,To) when From < To ->
    (To-From);

distance(From,To) ->
    (1.5 * (From-To)).

swap(List, S1, S2) -> 
    {List2,[F|List3]} = lists:split(S1-1,List),
    LT = List2++[lists:nth(S2,List)|List3],
    {List4,[_|List5]} = lists:split(S2-1,LT),
    List4++[F|List5].

%Permutate List Function - grabbed from
% https://gist.github.com/baali/2349226
permute(List) ->
    permute(List, length(List)).
 
permute(List, Length) ->
    Indices = [],
    Permuted_List = [],
    jumble(List, Permuted_List, Indices, Length).
 
jumble(_, Permuted_List, _, 0) ->
    Permuted_List;
 
jumble(List, Permuted_List, Indices, Length) ->
    Rand_Ind = random:uniform(length(List)),
    case lists:member(Rand_Ind, Indices) of
        true ->
            jumble(List, Permuted_List, Indices, Length);
        false ->
            New_Permutation = lists:append(Permuted_List, [lists:nth(Rand_Ind, List)]),
            New_Indices = lists:append(Indices, [Rand_Ind]),
            jumble(List, New_Permutation, New_Indices, Length - 1)
    end.
