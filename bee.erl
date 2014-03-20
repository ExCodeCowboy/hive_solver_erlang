-module(bee).
-export([start_bee/4]).

-define(ActiveTime, 100).
-define(Accuracy,95).
-define(WagglePersuation,70).

-record(probDef, {evalFunc,ranSolFunc,neighborFunc}).
-record(beeContext, {hivePid,
                    beePid,
                    prob,
                    bestSolution,
                    bestSolutionScore,
                    remainingLife}).



start_bee(HivePid, Prob, active, LifeTime)->
    start_bee(HivePid,Prob,{active,?ActiveTime},LifeTime);

start_bee(HivePid, Prob, Action, LifeTime)->
    random:seed(crypto:rand_uniform(1, 200000000),
                crypto:rand_uniform(1, 200000000),
                crypto:rand_uniform(1, 200000000)),
    StartSolution = (Prob#probDef.ranSolFunc)(),
    StartScore = (Prob#probDef.evalFunc)(StartSolution),
    InitialBeeContext = 
        #beeContext{hivePid=HivePid,
                   beePid = self(),
                   prob=Prob,
                   bestSolution=StartSolution,
                   bestSolutionScore=StartScore,
                   remainingLife=LifeTime},
    bee(Action,InitialBeeContext).



bee(_Action,Context) when Context#beeContext.remainingLife==0 ->
    Context#beeContext.hivePid ! {dead,Context#beeContext.beePid};

bee(initializeInactive,Context)->
    Context#beeContext.hivePid!{startLanded,Context#beeContext.beePid},
    bee(inactive,age(Context));

%Bee has run out of time and needs to return to the hive
bee({active,RemainingIterations},Context) when RemainingIterations == 0 ->
    %then go inactive
    Context#beeContext.hivePid!{comingHome,Context#beeContext.beePid},
    bee(inactive,age(Context));

%Bee is actively searching
bee({active,RemainingIterations},Context)->
    Neighbor = ((Context#beeContext.prob)#probDef.neighborFunc)(Context#beeContext.bestSolution),
    NeighborScore = ((Context#beeContext.prob)#probDef.evalFunc)(Neighbor),
    %if neghbor is better
    BetterChoice = fuzzy_choice(NeighborScore>Context#beeContext.bestSolutionScore),
    if BetterChoice ->
        Context#beeContext.hivePid!{newBest, Context#beeContext.beePid, Neighbor, NeighborScore},
        bee({active,RemainingIterations},
            age(Context#beeContext{bestSolution=Neighbor,
                                   bestSolutionScore=NeighborScore}));
    true->
        bee({active,RemainingIterations-1},age(Context))
    end;

bee(inactive,Context)->
    receive
        {forage}->
            bee({active,?ActiveTime},age(Context));
        {newBest,BeeBest,BeeBestScore}->
            Persuaded = ?WagglePersuation =< random:uniform(100),
            BetterChoice = BeeBestScore>Context#beeContext.bestSolutionScore,
            if BetterChoice and Persuaded ->
                io:format("~p learned new best ~p ~n",[Context#beeContext.beePid,BeeBest]),
                bee(inactive,
                    age(Context#beeContext{bestSolution=BeeBest,
                                   bestSolutionScore=BeeBestScore}));
            true->
                bee(inactive,age(Context))
        end
    after 50->
        bee(inactive,age(Context))
    end;

bee(scouting,Context)->
    %try random
    RanSol = ((Context#beeContext.prob)#probDef.ranSolFunc)(),
    RanSolScore = ((Context#beeContext.prob)#probDef.evalFunc)(RanSol),
    %if neighbor is better
    
    BetterChoice = fuzzy_choice(RanSolScore>Context#beeContext.bestSolutionScore),
    if BetterChoice ->
        %need to insert possible mistakes that bee makes
        Context#beeContext.hivePid!{newBest, Context#beeContext.beePid, RanSol, RanSolScore},
        bee(scouting,
            age(Context#beeContext{bestSolution=RanSol,
                                   bestSolutionScore=RanSolScore}));
    true->
        bee(scouting,age(Context))
    end.

age(Context)->
    Context#beeContext{remainingLife = Context#beeContext.remainingLife-1}.

fuzzy_choice(Truth)->
    Chance = random:uniform(100),
    if Chance > ?Accuracy -> 
        %io:format("Bee ~p made a mistake. ~n",[self()]),
        not Truth;
    true->
        Truth
    end.