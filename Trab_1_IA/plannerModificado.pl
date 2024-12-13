%----------------------------------------------------------
% plan(State, Goals, Plan, FinalState)
% Modificado para tratar metas dinamicamente e verificar condições
plan(State, Goals, [], State):-
    satisfied(State, Goals).
plan(State, Goals, Plan, FinalState):-
    append(Plan, _, _),
    append(PrePlan, [Action|PostPlan], Plan),
    select_priority_goal(State, Goals, Goal), % Seleciona a meta com prioridade
    achieves(Action, Goal),
    can(Action, Condition),
    plan(State, Condition, PrePlan, MidState1),
    apply(MidState1, Action, MidState2),
    plan(MidState2, Goals, PostPlan, FinalState).

%----------------------------------------------------------
% satisfied(State, Goals): Goals are true in State
satisfied(_, []).
satisfied(State, [Goal|Goals]):-
    member(Goal, State),
    satisfied(State, Goals).

%----------------------------------------------------------
% select_priority_goal(State, Goals, Goal): Seleciona meta não atingida com prioridade
select_priority_goal(State, Goals, Goal):-
    sort_goals_by_priority(Goals, SortedGoals), % Ordena metas por prioridade
    member(Goal, SortedGoals),
    \+ member(Goal, State). % A meta não é verdadeira no estado atual

%----------------------------------------------------------
% sort_goals_by_priority(Goals, SortedGoals): Ordena metas por prioridade
% Aqui assumimos que as metas são representadas como pares (Prioridade, Meta)
sort_goals_by_priority(Goals, SortedGoals):-
    maplist(priority_goal_pair, Goals, GoalPairs),
    sort(GoalPairs, SortedPairs),
    pairs_values(SortedPairs, SortedGoals).

priority_goal_pair((Priority, Goal), Priority-Goal).

%----------------------------------------------------------
% achieves(Action, Goal): goal is in add-list of Action 
achieves(Action, Goal):-
    adds(Action, Goals),
    member(Goal, Goals).

%----------------------------------------------------------
% apply(State, Action, NewState): execution of Action at 
%                                 State produces NewState
apply(State, Action, NewState):-
    deletes(Action, DelList), % get properties to be deleted
    delete_all(State, DelList, State1), !,
    adds(Action, AddList),
    append(AddList, State1, NewState).

%----------------------------------------------------------
% delete_all(L1, L2, Diff) Diff is set-difference of L1 and L2
delete_all([], _, []).
delete_all([X|L1], L2, Diff):-
    member(X, L2), !,
    delete_all(L1, L2, Diff).
delete_all([X|L1], L2, [X|Diff]):-
    delete_all(L1, L2, Diff).

% Funções auxiliares para manipulação de pares
pairs_values([], []).
pairs_values([_-Value|Rest], [Value|Values]):-
    pairs_values(Rest, Values).

member(X, [X|_]).
member(X, [_|T]):-
    member(X, T).

delete(X, [X|Tail], Tail).
delete(X, [Y|Tail], [Y|Tail1]):- 
   delete(X, Tail, Tail1).
