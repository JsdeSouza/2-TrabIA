%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                         
%   Implementação para o mundo dos blocos  %
%%%                                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Definição dos blocos e espaços
block(a).
block(b).
block(c).
block(d).

place(1).
place(2).
place(3).
place(4).

% Representação de estados
% Exemplo de estado inicial e final
initial_state([clear(2), clear(3), clear(4), on(a, 1), on(b, a), on(c, 2), on(d, c)]).
final_state([clear(4), on(a, 1), on(b, 2), on(c, 3), on(d, 4)]).

% Definição de ações
% can(Action, Precondition): Ação é possível se as condições forem verdadeiras
can(move(Block, From, To), [clear(Block), clear(To), on(Block, From), different(Block, To), different(From, To)]).

% Adição de fatos após a execução de uma ação
adds(move(Block, _, To), [on(Block, To), clear(From)]).

% Remoção de fatos após a execução de uma ação
deletes(move(Block, From, _), [on(Block, From), clear(To)]).

% Verificação de incompatibilidades
impossible(on(X, X), _).
impossible(on(X, Y), Goals) :-
    member(clear(Y), Goals) ;
    member(on(X, Y1), Goals), Y1 \== Y ;
    member(on(X1, Y), Goals), X1 \== X.

impossible(clear(X), Goals) :-
    member(on(_, X), Goals).

% Planejador atualizado
% plan(State, Goals, Plan, FinalState): Gera um plano para alcançar Goals a partir de State
plan(State, Goals, [], State) :-
    satisfied(State, Goals).

plan(State, Goals, Plan, FinalState) :-
    append(PrePlan, [Action | PostPlan], Plan),
    select_goal(Goals, Goal),
    achieves(Action, Goal),
    can(Action, Conditions),
    satisfied(State, Conditions),
    apply(State, Action, MidState),
    regress(Goals, Action, NewGoals),
    plan(MidState, NewGoals, PrePlan, FinalState).

% Verificação de objetivos satisfeitos
satisfied(_, []).
satisfied(State, [Goal | Goals]) :-
    member(Goal, State),
    satisfied(State, Goals).

% Seleção de um objetivo
select_goal(Goals, Goal) :-
    member(Goal, Goals).

% Regressão dos objetivos através de uma ação
regress(Goals, Action, NewGoals) :-
    adds(Action, Additions),
    deletes(Action, Deletions),
    subtract(Goals, Additions, TempGoals),
    append(TempGoals, Deletions, NewGoals).

% Aplicação da ação para transição de estado
apply(State, Action, NewState) :-
    deletes(Action, DelList),
    delete_all(State, DelList, TempState),
    adds(Action, AddList),
    append(AddList, TempState, NewState).

% Predicados auxiliares
subset([], _).
subset([H | T], List) :-
    member(H, List),
    subset(T, List).

subtract([], _, []).
subtract([H | T], L2, Result) :-
    member(H, L2),
    subtract(T, L2, Result).
subtract([H | T], L2, [H | Result]) :-
    \+ member(H, L2),
    subtract(T, L2, Result).

% Teste de execução do plano
run_plan :-
    initial_state(Init),
    final_state(Goal),
    plan(Init, Goal, Plan, _),
    writeln('Plano gerado:'),
    writeln(Plan).
