% Detailed test of classification process
:- use_module(library(debug)).

debug_classification :-
    % Test the exact clauses from our algorithm
    Clause1 = (sum_list([], 0) :- true),
    Clause2 = (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1),
    Algorithm = [Clause1, Clause2],
    
    writeln('=== Testing Clause 1 ==='),
    writeln(Clause1),
    test_single_clause(Clause1, Algorithm),
    
    writeln('=== Testing Clause 2 ==='),
    writeln(Clause2),
    test_single_clause(Clause2, Algorithm).

test_single_clause((Head :- Body), Algorithm) :-
    writeln('Head:'), writeln(Head),
    writeln('Body:'), writeln(Body),
    
    % Test is_base_case
    writeln('Testing is_base_case...'),
    (   test_is_base_case(Head, Body) ->
        writeln('  -> IS base case')
    ;   writeln('  -> NOT base case')
    ),
    
    % Test has_recursion
    writeln('Testing has_recursion...'),
    (   test_has_recursion((Head :- Body), Algorithm) ->
        writeln('  -> HAS recursion')
    ;   writeln('  -> NO recursion')
    ),
    
    nl.

test_is_base_case(Head, Body) :-
    Head =.. [_|Args],
    writeln('Args:'), writeln(Args),
    (   member([], Args) ->
        writeln('  Found empty list'), !
    ;   member(0, Args) ->
        writeln('  Found zero'), !
    ;   Body = true ->
        writeln('  Body is true'), !
    ;   writeln('  None of base case conditions met'), fail
    ).

test_has_recursion((Head :- Body), Algorithm) :-
    functor(Head, Name, _),
    format('Predicate name: ~w~n', [Name]),
    writeln('Testing if body contains recursive call...'),
    test_contains_call_to(Body, Name).

test_contains_call_to((Goal, Rest), Name) :- !,
    format('Testing goal: ~w~n', [Goal]),
    (   functor(Goal, Name, _) ->
        format('  Found recursive call in: ~w~n', [Goal]), !
    ;   test_contains_call_to(Rest, Name)
    ).
test_contains_call_to(Goal, Name) :-
    format('Testing single goal: ~w~n', [Goal]),
    functor(Goal, GoalName, _),
    format('  Goal name: ~w, Target: ~w~n', [GoalName, Name]),
    GoalName = Name.