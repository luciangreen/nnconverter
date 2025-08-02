% Test additional algorithms to demonstrate enhanced nnconverter functionality
% This tests the newly implemented pattern_unfold and inductive_insert capabilities

:- use_module(neuronet_converter).

test_additional_algorithms :-
    writeln('=== Testing Additional Algorithms ==='),
    nl,
    
    % Test 1: List length algorithm
    test_list_length,
    nl,
    
    % Test 2: List append algorithm  
    test_list_append,
    nl,
    
    % Test 3: List reverse algorithm
    test_list_reverse,
    nl,
    
    % Test 4: Fibonacci sequence
    test_fibonacci,
    nl,
    
    writeln('=== All Additional Algorithm Tests Complete ===').

% Test 1: List Length Algorithm
test_list_length :-
    writeln('--- Testing List Length Algorithm ---'),
    
    Algorithm = [
        (list_length([], 0) :- true),
        (list_length([_|T], L) :- list_length(T, L1), L is L1 + 1)
    ],
    
    writeln('Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    get_dict(complexity, Neuronet, Complexity),
    format('Complexity: ~w~n', [Complexity]),
    
    get_dict(types, Neuronet, Types),
    format('Types: ~w~n', [Types]),
    
    get_dict(formulas, Neuronet, Formulas),
    format('Formulas: ~w~n', [Formulas]),
    
    % Test the pattern unfolding
    get_dict(unfolded_form, Neuronet, UnfoldedForm),
    format('Unfolded Form: ~w~n', [UnfoldedForm]),
    
    % Test the optimized form
    get_dict(optimised, Neuronet, OptimisedForm),
    format('Optimised Form: ~w~n', [OptimisedForm]).

% Test 2: List Append Algorithm
test_list_append :-
    writeln('--- Testing List Append Algorithm ---'),
    
    Algorithm = [
        (append([], L, L) :- true),
        (append([H|T], L, [H|R]) :- append(T, L, R))
    ],
    
    writeln('Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    get_dict(formulas, Neuronet, Formulas),
    format('Formulas: ~w~n', [Formulas]),
    
    % Verify with test cases
    test_append_examples.

% Test 3: List Reverse Algorithm  
test_list_reverse :-
    writeln('--- Testing List Reverse Algorithm ---'),
    
    Algorithm = [
        (reverse([], []) :- true),
        (reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R))
    ],
    
    writeln('Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    get_dict(complexity, Neuronet, Complexity),
    format('Complexity: ~w~n', [Complexity]),
    
    get_dict(formulas, Neuronet, Formulas),
    format('Formulas: ~w~n', [Formulas]).

% Test 4: Fibonacci Sequence
test_fibonacci :-
    writeln('--- Testing Fibonacci Algorithm ---'),
    
    Algorithm = [
        (fib(0, 0) :- true),
        (fib(1, 1) :- true),
        (fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2)
    ],
    
    writeln('Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    get_dict(complexity, Neuronet, Complexity),
    format('Complexity: ~w~n', [Complexity]),
    
    get_dict(formulas, Neuronet, Formulas),
    format('Formulas: ~w~n', [Formulas]),
    
    % Test the enhanced pattern unfolding for multiple recursive calls
    get_dict(unfolded_form, Neuronet, UnfoldedForm),
    format('Unfolded Form (showing multiple recursion): ~w~n', [UnfoldedForm]).

% Helper: Test append examples
test_append_examples :-
    TestCases = [
        ([], [1,2,3], [1,2,3]),
        ([1], [2,3], [1,2,3]),
        ([1,2], [3,4], [1,2,3,4])
    ],
    writeln('Append test cases:'),
    forall(member((L1, L2, Expected), TestCases),
           test_single_append(L1, L2, Expected)).

test_single_append(L1, L2, Expected) :-
    simulate_append(L1, L2, Result),
    (   Result = Expected ->
        format('  ✓ append(~w, ~w) = ~w~n', [L1, L2, Result])
    ;   format('  ✗ append(~w, ~w) = ~w, expected ~w~n', [L1, L2, Result, Expected])
    ).

% Helper: Simulate append algorithm
simulate_append([], L, L).
simulate_append([H|T], L, [H|R]) :-
    simulate_append(T, L, R).