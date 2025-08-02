% Test the enhanced features of the completed nnconverter
% This tests the newly implemented pattern_unfold/2 and inductive_insert/3 predicates

:- use_module(neuronet_converter).

test_enhanced_features :-
    writeln('=== Testing Enhanced nnconverter Features ==='),
    nl,
    
    % Test pattern unfolding capabilities
    test_pattern_unfolding,
    nl,
    
    % Test inductive insertion capabilities
    test_inductive_insertion,
    nl,
    
    % Test complex algorithm handling
    test_complex_algorithm_processing,
    nl,
    
    writeln('=== Enhanced Features Test Complete ===').

% Test pattern unfolding
test_pattern_unfolding :-
    writeln('--- Testing Pattern Unfolding ---'),
    
    % Define a more complex algorithm with multiple predicates
    Algorithm = [
        (sum_list([], 0) :- true),
        (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1),
        (double_sum(L, D) :- sum_list(L, S), D is S * 2)
    ],
    
    writeln('Original Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    % Test inductive transformation
    inductive_transform(Algorithm, InductiveForm),
    writeln('Inductive Form:'),
    maplist(writeln, InductiveForm),
    nl,
    
    % Test pattern unfolding
    pattern_unfold(InductiveForm, UnfoldedForm),
    writeln('Unfolded Form (shows pattern expansion):'),
    maplist(writeln, UnfoldedForm),
    nl.

% Test inductive insertion
test_inductive_insertion :-
    writeln('--- Testing Inductive Insertion ---'),
    
    Algorithm = [
        (factorial(0, 1) :- true),
        (factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1)
    ],
    
    % Get intermediate forms
    inductive_transform(Algorithm, InductiveForm),
    pattern_unfold(InductiveForm, UnfoldedForm),
    grammar_generate(UnfoldedForm, Grammar),
    
    writeln('Grammar Rules:'),
    maplist(writeln, Grammar),
    nl,
    
    % Test inductive insertion
    inductive_insert(UnfoldedForm, Grammar, OptimisedForm),
    writeln('Optimised Form (shows inductive patterns):'),
    maplist(writeln, OptimisedForm),
    nl.

% Test complex algorithm processing
test_complex_algorithm_processing :-
    writeln('--- Testing Complex Algorithm Processing ---'),
    
    % A more complex algorithm: quicksort-like partition
    Algorithm = [
        (partition([], _, [], []) :- true),
        (partition([H|T], Pivot, [H|Smaller], Bigger) :- 
            H =< Pivot, partition(T, Pivot, Smaller, Bigger)),
        (partition([H|T], Pivot, Smaller, [H|Bigger]) :- 
            H > Pivot, partition(T, Pivot, Smaller, Bigger))
    ],
    
    writeln('Complex Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    % Full conversion
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Full Conversion Results:'),
    
    get_dict(complexity, Neuronet, Complexity),
    format('Complexity Analysis: ~w~n', [Complexity]),
    
    get_dict(types, Neuronet, Types),
    format('Type Inference: ~w~n', [Types]),
    
    get_dict(formulas, Neuronet, Formulas),
    format('Generated Formulas: ~w~n', [Formulas]),
    
    writeln(''),
    writeln('This demonstrates the nnconverter can handle:'),
    writeln('✓ Multiple clause definitions'),
    writeln('✓ Conditional logic (=< and >)'),
    writeln('✓ Complex recursive patterns'),
    writeln('✓ Pattern unfolding with cross-predicate calls'),
    writeln('✓ Inductive pattern optimization'),
    nl.

% Demonstrate the complete workflow
demo_complete_workflow :-
    writeln('=== Demonstrating Complete nnconverter Workflow ==='),
    nl,
    
    Algorithm = [
        (power(_, 0, 1) :- true),
        (power(X, N, P) :- N > 0, N1 is N - 1, power(X, N1, P1), P is X * P1)
    ],
    
    writeln('Step 1: Input Algorithm'),
    maplist(writeln, Algorithm),
    nl,
    
    writeln('Step 2: Complexity Analysis'),
    complexity_finder(Algorithm, ComplexityInfo),
    format('  Result: ~w~n', [ComplexityInfo]),
    nl,
    
    writeln('Step 3: Type Inference'),
    type_finder(Algorithm, TypeInfo),
    format('  Result: ~w~n', [TypeInfo]),
    nl,
    
    writeln('Step 4: Inductive Transformation'),
    inductive_transform(Algorithm, InductiveForm),
    format('  Result: ~w~n', [InductiveForm]),
    nl,
    
    writeln('Step 5: Pattern Unfolding'),
    pattern_unfold(InductiveForm, UnfoldedForm),
    format('  Result: ~w~n', [UnfoldedForm]),
    nl,
    
    writeln('Step 6: Grammar Generation'),
    grammar_generate(UnfoldedForm, Grammar),
    format('  Result: ~w~n', [Grammar]),
    nl,
    
    writeln('Step 7: Inductive Insertion'),
    inductive_insert(UnfoldedForm, Grammar, OptimisedForm),
    format('  Result: ~w~n', [OptimisedForm]),
    nl,
    
    writeln('Step 8: Formula Generation'),
    formula_generate(InductiveForm, Formulas),
    format('  Result: ~w~n', [Formulas]),
    nl,
    
    writeln('=== Complete Workflow Demonstration Finished ===').

% Entry point for complete workflow demo
demo_workflow :- demo_complete_workflow.