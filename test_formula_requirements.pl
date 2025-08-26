% Test specific requirements from the problem statement
% Tests the formula "0.5n^2+0.5*n+0*n" and cognitive code with pattern matching

:- use_module(neuronet_converter).

test_formula_requirements :-
    writeln('=== Testing Problem Statement Requirements ==='),
    nl,
    
    % Test 1: Verify the explicit formula format
    writeln('1. Testing formula format "0.5*n^2+0.5*n+0*n":'),
    test_explicit_formula_format,
    nl,
    
    % Test 2: Verify cognitive code contains pattern matching operations  
    writeln('2. Testing cognitive code with pattern matching (append, string_concat, if-then):'),
    test_cognitive_code_pattern_matching,
    nl,
    
    % Test 3: Verify n characters in compressed inductive formulas
    writeln('3. Testing n characters in compressed inductive formulas:'),
    test_n_characters_in_formulas,
    nl,
    
    writeln('=== All requirements tests complete ===').

% Test the explicit formula format
test_explicit_formula_format :-
    % Test the nn_induction_optimisation with the exact formula format
    (   nn_induction_optimisation(0.5, 0.5) ->
        writeln('✓ Formula 0.5*n^2 + 0.5*n + 0*n verified'),
        % Test specific values to confirm
        TestValues = [1, 2, 3, 4],
        maplist(test_formula_value, TestValues)
    ;   writeln('✗ Formula verification failed')
    ).

test_formula_value(N) :-
    % Using the explicit formula: 0.5*n^2 + 0.5*n + 0*n
    Result is 0.5*(N^2) + 0.5*N + 0*N,
    Expected is N*(N+1)//2,
    (   abs(Result - Expected) < 0.001 ->
        format('  ✓ n=~w: 0.5*~w^2 + 0.5*~w + 0*~w = ~w (expected ~w)~n', 
               [N, N, N, N, Result, Expected])
    ;   format('  ✗ n=~w: formula mismatch~n', [N])
    ).

% Test cognitive code contains pattern matching operations
test_cognitive_code_pattern_matching :-
    % Create a sample formula
    Formula = combined_formula(sum_list, 
                              sum(empty_list) = 0,
                              sum(list_n) = sum(list_n_minus_1) + head_element),
    
    % Generate cognitive code
    (   generate_cognitive_code(Formula, CognitiveCode) ->
        writeln('✓ Cognitive code generated successfully'),
        % Check for pattern matching operations
        check_pattern_matching_operations(CognitiveCode)
    ;   writeln('✗ Cognitive code generation failed')
    ).

check_pattern_matching_operations(cognitive_predicate(_, Components)) :-
    % Look for pattern matching operations in the components
    (   member(pattern_matching_operations(Ops), Components) ->
        writeln('✓ Pattern matching operations found:'),
        check_specific_operations(Ops)
    ;   writeln('✗ No pattern matching operations found')
    ).

check_specific_operations(Ops) :-
    % Check for append operation
    (   member(append([], _, _), Ops) ->
        writeln('  ✓ append operation present')
    ;   writeln('  ✗ append operation missing')
    ),
    % Check for if_then operation
    (   member(if_then(_, _, _), Ops) ->
        writeln('  ✓ if_then operation present')
    ;   writeln('  ✗ if_then operation missing')
    ),
    % Check for string_concat operation
    (   member(string_concat(_, _, _), Ops) ->
        writeln('  ✓ string_concat operation present')
    ;   writeln('  ✗ string_concat operation missing')
    ).

% Test n characters in compressed inductive formulas
test_n_characters_in_formulas :-
    % Create a sample formula
    Formula = combined_formula(sum_list, 
                              sum(empty_list) = 0,
                              sum(list_n) = sum(list_n_minus_1) + head_element),
    
    % Generate cognitive code
    (   generate_cognitive_code(Formula, CognitiveCode) ->
        writeln('✓ Cognitive code with compressed inductive formulas generated'),
        % Check for n characters in formulas
        check_n_characters_in_formulas(CognitiveCode)
    ;   writeln('✗ Cognitive code generation failed')
    ).

check_n_characters_in_formulas(cognitive_predicate(_, Components)) :-
    % Look for compressed inductive formula with n characters
    (   member(compressed_inductive_formula(FormulaInN), Components) ->
        writeln('✓ Compressed inductive formula found:'),
        format('  Formula with n: ~w~n', [FormulaInN]),
        % Check if it contains the explicit n^2 formula
        check_explicit_n_formula(FormulaInN)
    ;   writeln('✗ No compressed inductive formula found')
    ).

check_explicit_n_formula(formula_in_n_terms(_, _, '0.5*n^2 + 0.5*n + 0*n')) :-
    writeln('  ✓ Explicit formula "0.5*n^2 + 0.5*n + 0*n" found in compressed form'),
    !.
check_explicit_n_formula(FormulaInN) :-
    format('  ⚠ Formula found but not in expected format: ~w~n', [FormulaInN]).

% Entry point
main :- test_formula_requirements.