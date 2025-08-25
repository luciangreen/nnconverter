% Quick test of just the core nn_induction_optimisation functionality
:- use_module(neuronet_converter).

test_core_functionality :-
    writeln('=== Testing Core nn_induction_optimisation ==='),
    
    % Test 1: Basic function
    writeln('Testing nn_induction_optimisation/2:'),
    (   nn_induction_optimisation(B, C) ->
        format('✓ Found coefficients: B=~w, C=~w~n', [B, C]),
        
        % Verify the formula works for n(n+1)/2
        TestCases = [1, 2, 3, 4, 5],
        writeln('Verification:'),
        forall(member(N, TestCases),
               (Formula is B*(N^2) + C*N,
                Expected is N*(N+1)//2,
                (   abs(Formula - Expected) < 0.001 ->
                    format('  ✓ n=~w: ~w (expected ~w)~n', [N, Formula, Expected])
                ;   format('  ✗ n=~w: ~w (expected ~w)~n', [N, Formula, Expected])
                )))
    ;   writeln('✗ No solution found')
    ),
    
    nl,
    
    % Test 2: Polynomial fitting
    writeln('Testing polynomial_fit/3:'),
    TestData = [[1,1], [2,4], [3,9]],  % n^2
    (   polynomial_fit(TestData, 2, Coefficients) ->
        format('✓ Fitted polynomial: ~w~n', [Coefficients]),
        
        % Test evaluation
        forall(member([X, Expected], TestData),
               (evaluate_polynomial(X, Coefficients, Result),
                (   abs(Result - Expected) < 0.001 ->
                    format('  ✓ f(~w) = ~w (expected ~w)~n', [X, Result, Expected])
                ;   format('  ✗ f(~w) = ~w (expected ~w)~n', [X, Result, Expected])
                )))
    ;   writeln('✗ Could not fit polynomial')
    ),
    
    nl,
    
    % Test 3: Coefficient generation
    writeln('Testing generate_coefficient_candidates/1:'),
    generate_coefficient_candidates(Candidates),
    length(Candidates, N),
    format('✓ Generated ~w candidates~n', [N]),
    
    nl,
    writeln('=== Core functionality tests complete ===').

% Entry point  
quick_test :- test_core_functionality.