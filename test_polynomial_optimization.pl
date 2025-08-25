% Test polynomial optimization and enhanced nnconverter functionality
% This tests the new features requested in the problem statement

:- use_module(neuronet_converter).

test_polynomial_optimization :-
    writeln('=== Testing Polynomial Optimization Features ==='),
    nl,
    
    % Test 1: Original nn_induction_optimisation
    test_nn_induction_optimisation,
    nl,
    
    % Test 2: Higher degree polynomial fitting
    test_higher_degree_polynomials,
    nl,
    
    % Test 3: Systematic coefficient discovery
    test_coefficient_discovery,
    nl,
    
    % Test 4: Cognitive code generation
    test_cognitive_code_generation,
    nl,
    
    % Test 5: Enhanced conversion with polynomials
    test_enhanced_conversion,
    nl,
    
    writeln('=== All Polynomial Optimization Tests Complete ===').

% Test 1: nn_induction_optimisation predicate as requested in problem statement
test_nn_induction_optimisation :-
    writeln('--- Testing nn_induction_optimisation ---'),
    writeln('Finding coefficients B,C such that B*n^2 + C*n = n(n+1)/2'),
    
    % Find solution
    (   nn_induction_optimisation(B, C) ->
        format('✓ Found solution: B=~w, C=~w~n', [B, C]),
        test_formula_validation(B, C)
    ;   writeln('✗ No solution found')
    ).

test_formula_validation(B, C) :-
    writeln('Validating formula against test cases:'),
    TestCases = [[1,1], [2,3], [3,6], [4,10], [5,15]],
    forall(member([N, Expected], TestCases),
           (Result is B*(N^2) + C*N,
            (   abs(Result - Expected) < 0.0001 ->
                format('  ✓ n=~w: ~w*~w^2 + ~w*~w = ~w (expected ~w)~n', 
                       [N, B, N, C, N, Result, Expected])
            ;   format('  ✗ n=~w: ~w*~w^2 + ~w*~w = ~w (expected ~w)~n', 
                       [N, B, N, C, N, Result, Expected])
            ))).

% Test 2: Higher degree polynomial fitting
test_higher_degree_polynomials :-
    writeln('--- Testing Higher Degree Polynomial Fitting ---'),
    
    % Test cubic polynomial: simple case n^3
    CubicData = [[1,1], [2,8], [3,27]],  % Simple n^3 pattern
    writeln('Testing cubic polynomial fitting (n^3):'),
    
    (   polynomial_fit(CubicData, 3, Coefficients) ->
        format('✓ Found cubic coefficients: ~w~n', [Coefficients]),
        test_polynomial_evaluation(CubicData, Coefficients)
    ;   writeln('✗ Could not fit cubic polynomial')
    ),
    
    % Test simpler quadratic: n^2
    QuadraticData = [[1,1], [2,4], [3,9]],  % Simple n^2 pattern  
    writeln('Testing quadratic polynomial fitting (n^2):'),
    
    (   polynomial_fit(QuadraticData, 2, QuadraticCoeffs) ->
        format('✓ Found quadratic coefficients: ~w~n', [QuadraticCoeffs]),
        test_polynomial_evaluation(QuadraticData, QuadraticCoeffs)
    ;   writeln('✗ Could not fit quadratic polynomial')
    ).

test_polynomial_evaluation(DataPoints, Coefficients) :-
    writeln('Validating polynomial evaluation:'),
    forall(member([X, Expected], DataPoints),
           (evaluate_polynomial(X, Coefficients, Result),
            Error is abs(Result - Expected),
            (   Error < 0.1 ->
                format('  ✓ f(~w) = ~w (expected ~w, error ~w)~n', 
                       [X, Result, Expected, Error])
            ;   format('  ✗ f(~w) = ~w (expected ~w, error ~w)~n', 
                       [X, Result, Expected, Error])
            ))).

% Test 3: Systematic coefficient discovery
test_coefficient_discovery :-
    writeln('--- Testing Systematic Coefficient Discovery ---'),
    
    % Test the extended coefficient generation
    generate_coefficient_candidates(Candidates),
    length(Candidates, NumCandidates),
    format('Generated ~w coefficient candidates~n', [NumCandidates]),
    
    % Show some examples
    writeln('Sample candidates:'),
    take_first_n(Candidates, 10, SampleCandidates),
    forall(member(C, SampleCandidates),
           format('  ~w~n', [C])),
    
    % Test systematic search on simple quadratic
    SimpleData = [[1,1], [2,4], [3,9], [4,16]],  % Perfect squares: n^2
    writeln('Testing systematic search for perfect squares (n^2):'),
    
    (   find_polynomial_coefficients(SimpleData, 2, [A, B, C], Error) ->
        format('✓ Found coefficients: [~w, ~w, ~w] with error ~w~n', [A, B, C, Error])
    ;   writeln('✗ Could not find coefficients')
    ).

% Test 4: Cognitive code generation
test_cognitive_code_generation :-
    writeln('--- Testing Cognitive Code Generation ---'),
    
    % Test with sum_list formula
    SumFormula = combined_formula(sum_list, 
                                 sum(empty_list) = 0, 
                                 sum(list_n) = sum(list_n_minus_1) + head_element),
    
    (   generate_cognitive_code(SumFormula, CognitiveCode) ->
        writeln('✓ Generated cognitive code for sum_list:'),
        format('  ~w~n', [CognitiveCode])
    ;   writeln('✗ Could not generate cognitive code')
    ),
    
    % Test with polynomial formula
    PolyFormula = polynomial_formula([[1,1], [2,4]], 2, [1, 0, 0]),
    
    (   generate_cognitive_code(PolyFormula, PolyCognitiveCode) ->
        writeln('✓ Generated cognitive code for polynomial:'),
        format('  ~w~n', [PolyCognitiveCode])
    ;   writeln('✗ Could not generate polynomial cognitive code')
    ).

% Test 5: Enhanced conversion integration
test_enhanced_conversion :-
    writeln('--- Testing Enhanced Conversion with Polynomials ---'),
    
    % Test with sum_list algorithm
    Algorithm = [
        (sum_list([], 0) :- true),
        (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1)
    ],
    
    writeln('Converting sum_list algorithm with polynomial enhancement:'),
    
    (   enhanced_convert_algorithm(Algorithm, EnhancedNeuronet) ->
        writeln('✓ Enhanced conversion successful'),
        
        % Check if polynomial fits were found
        EnhancedNeuronet = neuronet{polynomial_fits: PolyFits},
        (   PolyFits \= [] ->
            length(PolyFits, NumFits),
            format('  Found ~w polynomial fits~n', [NumFits]),
            forall(member(poly_fit(Degree, Coeffs, Error), PolyFits),
                   format('    Degree ~w: coeffs=~w, error=~w~n', [Degree, Coeffs, Error]))
        ;   writeln('  No polynomial fits found')
        ),
        
        % Check if cognitive codes were generated
        EnhancedNeuronet = neuronet{cognitive_codes: CogCodes},
        (   CogCodes \= [] ->
            length(CogCodes, NumCodes),
            format('  Generated ~w cognitive code structures~n', [NumCodes])
        ;   writeln('  No cognitive codes generated')
        )
        
    ;   writeln('✗ Enhanced conversion failed')
    ).

% Helper predicates
take_first_n(_, 0, []) :- !.
take_first_n([], _, []) :- !.
take_first_n([H|T], N, [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_first_n(T, N1, Rest).

% Entry point
demo_polynomial :-
    test_polynomial_optimization.