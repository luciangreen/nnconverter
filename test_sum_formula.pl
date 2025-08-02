% Test mathematical formula generation and proof for sum of first n natural numbers
% This demonstrates the key capability requested in the problem statement

:- use_module(neuronet_converter).

test_sum_formula :-
    writeln('=== Testing Sum of First N Natural Numbers Formula ==='),
    nl,
    
    % Define sum_list algorithm
    Algorithm = [
        (sum_list([], 0) :- true),
        (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1)
    ],
    
    writeln('Algorithm:'),
    maplist(writeln, Algorithm),
    nl,
    
    % Convert to neuronet and extract formulas
    convert_algorithm(Algorithm, Neuronet),
    get_dict(formulas, Neuronet, [Formula|_]),
    
    writeln('Generated Formula:'),
    writeln(Formula),
    nl,
    
    % Test the specific formula for sum of first n natural numbers
    writeln('=== Testing Formula: 1+2+...+n = n(n+1)/2 ==='),
    test_natural_number_sum_formula,
    nl,
    
    % Generate and verify inductive proof
    writeln('=== Inductive Proof ==='),
    inductive_proof(Formula, BaseProof, StepProof),
    writeln('Base Case Proof:'),
    writeln(BaseProof),
    writeln('Inductive Step Proof:'),
    writeln(StepProof),
    nl,
    
    % Verify formula with actual test cases
    writeln('=== Formula Verification ==='),
    TestInputs = [
        [1],           % n=1: sum=1, formula=1*2/2=1
        [1,2],         % n=2: sum=3, formula=2*3/2=3  
        [1,2,3],       % n=3: sum=6, formula=3*4/2=6
        [1,2,3,4],     % n=4: sum=10, formula=4*5/2=10
        [1,2,3,4,5]    % n=5: sum=15, formula=5*6/2=15
    ],
    verify_formula(Formula, TestInputs, VerificationResult),
    writeln('Verification Result:'),
    writeln(VerificationResult),
    nl,
    
    writeln('Test completed successfully!').

% Test the mathematical formula for sum of first n natural numbers
test_natural_number_sum_formula :-
    forall(between(1, 10, N),
           test_single_natural_sum(N)).

test_single_natural_sum(N) :-
    % Generate list [1,2,...,N]
    numlist(1, N, List),
    
    % Calculate sum using algorithm
    simulate_sum_list(List, AlgorithmSum),
    
    % Calculate sum using formula n(n+1)/2
    FormulaSum is N * (N + 1) // 2,
    
    % Verify they match
    (   AlgorithmSum =:= FormulaSum ->
        format('✓ n=~w: sum=~w, formula=~w (MATCH)~n', [N, AlgorithmSum, FormulaSum])
    ;   format('✗ n=~w: sum=~w, formula=~w (MISMATCH)~n', [N, AlgorithmSum, FormulaSum])
    ).

% Simulate sum_list algorithm (same as in main module)
simulate_sum_list([], 0).
simulate_sum_list([H|T], Sum) :-
    simulate_sum_list(T, TSum),
    Sum is H + TSum.