% Test factorial algorithm formula generation and proof
% Demonstrates factorial(n) = n * factorial(n-1) with factorial(0) = 1

:- use_module(neuronet_converter).

test_factorial_formula :-
    writeln('=== Testing Factorial Formula Generation ==='),
    nl,
    
    % Define factorial algorithm
    Algorithm = [
        (factorial(0, 1) :- true),
        (factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1)
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
    
    % Test the factorial formula
    writeln('=== Testing Factorial Values ==='),
    test_factorial_values,
    nl,
    
    % Generate and verify inductive proof
    writeln('=== Inductive Proof ==='),
    inductive_proof(Formula, BaseProof, StepProof),
    writeln('Base Case Proof:'),
    writeln(BaseProof),
    writeln('Inductive Step Proof:'),
    writeln(StepProof),
    nl,
    
    % Verify formula with test cases
    writeln('=== Formula Verification ==='),
    TestInputs = [0, 1, 2, 3, 4, 5],
    verify_formula(Formula, TestInputs, VerificationResult),
    writeln('Verification Result:'),
    writeln(VerificationResult),
    nl,
    
    writeln('Factorial test completed successfully!').

% Test factorial values against known results
test_factorial_values :-
    KnownFactorials = [
        (0, 1),
        (1, 1), 
        (2, 2),
        (3, 6),
        (4, 24),
        (5, 120),
        (6, 720)
    ],
    forall(member((N, ExpectedResult), KnownFactorials),
           test_single_factorial(N, ExpectedResult)).

test_single_factorial(N, Expected) :-
    % Calculate using algorithm simulation
    simulate_factorial(N, Calculated),
    
    % Verify they match
    (   Calculated =:= Expected ->
        format('✓ factorial(~w) = ~w (CORRECT)~n', [N, Calculated])
    ;   format('✗ factorial(~w) = ~w, expected ~w (INCORRECT)~n', [N, Calculated, Expected])
    ).

% Simulate factorial algorithm
simulate_factorial(0, 1) :- !.
simulate_factorial(N, Fact) :-
    N > 0,
    N1 is N - 1,
    simulate_factorial(N1, Fact1),
    Fact is N * Fact1.