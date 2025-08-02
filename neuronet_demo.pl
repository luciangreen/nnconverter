% Comprehensive demonstration of the Prolog Algorithm → Manual Neuronet Converter
% This file shows the complete workflow from algorithm specification to neuronet representation

:- use_module(neuronet_converter).

% Main demonstration predicate
main_demo :-
    writeln(''),
    writeln('==========================================================='),
    writeln('  PROLOG ALGORITHM → MANUAL NEURONET CONVERTER'),
    writeln('==========================================================='),
    writeln(''),
    writeln('This system transforms Prolog algorithms into neuronet-style'),
    writeln('architectures by detecting recursive structures, applying'),
    writeln('inductive transformations, and generating grammars.'),
    writeln(''),
    
    % Example 1: Sum List Algorithm
    demo_sum_list,
    
    % Example 2: Factorial Algorithm
    demo_factorial,
    
    % Example 3: List Append Algorithm
    demo_append,
    
    writeln(''),
    writeln('==========================================================='),
    writeln('  DEMONSTRATION COMPLETE'),
    writeln('==========================================================='),
    writeln(''),
    writeln('The Prolog Algorithm → Manual Neuronet Converter successfully'),
    writeln('transforms recursive algorithms into structured representations'),
    writeln('suitable for:'),
    writeln('  • Teaching recursion and induction in AI pedagogy'),
    writeln('  • Algorithm analysis and simplification'),
    writeln('  • Code-to-logic-based neuronet design'),
    writeln('').

% Demonstration 1: Sum List Algorithm
demo_sum_list :-
    writeln('-----------------------------------------------------------'),
    writeln('EXAMPLE 1: Sum List Algorithm'),
    writeln('-----------------------------------------------------------'),
    writeln(''),
    writeln('Original Prolog Algorithm:'),
    writeln('  sum_list([], 0).'),
    writeln('  sum_list([H|T], S) :- sum_list(T, S1), S is H + S1.'),
    writeln(''),
    
    Algorithm = [
        (sum_list([], 0) :- true),
        (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1)
    ],
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    writeln(''),
    writeln('• Complexity Analysis:'),
    print_complexity_results(Neuronet.complexity),
    writeln(''),
    writeln('• Type Inference:'),
    print_type_results(Neuronet.types),
    writeln(''),
    writeln('• Inductive Form:'),
    print_inductive_results(Neuronet.inductive_form),
    writeln(''),
    writeln('• Generated Grammar:'),
    print_grammar_results(Neuronet.grammar),
    writeln('').

% Demonstration 2: Factorial Algorithm  
demo_factorial :-
    writeln('-----------------------------------------------------------'),
    writeln('EXAMPLE 2: Factorial Algorithm'),
    writeln('-----------------------------------------------------------'),
    writeln(''),
    writeln('Original Prolog Algorithm:'),
    writeln('  factorial(0, 1).'),
    writeln('  factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.'),
    writeln(''),
    
    Algorithm = [
        (factorial(0, 1) :- true),
        (factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1)
    ],
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    writeln(''),
    writeln('• Complexity Analysis:'),
    print_complexity_results(Neuronet.complexity),
    writeln(''),
    writeln('• Type Inference:'),
    print_type_results(Neuronet.types),
    writeln('').

% Demonstration 3: List Append Algorithm
demo_append :-
    writeln('-----------------------------------------------------------'),
    writeln('EXAMPLE 3: List Append Algorithm'),
    writeln('-----------------------------------------------------------'),
    writeln(''),
    writeln('Original Prolog Algorithm:'),
    writeln('  append([], L, L).'),
    writeln('  append([H|T], L, [H|R]) :- append(T, L, R).'),
    writeln(''),
    
    Algorithm = [
        (append([], L, L) :- true),
        (append([H|T], L, [H|R]) :- append(T, L, R))
    ],
    
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Conversion Results:'),
    writeln(''),
    writeln('• Complexity Analysis:'),
    print_complexity_results(Neuronet.complexity),
    writeln(''),
    writeln('• Type Inference:'),
    print_type_results(Neuronet.types),
    writeln('').

% Helper predicates for pretty printing
print_complexity_results([]) :- !.
print_complexity_results([predicate(Name, Complexity)|Rest]) :-
    format('    ~w: ~w~n', [Name, Complexity]),
    print_complexity_results(Rest).

print_type_results([]) :- !.
print_type_results([predicate(Name, Types)|Rest]) :-
    format('    ~w: ~w~n', [Name, Types]),
    print_type_results(Rest).

print_inductive_results([]) :- !.
print_inductive_results([base_case(Head)|Rest]) :-
    format('    Base Case: ~w~n', [Head]),
    print_inductive_results(Rest).
print_inductive_results([inductive_step(Head, Body)|Rest]) :-
    format('    Inductive Step: ~w :- ~w~n', [Head, Body]),
    print_inductive_results(Rest).

print_grammar_results([]) :- !.
print_grammar_results([Rule|Rest]) :-
    format('    ~w~n', [Rule]),
    print_grammar_results(Rest).

% Entry point for demonstration
demo :- main_demo.