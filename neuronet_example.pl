:- module(neuronet_example, [
    example_conversion/0,
    test_sum_list/0,
    test_reverse/0,
    run_all_examples/0
]).

:- use_module(neuronet_converter).

/** <module> Neuronet Example
 *
 *  Demonstrates Prolog Algorithm → Neuronet conversion with worked examples.
 *  
 *  This module provides concrete examples of how the neuronet converter
 *  transforms standard recursive predicates like sum_list/2 and reverse/2
 *  into neuronet-like representations.
 *
 *  Examples included:
 *  - sum_list/2: List summation with O(n) complexity
 *  - reverse/2: List reversal with O(n²) complexity (naive implementation)
 */

%% example_conversion/0
%  Runs sum_list/2 and reverse/2 through the neuronet converter pipeline.
%  This demonstrates the complete transformation process from algorithm
%  to neuronet representation.
example_conversion :-
    writeln('=== Prolog Algorithm → Manual Neuronet Converter ==='),
    nl,
    
    % Define example algorithms as list of clauses
    Algorithm = [
        % sum_list/2 - Sums all elements in a list
        (sum_list([], 0) :- true),
        (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1),

        % reverse/2 - Reverses a list (naive O(n²) implementation)
        (reverse([], []) :- true),
        (reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R)),
        
        % append/3 - Helper predicate for reverse
        (append([], L, L) :- true),
        (append([H|T], L, [H|R]) :- append(T, L, R))
    ],

    % Convert to neuronet form
    writeln('Converting algorithm to neuronet representation...'),
    convert_algorithm(Algorithm, Neuronet),
    nl,

    % Pretty-print each stage of the transformation
    writeln('--- COMPLEXITY ANALYSIS ---'),
    print_complexity_info(Neuronet.complexity),
    nl,

    writeln('--- TYPE INFERENCE ---'),
    print_type_info(Neuronet.types),
    nl,

    writeln('--- INDUCTIVE TRANSFORMATION ---'),
    print_inductive_form(Neuronet.inductive_form),
    nl,

    writeln('--- PATTERN UNFOLDING ---'),
    print_unfolded_form(Neuronet.unfolded_form),
    nl,

    writeln('--- GRAMMAR GENERATION ---'),
    print_grammar(Neuronet.grammar),
    nl,

    writeln('--- OPTIMIZED FORM ---'),
    print_optimised_form(Neuronet.optimised),
    nl,
    
    writeln('=== Conversion Complete ===').

%% test_sum_list/0
%  Tests the sum_list algorithm conversion specifically.
test_sum_list :-
    writeln('=== Testing sum_list/2 Conversion ==='),
    Algorithm = [
        (sum_list([], 0) :- true),
        (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1)
    ],
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Sum List Complexity:'),
    print_complexity_info(Neuronet.complexity),
    nl,
    
    writeln('Sum List Types:'),
    print_type_info(Neuronet.types),
    nl,
    
    writeln('Sum List Inductive Form:'),
    print_inductive_form(Neuronet.inductive_form),
    nl.

%% test_reverse/0  
%  Tests the reverse algorithm conversion specifically.
test_reverse :-
    writeln('=== Testing reverse/2 Conversion ==='),
    Algorithm = [
        (reverse([], []) :- true),
        (reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R)),
        (append([], L, L) :- true),
        (append([H|T], L, [H|R]) :- append(T, L, R))
    ],
    convert_algorithm(Algorithm, Neuronet),
    
    writeln('Reverse Complexity:'),
    print_complexity_info(Neuronet.complexity),
    nl,
    
    writeln('Reverse Types:'),
    print_type_info(Neuronet.types),
    nl,
    
    writeln('Reverse Inductive Form:'),
    print_inductive_form(Neuronet.inductive_form),
    nl.

%% run_all_examples/0
%  Runs all example conversions for comprehensive testing.
run_all_examples :-
    writeln('Running all neuronet conversion examples...'),
    nl,
    example_conversion,
    nl,
    test_sum_list,
    nl,
    test_reverse,
    nl,
    writeln('All examples completed successfully!').

% Pretty-printing helpers

%% print_complexity_info(+ComplexityInfo)
%  Pretty prints complexity analysis results.
print_complexity_info([]) :- !.
print_complexity_info([predicate(Name, Complexity)|Rest]) :-
    format('  ~w: ~w~n', [Name, Complexity]),
    print_complexity_info(Rest).

%% print_type_info(+TypeInfo)
%  Pretty prints type inference results.
print_type_info([]) :- !.
print_type_info([predicate(Name, Types)|Rest]) :-
    format('  ~w: ~w~n', [Name, Types]),
    print_type_info(Rest).

%% print_inductive_form(+InductiveForm)
%  Pretty prints inductive transformation results.
print_inductive_form([]) :- !.
print_inductive_form([base_case(Head)|Rest]) :-
    format('  Base Case: ~w~n', [Head]),
    print_inductive_form(Rest).
print_inductive_form([inductive_step(Head, Body)|Rest]) :-
    format('  Inductive Step: ~w :- ~w~n', [Head, Body]),
    print_inductive_form(Rest).

%% print_unfolded_form(+UnfoldedForm)
%  Pretty prints pattern unfolding results.
print_unfolded_form(Form) :-
    format('  ~w~n', [Form]).

%% print_grammar(+Grammar)
%  Pretty prints generated grammar rules.
print_grammar([]) :- !.
print_grammar([Rule|Rest]) :-
    format('  ~w~n', [Rule]),
    print_grammar(Rest).

%% print_optimised_form(+OptimisedForm)
%  Pretty prints final optimized form.
print_optimised_form(Form) :-
    format('  ~w~n', [Form]).

/* ===== Expected Output Structure =====

=== Prolog Algorithm → Manual Neuronet Converter ===

Converting algorithm to neuronet representation...

--- COMPLEXITY ANALYSIS ---
  sum_list: o_n
  reverse: o_n
  append: o_n

--- TYPE INFERENCE ---
  sum_list: [list(var), var]
  reverse: [list(var), var]
  append: [var, var, var]

--- INDUCTIVE TRANSFORMATION ---
  Base Case: sum_list([], 0)
  Inductive Step: sum_list([H|T], S) :- (sum_list(T, S1), S is H + S1)
  Base Case: reverse([], [])
  Inductive Step: reverse([H|T], R) :- (reverse(T, RT), append(RT, [H], R))
  Base Case: append([], L, L)
  Inductive Step: append([H|T], L, [H|R]) :- append(T, L, R)

--- PATTERN UNFOLDING ---
  [base_case(...), inductive_step(...), ...]

--- GRAMMAR GENERATION ---
  [list -> []]
  [list -> [element | list]]
  [sum_list_empty -> []]
  [sum_list_step -> ...]

--- OPTIMIZED FORM ---
  [optimized inductive patterns]

=== Conversion Complete ===

============================================================ */