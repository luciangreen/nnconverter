% Simple test to check the clause parsing issue
:- use_module(library(debug)).

test_clause_parsing :-
    % Define the problematic clause  
    Clause = (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1),
    Clause = (Head :- Body),
    
    writeln('Head:'), writeln(Head),
    writeln('Body:'), writeln(Body),
    
    % Check the structure
    functor(Head, HeadName, HeadArity),
    format('Head functor: ~w/~w~n', [HeadName, HeadArity]),
    
    % Check what is_base_case sees
    Head =.. [_|Args],
    writeln('Head args:'), writeln(Args),
    
    % Check each arg
    member(Arg, Args),
    format('Arg: ~w, is list?: ~w, is empty?: ~w~n', [Arg, is_list(Arg), Arg == []]),
    fail.
test_clause_parsing.