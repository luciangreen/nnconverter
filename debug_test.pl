% Debug test for recursion detection
:- include('neuronet_converter.pl').

debug_test :-
    Clause = (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1),
    Algorithm = [(sum_list([], 0) :- true), Clause],
    
    writeln('Testing clause:'),
    writeln(Clause),
    
    writeln('Testing has_recursion:'),
    (   has_recursion(Clause, Algorithm) ->
        writeln('  -> Has recursion: YES')
    ;   writeln('  -> Has recursion: NO')
    ),
    
    writeln('Testing is_base_case:'),
    Clause = (Head :- Body),
    (   is_base_case(Head, Body) ->
        writeln('  -> Is base case: YES')
    ;   writeln('  -> Is base case: NO')
    ),
    
    writeln('Classification:'),
    classify_clause(Clause, Algorithm, Result),
    writeln(Result).