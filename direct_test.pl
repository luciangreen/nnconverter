% Simple direct test
test_direct :-
    writeln('Direct test of sum_list clauses:'),
    
    Clause1 = (sum_list([], 0) :- true),
    Clause2 = (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1),
    
    writeln('Clause 1:'), writeln(Clause1),
    writeln('Clause 2:'), writeln(Clause2),
    
    % Test each clause manually
    Clause1 = (Head1 :- Body1),
    writeln('Head1:'), writeln(Head1),
    Head1 =.. [_|Args1],
    writeln('Args1:'), writeln(Args1),
    (   member([], Args1) -> writeln('Has empty list') ; writeln('No empty list') ),
    (   Body1 = true -> writeln('Body is true') ; writeln('Body not true') ),
    
    Clause2 = (Head2 :- Body2),
    writeln('Head2:'), writeln(Head2),
    Head2 =.. [_|Args2],
    writeln('Args2:'), writeln(Args2),
    (   member([], Args2) -> writeln('Has empty list') ; writeln('No empty list') ),
    (   Body2 = true -> writeln('Body is true') ; writeln('Body not true') ),
    
    writeln('Body2 structure:'), writeln(Body2).