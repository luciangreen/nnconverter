% Test the member check issue
test_member_bug :-
    Args = [[H|T], S],
    writeln('Args:'), writeln(Args),
    (   member([], Args) ->
        writeln('Found empty list - BUG!')
    ;   writeln('No empty list found - correct')
    ),
    
    % More detailed check
    member(Arg, Args),
    format('Checking arg: ~w~n', [Arg]),
    (   Arg == [] ->
        format('  This is empty list: ~w~n', [Arg])
    ;   format('  This is not empty list: ~w~n', [Arg])
    ),
    fail.
test_member_bug.