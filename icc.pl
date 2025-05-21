% Useful helpers
before(X, Y, [X,Y|_]).
before(X, Y, [_|Rest]) :- before(X, Y, Rest).

after(X, Y, Tour) :- before(Y, X, Tour).

% Generate solution
go(Tour) :-
    Tour = [stand(tuesday, C1, N1, F1),
            stand(wednesday, C2, N2, F2),
            stand(thursday, C3, N3, F3),
            stand(friday, C4, N4, F4)],
    
    % All different values
    permutation([C1,C2,C3,C4], [twickel, westerflier, weldam, espelo]),
    permutation([N1,N2,N3,N4], [arend, marieke, jaco, marco]),
    permutation([F1,F2,F3,F4], [peppermint, coffee, peanut, chocolate]),

    % Clue 1
    \+ member(stand(_, twickel, arend, _), Tour),
    \+ member(stand(thursday, _, _, peppermint), Tour),

    % Clue 2
    member(stand(wednesday, _, _, coffee), Tour),
    \+ member(stand(wednesday, _, marieke, _), Tour),

    % Clue 3
    member(stand(DayJaco, _, jaco, peanut), Tour),
    DayJaco \= tuesday,

    % Clue 4
    member(stand(D1, westerflier, _, _), Tour),
    member(stand(D2, _, _, chocolate), Tour),
    member(stand(D3, _, marco, _), Tour),
    before(stand(D1, westerflier, _, _), stand(D2, _, _, chocolate), Tour),
    after(stand(D1, westerflier, _, _), stand(D3, _, marco, _), Tour),

    % Clue 5
    member(stand(D4, weldam, _, _), Tour),
    member(stand(D5, _, arend, _), Tour),
    before(stand(D4, weldam, _, _), stand(D5, _, arend, _), Tour),

    % Clue 6
    member(stand(_, espelo, _, _), Tour).
