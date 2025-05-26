copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]).
copyRow([-1|R],[_|S]) :- copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

snake(RowClues, ColClues, Grid, Solution)
    :- copyGrid(Grid,Solution)
    , goodGrid(Solution)
    , checkRowClues(Solution,RowClues)
    , checkColClues(Solution,ColClues)
    , checkHead(Solution)
    , nonTouching(Solution) % snake cannot touch itself
    , countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
    , snakeConnected(Solution) % snake must be connected
    .

goodGrid([]).
goodGrid([Row|Grid]) :-
    goodRow(Row),
    goodGrid(Grid).

goodRow([]).
goodRow([0|Row]) :- goodRow(Row).
goodRow([1|Row]) :- goodRow(Row).
goodRow([2|Row]) :- goodRow(Row).
goodRow([Cell|Row]) :- Cell \= (0;1;2), fail.

checkHead(Grid) :- countHead(Grid, Count), Count = 2.

countHead([],0).
countHead([Row|Grid], Count) :-
    countHead(Grid, C),
    countHeadRow(Row, Cr),
    Count is C + Cr,
    Count #=< 2.

countHeadRow([],0).
countHeadRow([1|Row], Count) :-
    countHeadRow(Row, C),
    Count is C + 1,
    Count #=< 2.
countHeadRow([Val|Row], Count) :-
    Val #\= 1,
    countHeadRow(Row, Count).

checkRowClues([], []).
checkRowClues([_|Grid], [-1|Clues]) :- !, checkRowClues(Grid, Clues).
checkRowClues([Row|Grid], [Clue|Clues]) :- 
    countRow(Row, Count),
    Count #= Clue,
    checkRowClues(Grid, Clues).

countRow([], 0).
countRow([Val|Row], Count) :-
    ((Val #= 1) #\/ (Val #= 2))
    countRow(Row, C),
    Count is C + 1.


countRow([Val|Row], Count) :-
    Val #= 0,
    countRow(Row, Count).

checkColClues([],[]).
checkColClues(Grid, Clues) :-
    transpose(Grid, Trans),
    checkRowClues(Trans, Clues).

% count_cell(0, 0).
% count_cell(Cell,Value) :- not(Cell = 0), Value = 1.

% check_neighbors_pattern(0,_,_,_,_).
% check_neighbors_pattern(Piece,N,E,S,W) :- 1 #=< Piece,
%     count_cell(N,X1),
%     count_cell(E,X2),
%     count_cell(S,X3),
%     count_cell(W,X4),
%     Piece #= X1+X2+X3+X4.

% check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
%     check_neighbors_pattern(M,N,E,S,W),
%     check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

snakeConnected(Solution) :-
    getHead(Solution, H),
    nextSnake(Solution, [], H, Path),
    length(Path, Length),
    Snake is Length,
    count(Solution, Count),
    Snake = Count, !.

head(cell(_,_,1)).

getHead(Grid, cell(X,Y,1)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, 1), !.

getValue(Grid, cell(X,Y,V)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, V), !.

neighboring(cell(Xc,Y,_), cell(Xn,Y,_)) :-
    Xn is Xc + 1;
    Xn is Xc - 1.

neighboring(cell(X,Yc,_), cell(X,Yn,_)) :-
    Yn is Yc + 1;
    Yn is Yc - 1.

nextSnake(Grid, _, Cell, [Cell]) :- 
    head(Cell), 
    getHead(Grid, H),
    not(Cell = H), !.
nextSnake(Grid, Visited, Current, [Current, cell(Xn,Yn,V)|Snake]) :-
    neighboring(Current, cell(Xn,Yn,V)),
    getValue(Grid, cell(Xn, Yn, V)),
    (V = 1 ; V = 2),
    not(member(cell(Xn,Yn,V), Visited)),
    nextSnake(Grid, [Current|Visited], cell(Xn,Yn,V), [cell(Xn,Yn,V)|Snake]).

count([], 0).
count([Row|Grid], Count) :-
    count(Grid, C),
    rowCount(Row, Cr),
    Count is C + Cr.

rowCount([], 0).
rowCount([1|Row], Count) :- !,
    rowCount(Row, C),
    Count is C + 1.
rowCount([2|Row], Count) :- !,
    rowCount(Row, C),
    Count is C + 1.
rowCount([_|Row], Count) :-
    rowCount(Row, Count).

count_cell(0,0).
count_cell(1,1).
count_cell(2,1).
count_piece_cell(0,0).
count_piece_cell(1,1).
count_piece_cell(2,2).