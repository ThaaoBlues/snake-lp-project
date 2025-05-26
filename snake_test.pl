%
% CHECKING CLUES PART
%

checkRowClues([], []).
checkRowClues([_|Grid], [-1|Clues]) :- !, checkRowClues(Grid, Clues).
checkRowClues([Row|Grid], [Clue|Clues]) :- 
    countRow(Row, Count),
    Count #= Clue,
    checkRowClues(Grid, Clues).

countRow([], 0).
countRow([Val|Row], Count) :-
    countRow(Row, C),
    B #<==> (Val #= 1 #\/ Val #= 2),
    Count #= C + B.


countRow([Val|Row], Count) :-
    Val #= 0,
    countRow(Row, Count).

checkColClues([],[]).
checkColClues(Grid, Clues) :-
    transpose(Grid, Trans),
    checkRowClues(Trans, Clues).


% no hints
test1(S) :- copyGrid([[ 1, 1],[-1,-1]],S),
            % testRowRules([-1,-1],S), 
            % testColRules([-1,-1],S), 
            print_only_grid(S).


% must have 1 on row 1 of the grid and 2 (one or 2) on the first column, multiple solutions
test2(S) :- copyGrid([[-1,-1, 1],[-1,-1,-1],[ 1,-1,-1]],S),
             maplist(label,S), % force variable instanciation
             checkRowClues(S,[ 1,-1,-1]), 
             checkColClues(S,[ 2,-1,-1]),
            print_only_grid(S),
            nl.

%
% END OF CHECKING CLUES PART
%

%
% SNAKE CONNECTED PART
%

snakeConnected(Solution) :-
    getHead(Solution, H),
    nextSnake(Solution, [], H, Path),
    length(Path, Length),
    Snake #= Length,
    count(Solution, Count),
    Snake #= Count, !.

head(cell(_,_,1)).

getHead(Grid, cell(X,Y,1)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, 1), !.

getValue(Grid, cell(X,Y,V)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, V), !.

neighboring(cell(Xc,Y,_), cell(Xn,Y,_)) :-
    Xn #= Xc + 1;
    Xn #= Xc - 1.

neighboring(cell(X,Yc,_), cell(X,Yn,_)) :-
    Yn #= Yc + 1;
    Yn #= Yc - 1.

% nextSnake(Grid, _, Cell, [Cell]) :- 
%     head(Cell), 
%     getHead(Grid, H),
%     not(Cell = H), !.
% nextSnake(Grid, Visited, Current, [Current, cell(Xn,Yn,V)|Snake]) :-
%     neighboring(Current, cell(Xn,Yn,V)),
%     getValue(Grid, cell(Xn, Yn, V)),
%     (V = 1 ; V = 2),
%     not(member(cell(Xn,Yn,V), Visited)),
%     nextSnake(Grid, [Current|Visited], cell(Xn,Yn,V), [cell(Xn,Yn,V)|Snake]).

% Base case: no next neighbors to continue the snake
nextSnake(Grid, Visited, Current, [Current]) :-
    \+ (neighboring(Current, Next),
        getValue(Grid, Next),
        (Next = cell(_,_,1) ; Next = cell(_,_,2)),
        \+ member(Next, Visited)),
    !.

% Recursive case: follow snake neighbors
nextSnake(Grid, Visited, Current, [Current|Rest]) :-
    neighboring(Current, Next),
    getValue(Grid, Next),
    (Next = cell(_,_,1) ; Next = cell(_,_,2)),
    \+ member(Next, Visited),
    nextSnake(Grid, [Current|Visited], Next, Rest),
    !.



count([], 0).
count([Row|Grid], Count) :-
    count(Grid, C),
    rowCount(Row, Cr),
    Count #= C + Cr.

rowCount([], 0).
rowCount([X|Xs], Count) :-
    % Reify whether X is 1 or 2 into a boolean
    (X #= 1) #<==> B1,
    (X #= 2) #<==> B2,
    B #= B1 + B2,
    rowCount(Xs, RestCount),
    Count #= RestCount + B.

rowCount([_|Row], Count) :-
    rowCount(Row, Count).

%
% END OF SNAKE CONNECTED PART
%


%
% INPUT GRID SANITY CHECK
%

goodGrid([]).
goodGrid([Row|Grid]) :-
    goodRow(Row),
    goodGrid(Grid).

goodRow([]).
goodRow([Cell|Row]) :- ((Cell #=0) #\/ (Cell #=1) #\/ (Cell #=2)), goodRow(Row).
%goodRow([Cell|Row]) :- %((Cell #\=0) #/\ (Cell #\=1) #/\ (Cell #\=2)), fail.

checkHead(Grid) :- countHead(Grid, Count), Count #= 2.

countHead([],0).
countHead([Row|Grid], Count) :-
    countHead(Grid, C),
    countHeadRow(Row, Cr),
    Count #= C + Cr,
    Count #=< 2.

countHeadRow([],0).
countHeadRow([Cell|Row], Count) :-
    countHeadRow(Row, C),
    B #<==> (Cell #= 1),
    Count #= C + B,
    Count #=< 2.

    
countHeadRow([Val|Row], Count) :-
    Val #\= 1,
    countHeadRow(Row, Count).


%
% END OF INPUT GRID SANITY CHECK
%


%
% SELF TOUCHING PART
%

nonTouching([Row1, Row2]) :- nonTouching_rows(Row1, Row2), !.

nonTouching([Row1, Row2 | Rows]) :-
    nonTouching_rows(Row1, Row2),
    nonTouching([Row2 | Rows]).

%base case
nonTouching_rows(R1, R2) :- 
    %block_of_2([R1,R2]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([R1,R2]),!. % no head/tail with something in diagonal

nonTouching_rows([A1, A2 | AList], [B1, B2 | BList]) :- 
    block_of_2([[A1,A2],[B1,B2]]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([[A1,A2],[B1,B2]]), % no head/tail with something in diagonal
    diagonals_of_two([[A1,A2],[B1,B2]]),
    nonTouching_rows([A2 | AList], [B2 | BList]).



% constrained version, unlabeled variables
block_of_2([[A1,A2],[B1,B2]]) :- ((A1 #\= 2) #\/ (A2 #\= 2) #\/ (B1 #\= 2) #\/ (B2 #\= 2)).
    

diagonals_of_two([[A1,A2],[B1,B2]]) :- 
    ((A1 #= 2) #/\ (A2 #= 0) #/\ (B1 #= 0) #==> (B2 #= 0)),
    ((A2 #= 2) #/\ (A1 #= 0) #/\ (B2 #= 0) #==> (B1 #= 0)).


% constrained version, unlabeled variables
nothing_in_diag_of_1([[A1, A2], [B1, B2]]) :-
    % Diagonals: A1-B2 and A2-B1
    ((A1 #= 1) #/\ (A2 #= 0) #/\ (B1 #= 0) #==> (B2 #= 0)),
    ((A2 #= 1) #/\ (A1 #= 0) #/\ (B2 #= 0) #==> (B1 #= 0))
    ,!
    .

% accept the case where no 1 is on the 4x4 chunk
nothing_in_diag_of_1([[A1,A2],[B1,B2]]) :- A1 #\=1, A2 #\=1, B1 #\=1, B2 #\=1.

%
% END OF SELF TOUCHING PART
%



%
% COUNTING NEIGHBOURS PART
%

extend_grid(OldGrid,NewGrid) :-
    transpose(OldGrid,TransGrid),
    extend_grid_rows(TransGrid,RowTransGrid),
    transpose(RowTransGrid,RowGrid),
    extend_grid_rows(RowGrid,NewGrid).

%base case
extend_grid_rows([Row], [NewGrid]) :-
    extend_row(Row, NewGrid). 

extend_grid_rows([Row1 | Rows], [NewRow1 | NewRows]) :-
    extend_row(Row1, NewRow1),
    extend_grid_rows(Rows, NewRows).

% Extend a row by adding a 0 at both ends
extend_row(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).

undo_extend(OldGrid, NewGrid) :-
    undo_extend_rows(OldGrid, RowGrid),
    transpose(RowGrid, RowTransGrid),
    undo_extend_rows(RowTransGrid, TransGrid),
    transpose(TransGrid, NewGrid).

%base case
undo_extend_rows([Row], [NewGrid]) :-
    undo_extend_row(Row, NewGrid).

undo_extend_rows([Row1 | Rows], [NewRow1 | NewRows]) :-
    undo_extend_row(Row1, NewRow1),
    undo_extend_rows(Rows, NewRows).

undo_extend_row([0 | TailRow], NewRow) :- deleteLastElement(TailRow, NewRow). 

deleteLastElement(TailRow, NewRow) :- 
    reverse(TailRow, [0|ReverseRow]),
    reverse(ReverseRow, NewRow).

check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W) :- 
    1 #=< Piece,
    count_cell(N,X1),
    count_cell(E,X2),
    count_cell(S,X3),
    count_cell(W,X4),
    Piece #= X1+X2+X3+X4.



% scan the grid rows 3 by 3 and check the neighbors of each middle case of row B
% then put the count variable in the actual grid case that was scanned ( so will mostly put 2,0 or 1)
% this do not check diagonals
% rowA  #?#
% rowB  ?*?
% rowC  #?#

%base
check_neighbors_rows([_,A2], [B1,B2], [_, C2]) :-
    check_neighbors_pattern(B2, A2, 0, C2, B1), !.

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
    check_neighbors_pattern(M,N,E,S,W),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

countNeighbors2([Row1, Row2, Row3]) :- check_neighbors_rows(Row1, Row2, Row3),!.

countNeighbors2([Row1, Row2, Row3 | Rows]) :-
    check_neighbors_rows(Row1, Row2, Row3),
    countNeighbors2([Row2, Row3 | Rows]).


countNeighbors(Solution) :-
    extend_grid(Solution, Extended)
    , countNeighbors2(Extended)
    , undo_extend(Extended, Solution),!
    .

%
% END OF COUNTING NEIGHBOURS PART
%

:- dynamic(cache_solution/2).

snake(RowClues, ColClues, Grid, Solution):-
    %write("copyGrid\n"),
    copyGrid(Grid, Solution),
    goodGrid(Solution),
    checkHead(Solution),
    write("checkRowClues\n"),
    checkRowClues(Solution, RowClues),
    write("checkColClues\n"),
    checkColClues(Solution, ColClues),
    write("countNeighbors\n"),
    countNeighbors(Solution),
    write("snakeConnected\n"),
    snakeConnected(Solution),
    write("nonTouching\n"),
    nonTouching(Solution),
    \+ cache_solution(Grid,Solution),
    assertz(cache_solution(Grid,Solution))
.



copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).

% constraint the values
copyRow([-1|R],[Cell_Value|S]) :- 
                                Cell_Value in {0,2}
                                ,copyRow(R,S)
                                , !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).


count_cell(Cell, Count) :-
    % Count is 1 if Cell is 1 or 2, else 0
    % that's so fancy we have <==> to mean if and only if
    B1 #<==> (Cell #= 1),
    B2 #<==> (Cell #= 2),
    Count #= B1 + B2.
