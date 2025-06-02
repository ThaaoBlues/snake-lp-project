% LP PROJECT GROUP 29

:- use_module(library(clpfd)).
:- [tests].

%
% CHECKING CLUES PART
%

% base case
checkRowClues([], []).
% no clue, go to next cell
checkRowClues([_|Grid], [-1|Clues]) :- !, checkRowClues(Grid, Clues).
checkRowClues([Row|Grid], [Clue|Clues]) :- 
    countRow(Row, Count),
    Count #= Clue,
    checkRowClues(Grid, Clues).

% base case
countRow([], 0).
% increment when value is 1 or 2 (snake part)
countRow([Val|Row], Count) :-
    ((Val #= 1) #\/ (Val #= 2)),
    countRow(Row, C),
    Count is C + 1.
% move on when value is 0 (not 1 or 2)
countRow([Val|Row], Count) :-
    Val #= 0,
    countRow(Row, Count).

% base case
checkColClues([],[]).
checkColClues(Grid, Clues) :-
    transpose(Grid, Trans), % transpose grid, so we can use checkRowClues
    checkRowClues(Trans, Clues).

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
    count(Solution, Count),
    Length = Count, !.

% a cell is a head when the value is 1
head(cell(_,_,1)).

getHead(Grid, cell(X,Y,1)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, 1), !.

% get a cell with a given value, or get the value of a given cell
getValue(Grid, cell(X,Y,V)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, V), !.

% check if 2 cells are neighbors
neighboring(cell(Xc,Y,_), cell(Xn,Y,_)) :-
    Xn is Xc + 1;
    Xn is Xc - 1.
neighboring(cell(X,Yc,_), cell(X,Yn,_)) :-
    Yn is Yc + 1;
    Yn is Yc - 1.

% base case, go to the next snake part
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

% base case, count the amount of 1 and 2 in the grid
count([], 0).
count([Row|Grid], Count) :-
    count(Grid, C),
    rowCount(Row, Cr),
    Count is C + Cr.

% count the amount of 1 and 2 in a row
rowCount([], 0).
rowCount([1|Row], Count) :- !,
    rowCount(Row, C),
    Count is C + 1.
rowCount([2|Row], Count) :- !,
    rowCount(Row, C),
    Count is C + 1.
rowCount([_|Row], Count) :-
    rowCount(Row, Count).

%
% END OF SNAKE CONNECTED PART
%


%
% INPUT GRID SANITY CHECK
%

% make sure only 0, 1 and 2 are in the grid
goodGrid([]).
goodGrid([Row|Grid]) :-
    goodRow(Row),
    goodGrid(Grid).

% only 0, 1 and 2 in a row
goodRow([]).
goodRow([Cell|Row]) :- ((Cell #=0) #\/ (Cell #=1) #\/ (Cell #=2)), goodRow(Row).

% checks if there are 2 heads in the grid
checkHead(Grid) :- countHead(Grid, Count), Count = 2.

% count number of heads in given grid
countHead([],0).
countHead([Row|Grid], Count) :-
    countHead(Grid, C),
    countHeadRow(Row, Cr),
    Count is C + Cr,
    Count #=< 2.

% count number of 1 (head) in a row
countHeadRow([],0).
countHeadRow([Cell|Row], Count) :-
    Cell #= 1,
    countHeadRow(Row, C),
    Count is C + 1,
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

nonTouching([_]).
nonTouching([Row1, Row2 | Rows]) :-
    nonTouchingRows(Row1, Row2),
    nonTouching([Row2 | Rows]).

%base case
nonTouchingRows([_], [_]).
nonTouchingRows([A1, A2 | AList], [B1, B2 | BList]) :- 
    blockOf2([[A1,A2],[B1,B2]]), % no 4 block of 2s (technically induced by no diag ?)
    checkDiag([[A1,A2],[B1,B2]]), % no head/tail/body with something in diagonal, when they do not share a neighbor
    nonTouchingRows([A2 | AList], [B2 | BList]).

% constrained version, unlabeled variables
blockOf2([[A1,A2],[B1,B2]]) :- ((A1 #\= 2) #\/ (A2 #\= 2) #\/ (B1 #\= 2) #\/ (B2 #\= 2)).

% constrained version, unlabeled variables
checkDiag([[A1, A2], [B1, B2]]) :-
    % Diagonals: A1-B2 and A2-B1
    % Cells can be 0, 1 or 2, check diagonals of cells that are 1 or 2
    ((A1 #\= 0) #/\ (A2 #= 0) #/\ (B1 #= 0) #==> (B2 #= 0)),
    ((A2 #\= 0) #/\ (A1 #= 0) #/\ (B2 #= 0) #==> (B1 #= 0)), !.

%
% END OF SELF TOUCHING PART
%



%
% COUNTING NEIGHBOURS PART
%
extendGrid(OldGrid,NewGrid) :-
    transpose(OldGrid,TransGrid),
    extendGridRows(TransGrid,RowTransGrid),
    transpose(RowTransGrid,RowGrid),
    extendGridRows(RowGrid,NewGrid).

%base case
extendGridRows([Row], [NewGrid]) :-
    extendRow(Row, NewGrid). 
extendGridRows([Row1 | Rows], [NewRow1 | NewRows]) :-
    extendRow(Row1, NewRow1),
    extendGridRows(Rows, NewRows).

% Extend a row by adding a 0 at both ends
extendRow(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).

undoExtend(OldGrid, NewGrid) :-
    undoExtendRows(OldGrid, RowGrid),
    transpose(RowGrid, RowTransGrid),
    undoExtendRows(RowTransGrid, TransGrid),
    transpose(TransGrid, NewGrid).

%base case
undoExtendRows([Row], [NewGrid]) :-
    undoExtendRow(Row, NewGrid).
undoExtendRows([Row1 | Rows], [NewRow1 | NewRows]) :-
    undoExtendRow(Row1, NewRow1),
    undoExtendRows(Rows, NewRows).

undoExtendRow([0 | TailRow], NewRow) :- deleteLastElement(TailRow, NewRow). 

deleteLastElement(TailRow, NewRow) :- 
    reverse(TailRow, [0|ReverseRow]),
    reverse(ReverseRow, NewRow).

checkNeighborsPattern(0,_,_,_,_).
checkNeighborsPattern(Piece,N,E,S,W) :- 
    1 #=< Piece,
    countCell(N,X1),
    countCell(E,X2),
    countCell(S,X3),
    countCell(W,X4),
    Piece #= X1+X2+X3+X4.



% scan the grid rows 3 by 3 and check the neighbors of each middle case of row B
% then put the count variable in the actual grid case that was scanned ( so will mostly put 2,0 or 1)
% this do not check diagonals
% rowA  #?#
% rowB  ?*?
% rowC  #?#

%base
checkNeighborsRows([_,A2], [B1,B2], [_, C2]) :-
    checkNeighborsPattern(B2, A2, 0, C2, B1).
checkNeighborsRows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
    checkNeighborsPattern(M,N,E,S,W),
    checkNeighborsRows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

countNeighbors2([Row1, Row2, Row3]) :- checkNeighborsRows(Row1, Row2, Row3).
countNeighbors2([Row1, Row2, Row3 | Rows]) :-
    checkNeighborsRows(Row1, Row2, Row3),
    countNeighbors2([Row2, Row3 | Rows]).

countNeighbors(Solution) :-
    extendGrid(Solution, Extended),
    countNeighbors2(Extended),
    undoExtend(Extended, Solution).


%
% END OF COUNTING NEIGHBOURS PART
%


%
% MAIN PREDICATES
%
snake(RowClues, ColClues, Grid, Solution):-
    copyGrid(Grid, Solution),
    goodGrid(Solution),
    checkHead(Solution),
    checkRowClues(Solution, RowClues),
    checkColClues(Solution, ColClues),
    countNeighbors(Solution),
    nonTouching(Solution),
    snakeConnected(Solution).

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).

% constraint the values
copyRow([-1|R],[Cell_Value|S]) :- 
    Cell_Value in 0 \/ 2,
    copyRow(R,S), !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

countCell(Cell, Count) :-
    % Count is 1 if Cell is 1 or 2, else 0
    % <==> to mean if and only if
    B1 #<==> (Cell #= 1),
    B2 #<==> (Cell #= 2),
    Count #= B1 + B2.