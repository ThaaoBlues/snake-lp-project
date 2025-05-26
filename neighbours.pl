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
    %write(Piece),
    %count_piece_cell(Piece, Max_Neighbours),
    Piece #= X1+X2+X3+X4.



% scan the grid rows 3 by 3 and check the neighbors of each middle case of row B
% then put the count variable in the actual grid case that was scanned ( so will mostly put 2,0 or 1)
% this do not check diagonals
% rowA  #?#
% rowB  ?*?
% rowC  #?#

%base
check_neighbors_rows([_,A2], [B1,B2], [_, C2]) :-
    check_neighbors_pattern(B2, A2, 0, C2, B1).

check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
    check_neighbors_pattern(M,N,E,S,W),
    check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

countNeighbors2([Row1, Row2, Row3]) :- check_neighbors_rows(Row1, Row2, Row3).

countNeighbors2([Row1, Row2, Row3 | Rows]) :-
    check_neighbors_rows(Row1, Row2, Row3),
    countNeighbors2([Row2, Row3 | Rows]).


countNeighbors(Solution) :-
    extend_grid(Solution, Extended)
    , countNeighbors2(Extended)
    , undo_extend(Extended, Solution)
    .