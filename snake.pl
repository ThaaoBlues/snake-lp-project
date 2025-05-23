[neighbours, tests, checks].
%
% PASTE THIS TWO LINES IN SWIPL BEFORE
%
%use_module(library(clpfd)).
%set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes



% TO IMPLEMENT
% checkRowClues()
% checkColClues()
% nonTouching() % check diagonals ?? (what is this function supposed to do ?)
% countNeighbors() % don't check diagonals
% snakeConnected()


snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution)
%  , checkRowClues(Solution,RowClues)
%  , checkColClues(Solution,ColClues)
, nonTouching(Solution) % snake cannot touch itself
% , countNeighbors(Solution) % heads have 1 neighbor, midpoints 2
%, snakeConnected(Solution) % snake must be connected
.


copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).


copyRow([-1|R],[_|S]) :- copyRow(R,S), !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

count_cell(0,0).
count_cell(1,1).
count_cell(2,1).
count_piece_cell(0,0).
count_piece_cell(1,1).
count_piece_cell(2,2).