<<<<<<< HEAD
[tests].
[neighbours].
[touching].

=======
[neighbours, tests, checks].
>>>>>>> ab5540ce5c27b15e34eb562986474ea7eaf02b81
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
:- copyGrid(Grid,Solution),
    extend_grid(Solution, Extended)
<<<<<<< HEAD
% , checkRowClues(Solution,RowClues)
% , checkColClues(Solution,ColClues)
, nonTouching(Solution) % snake cannot touch itself
% , countNeighbors(Extended) % heads have 1 neighbor, midpoints 2
=======
 , checkRowClues(Solution,RowClues)
 , checkColClues(Solution,ColClues)
%, nonTouching(Solution) % snake cannot touch itself
, countNeighbors(Extended) % heads have 1 neighbor, midpoints 2
>>>>>>> ab5540ce5c27b15e34eb562986474ea7eaf02b81
%, snakeConnected(Solution) % snake must be connected
.


copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).


copyRow([-1|R],[_|S]) :- copyRow(R,S), !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).