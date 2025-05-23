[neighbours, tests, checks].
%
% PASTE THIS TWO LINES IN SWIPL BEFORE
%
%use_module(library(clpfd)).
%set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes

count_cell(0,0).
count_cell(_,1).


% TO IMPLEMENT
% checkRowClues()
% checkColClues()
% nonTouching() % check diagonals ?? (what is this function supposed to do ?)
% countNeighbors() % don't check diagonals
% snakeConnected()


snake(RowClues, ColClues, Grid, Solution):- 
    copyGrid(Grid,Solution)
    ,maplist(label,Solution)
    %,extend_grid(Solution, Extended)
    ,checkRowClues(Solution,RowClues)
    ,checkColClues(Solution,ColClues)
%, nonTouching(Solution) % snake cannot touch itself
% countNeighbors(Extended) % heads have 1 neighbor, midpoints 2
%, snakeConnected(Solution) % snake must be connected
.


copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).

% constraint the value to be 0 or 2 when it is not 1 (the head/tail are given)
copyRow([-1|R],[Cell_Value|S]) :- 
                                Cell_Value in 0 \/ 2
                                ,copyRow(R,S)
                                , !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

count_cell(0,0).
count_cell(1,1).