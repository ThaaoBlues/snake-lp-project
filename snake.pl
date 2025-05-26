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

:- dynamic(solution_cache/2).

snake(_,_,G,S) :-solution_cache(G,S),write("Cache hit\n").

snake(RowClues, ColClues, Grid, Solution):-
    %write("copyGrid\n"),
    copyGrid(Grid, Solution),
    %write("checkRowClues\n"),
    checkRowClues(Solution, RowClues),
    %write("checkColClues\n"),
    checkColClues(Solution, ColClues),
    %write("nonTouching\n"),
    nonTouching(Solution),
    %write("countNeighbors\n"),
    countNeighbors(Solution),
    %write("snakeConnected\n"),
    snakeConnected(Solution),
    %write("labeling\n"),
    maplist(label, Solution),
    \+ solution_cache(Grid,Solution),
    assertz(solution_cache(Grid,Solution)),
    print_only_grid(Solution), nl
.


copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).

% constraint the values
copyRow([-1|R],[Cell_Value|S]) :- 
                                Cell_Value in 0 \/ 2
                                ,copyRow(R,S)
                                , !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

%count_cell(0,0).
%count_cell(1,1).
%count_cell(2,1).
count_cell(Cell, Count) :-
    % Count is 1 if Cell is 1 or 2, else 0
    % that's so fancy we have <==> to mean if and only if
    B1 #<==> (Cell #= 1),
    B2 #<==> (Cell #= 2),
    Count #= B1 + B2.
