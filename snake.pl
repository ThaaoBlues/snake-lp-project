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


snake(RowClues, ColClues, Grid, Solution):- 
    copyGrid(Grid,Solution)
    % ,maplist(label,Solution) % force variable instanciation
    % ,checkRowClues(Solution,RowClues)
    % ,checkColClues(Solution,ColClues)
    % ,print_only_grid(Solution)
    % ,nl
    %,! % no backtrack for easy testing (remove later)
    ,nonTouching(Solution) % snake cannot touch itself in diagonal
    % ,countNeighbors(Solution) % heads have 1 neighbor, midpoints 2 ( => no touch everywhere else than diagonal)
    %,not_more_than_2_ends(Solution)
    %, snakeConnected(Solution) % snake must be connected
.


% the following predicate was for testing purposes, might be still useful sometimes

% Predicate to check if 1 appears no more than twice in the entire grid
not_more_than_2_ends(Solution) :-
    count_total_ones(ListOfLists, TotalCount),
    TotalCount #=< 2.

% Helper predicate to count the total number of 1s in the entire grid
count_total_ones([], 0).
count_total_ones([Sublist | Rest], TotalCount) :-
    count_ones(Sublist, Count),
    count_total_ones(Rest, RestTotalCount),
    TotalCount #= Count + RestTotalCount.

% Helper predicate to count the number of 1s in a list
count_ones([], 0).
count_ones([1 | Rest], Count) :-
    count_ones(Rest, RestCount),
    Count #= RestCount + 1.
count_ones([_ | Rest], Count) :-
    count_ones(Rest, Count).


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
count_cell(2,1).
count_piece_cell(0,0).
count_piece_cell(1,1).
count_piece_cell(2,2).