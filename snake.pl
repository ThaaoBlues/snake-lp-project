[tests].
[neighbours].

%
% PASTE THIS TWO LINES IN SWIPL BEFORE
%
%use_module(library(clpfd)).
%set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes



%
% TO TEST
%


count_cell(0,0).
count_cell(_,1).


% base function used in snake predicate
checkRowClues([],[]).
checkRowClues([R|RS],[-1|CS]) :- checkColClues(RS,CS).
checkRowClues([R|RS],[C|CS]) :- count_parts_in_row(R,Count), 
                                Count is C,
                                checkColClues(RS,CS).

% actual stuff
count_parts_in_row([],0).
count_parts_in_row([X|XS],C+1) :- X > 0, count_parts_in_row(XS,C).
count_parts_in_row([_|XS],C) :- count_parts_in_row(XS,C).


% base function used in snake predicate

checkColClues(S,C) :- doesAllColCluesMatch(S,C,0).

% actual stuff
doesAllColCluesMatch(S,[],_).
doesAllColCluesMatch(S,[C|CS],Col_Index) :- count_parts_in_col(S,Col_Index,Count), 
                                            Count is C,
                                            colClues(S,CS,Col_Index+1).

% how are we supposed to sum on columns ????
% nevermind
count_parts_in_col([],_,0).

count_parts_in_col([R|RS],Col_Index,Count+1) :- nth0(R,Col_Index,Cell_Value),
                                                Cell_Value > 0,
                                                count_parts_in_col(RS,Col_Index,Count).
% kill myself

count_parts_in_col([R|RS],Col_Index,Count) :- count_parts_in_col(RS,Col_Index,Count).



%
% END OF SHI TO TEST
%

% TO IMPLEMENT
% checkRowClues()
% checkColClues()
% nonTouching() % check diagonals ?? (what is this function supposed to do ?)
% countNeighbors() % don't check diagonals
% snakeConnected()


snake(RowClues, ColClues, Grid, Solution)
:- copyGrid(Grid,Solution),
    extend_grid(Solution, Extended)
% , checkRowClues(Solution,RowClues)
% , checkColClues(Solution,ColClues)
%, nonTouching(Solution) % snake cannot touch itself
, countNeighbors(Extended) % heads have 1 neighbor, midpoints 2
%, snakeConnected(Solution) % snake must be connected
.


copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).


copyRow([-1|R],[_|S]) :- copyRow(R,S), !.
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).