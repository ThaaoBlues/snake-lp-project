[tests].

check_neighbors_pattern(0,_,_,_,_).
check_neighbors_pattern(Piece,N,E,S,W) :- 1 #=< Piece,
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
check_neighbors_rows([_,N,A3|RowA],[W,M,E|RowB],[_,S,C3|RowC]) :-
check_neighbors_pattern(M,N,E,S,W),
check_neighbors_rows([N,A3|RowA],[M,E|RowB],[S,C3|RowC]).

% base case
check_neighbors_rows([_,A2],[B1,B2],[_,C2]) :- check_neighbors_pattern(B2,A2,0,C2,B1).



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