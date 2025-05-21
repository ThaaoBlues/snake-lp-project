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
checkRowClues([R|RS],[-1|CS]) :- checkRowClues(RS,CS).
checkRowClues([R|RS],[C|CS]) :- count_parts_in_row(R,Count), 
                                Count is C,
                                checkRowClues(RS,CS).

% actual stuff
count_parts_in_row([],0).
count_parts_in_row([X|XS],CP1) :- integer(X),
                                X > 0, % prevent _ values
                                count_parts_in_row(XS,C), 
                                CP1 is C+1.
count_parts_in_row([_|XS],C) :- count_parts_in_row(XS,C).


% base function used in snake predicate

checkColClues(S,C) :- doesAllColCluesMatch(S,C,0).

% actual stuff
doesAllColCluesMatch(S,[],_).
doesAllColCluesMatch(S,[C|CS],Col_Index) :- C > 0,
                                            count_parts_in_col(S,Col_Index,Count), 
                                            Count is C,
                                            Next_Col_Index is Col_Index + 1,
                                            doesAllColCluesMatch(S, CS, Next_Col_Index).
% in case of absence of clue (-1)
doesAllColCluesMatch(S,[-1|CS],Col_Index) :- Next_Col_Index is Col_Index + 1,
                                            doesAllColCluesMatch(S, CS, Next_Col_Index).

% how are we supposed to sum on columns ????
% nevermind
count_parts_in_col([],_,0).

count_parts_in_col([R|RS],Col_Index,CP1) :- nth0(Col_Index,R,Cell_Value),
                                                integer(Cell_Value), % prevent _ values
                                                Cell_Value > 0,
                                                count_parts_in_col(RS,Col_Index,Count),
                                                CP1 is Count+1.

count_parts_in_col([R|RS],Col_Index,Count) :- count_parts_in_col(RS,Col_Index,Count).


%
% END OF SHI TO TEST
%

copyGrid([],[]).
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).
copyRow([],[]).


copyRow([-1|R],[_|S]) :- copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).

%
test1(S) :- copyGrid([[ 1, 1],[-1,-1]],S),
            testRowRules([-1,-1],S), 
            testColRules([-1,-1],S), 
            print_only_grid(S).


test2(S) :- copyGrid([[-1,-1, 1],[-1,-1,-1],[ 1,-1,-1]],S),
            print_only_grid(S),
            nl,
            testRowRules([ 1,-1,-1],S), 
            testColRules([ 2,-1,-1],S), 
            print_only_grid(S).
testRowRules(Rowh,S) :- checkRowClues(S,Rowh).
testColRules(Colh,S) :- checkColClues(S,Colh).