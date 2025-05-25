% base case

nonTouching([Row1, Row2]) :- nonTouching_rows(Row1, Row2).

nonTouching([Row1, Row2, Row3 | Rows]) :-
    nonTouching_rows(Row1, Row2),
    nonTouching([Row2, Row3 | Rows]).

%base case
nonTouching_rows([A1, A2], [B1, B2]) :- 
    \+ diag_2_grid([[A1,A2],[B1,B2]]), % makes sure we have no diagonals constitued of 2s
    \+ block_of_2([[A1,A2],[B1,B2]]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([[A1,A2],[B1,B2]]). % no head/tail with something in diagonal

nonTouching_rows([A1, A2, A3 | AList], [B1, B2, B3 | BList]) :- 
    %valid_4_grid([[A1, A2], [B1, B2]]),
    \+ diag_2_grid([[A1,A2],[B1,B2]]), % makes sure we have no diagonals constitued of 2s
    \+ block_of_2([[A1,A2],[B1,B2]]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([[A1,A2],[B1,B2]]), % no head/tail with something in diagonal
    nonTouching_rows([A2, A3 | Alist], [B2, B3 | Blist]).


% cases mentionned by the subject
diag_2_grid([[_,2],[2,_]]).
diag_2_grid([[2,_],[_,2]]).
block_of_2([[2,2],[2,2]]).
nothing_in_diag_of_1([[1,_],[_,0]]). 
nothing_in_diag_of_1([[_,1],[0,_]]). 
nothing_in_diag_of_1([[_,0],[1,_]]). 
nothing_in_diag_of_1([[0,_],[_,1]]). 


valid_4_grid([[0,1], [2,2]]).
valid_4_grid([[0,2], [1,2]]).
valid_4_grid([[0,2], [2,2]]).
valid_4_grid([[1,0], [2,2]]).
valid_4_grid([[2,0], [2,1]]).
valid_4_grid([[2,0], [2,2]]).
valid_4_grid([[1,2], [0,2]]).
valid_4_grid([[2,2], [0,1]]).
valid_4_grid([[2,2], [0,2]]).
valid_4_grid([[2,2], [1,0]]).
valid_4_grid([[2,2], [2,0]]).
valid_4_grid([[2,1], [2,0]]).
valid_4_grid([[2,2], [2,0]]).
valid_4_grid([[0,1], [0,0]]).
valid_4_grid([[0,2], [0,0]]).
valid_4_grid([[0,0], [0,1]]).
valid_4_grid([[0,0], [0,2]]).
valid_4_grid([[1,0], [0,0]]).
valid_4_grid([[2,0], [0,0]]).
valid_4_grid([[1,0], [0,0]]).
valid_4_grid([[0,0], [1,0]]).
valid_4_grid([[0,0], [2,0]]).
valid_4_grid([[0,0], [0,0]]).
valid_4_grid([[0,2], [0,2]]).
valid_4_grid([[0,1], [0,1]]).
valid_4_grid([[0,1], [0,2]]).
valid_4_grid([[0,2], [0,1]]).
valid_4_grid([[0,0], [1,1]]).
valid_4_grid([[0,0], [2,2]]).
valid_4_grid([[0,0], [1,2]]).
valid_4_grid([[0,0], [2,1]]).
valid_4_grid([[1,0], [1,0]]).
valid_4_grid([[2,0], [2,0]]).
valid_4_grid([[1,0], [2,0]]).
valid_4_grid([[2,0], [1,0]]).
valid_4_grid([[1,1], [0,0]]).
valid_4_grid([[2,2], [0,0]]).
valid_4_grid([[1,2], [0,0]]).
valid_4_grid([[2,1], [0,0]]).
valid_4_grid([[1,1], [0,2]]).
valid_4_grid([[1,1], [2,0]]).
valid_4_grid([[1,2], [1,0]]).
valid_4_grid([[1,0], [1,2]]).
valid_4_grid([[2,1], [0,1]]).
valid_4_grid([[0,1], [2,1]]).
valid_4_grid([[2,0], [1,1]]).
valid_4_grid([[0,2], [1,1]]).
valid_4_grid([[1,2], [0,1]]).
valid_4_grid([[1,0], [2,1]]).
valid_4_grid([[0,1], [1,2]]).
valid_4_grid([[1,0], [0,1]]).
valid_4_grid([[0,1], [1,0]]).
% valid_4_grid(Grid4) :-
%     not_case1(Grid4),
%     not_case2(Grid4),
%     not_case3(Grid4),
%     not_case4(Grid4),
%     not_case5(Grid4),
%     not_case6(Grid4),
%     not_case7(Grid4).

not_case1([[A1, A2], [B1, B2]]) :-
   
   A1 + B2 =\= 3; A2 + B1 =\= 0.


not_case2([[A1, A2], [B1, B2]]) :-    A2 + B1 =\= 3; A1 + B2 =\= 0.
not_case3([[A1, A2], [B1, B2]]) :-    A1 + B2 =\= 4; A2 + B1 =\= 0.
not_case4([[A1, A2], [B1, B2]]) :-    A2 + B1 =\= 4; A1 + B2 =\= 0.
not_case5([[A1, A2], [B1, B2]]) :-    A1 + A2 + B1 + B2 =\= 8.
not_case6([[A1, A2], [B1, B2]]) :-    A1 =\= 1; B2 =\= 1; A2 + B1 =\= 0.
not_case7([[A1, A2], [B1, B2]]) :-    A2 =\= 1; B1 =\= 1; A1 + B2 =\= 0.

