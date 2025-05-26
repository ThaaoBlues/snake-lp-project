%base case

nonTouching([Row1, Row2]) :- nonTouching_rows(Row1, Row2).

nonTouching([Row1, Row2, Row3 | Rows]) :-
    nonTouching_rows(Row1, Row2),
    nonTouching([Row2, Row3 | Rows]).

%base case
nonTouching_rows(R1, R2) :- 
    %\+ diag_2_grid([R1,R2]), % makes sure we have no diagonals constitued of 2s
    block_of_2([R1,R2]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([R1,R2]). % no head/tail with something in diagonal

nonTouching_rows([A1, A2 | AList], [B1, B2 | BList]) :- 
    %valid_4_grid([[A1, A2], [B1, B2]]),
    %\+ diag_2_grid([[A1,A2],[B1,B2]]), % makes sure we have no diagonals constitued of 2s
    block_of_2([[A1,A2],[B1,B2]]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([[A1,A2],[B1,B2]]), % no head/tail with something in diagonal
    nonTouching_rows([A2 | AList], [B2 | BList]).


% cases mentionned by the subject
% non constraint version (labeled variables)
%diag_2_grid([[_,2],[2,_]]).
%diag_2_grid([[2,_],[_,2]]).

% constrained version, unlabeled variables
block_of_2([[A,B],[C,D]]) :- ( (A #\= 2) #\/ (B #\= 2) #\/ (C #\= 2) #\/ (D #\= 2) ).

% non constraint version (labeled variables)
%nothing_in_diag_of_1([[1,_],[_,X]]):- X in 0 \/ 2. % 1 can only have a body part or nothing as diagonal
%nothing_in_diag_of_1([[_,1],[X,_]]):- X in 0 \/ 2. 
%nothing_in_diag_of_1([[_,X],[1,_]]):- X in 0 \/ 2. 
%nothing_in_diag_of_1([[X,_],[_,1]]):- X in 0 \/ 2. 

% constrained version, unlabeled variables
% ==> means if left hand side then right hand side
nothing_in_diag_of_1([[A1, A2], [B1, B2]]) :-
    % Diagonals: A1-B2 and A2-B1
    ((A1 #= 1) #==> (B2 in 0 \/ 2)),
    ((A2 #= 1) #==> (B1 in 0 \/ 2)),
    ((B1 #= 1) #==> (A2 in 0 \/ 2)),
    ((B2 #= 1) #==> (A1 in 0 \/ 2)).


% accept the case where no 1 is on the 4x4 chunk
nothing_in_diag_of_1([[A1,A2],[B1,B2]]) :- A1 #\=1, A2 #\=1, B1 #\=1, B2 #\=1.

%valid_4_grid([[0,1], [2,2]]).
%valid_4_grid([[0,2], [1,2]]).
%valid_4_grid([[0,2], [2,2]]).
%valid_4_grid([[1,0], [2,2]]).
%valid_4_grid([[2,0], [2,1]]).
%valid_4_grid([[2,0], [2,2]]).
%valid_4_grid([[1,2], [0,2]]).
%valid_4_grid([[2,2], [0,1]]).
%valid_4_grid([[2,2], [0,2]]).
%valid_4_grid([[2,2], [1,0]]).
%valid_4_grid([[2,2], [2,0]]).
%valid_4_grid([[2,1], [2,0]]).
%valid_4_grid([[2,2], [2,0]]).
%valid_4_grid([[0,1], [0,0]]).
%valid_4_grid([[0,2], [0,0]]).
%valid_4_grid([[0,0], [0,1]]).
%valid_4_grid([[0,0], [0,2]]).
%valid_4_grid([[1,0], [0,0]]).
%valid_4_grid([[2,0], [0,0]]).
%valid_4_grid([[1,0], [0,0]]).
%valid_4_grid([[0,0], [1,0]]).
%valid_4_grid([[0,0], [2,0]]).
%valid_4_grid([[0,0], [0,0]]).
%valid_4_grid([[0,2], [0,2]]).
%valid_4_grid([[0,1], [0,1]]).
%valid_4_grid([[0,1], [0,2]]).
%valid_4_grid([[0,2], [0,1]]).
%valid_4_grid([[0,0], [1,1]]).
%valid_4_grid([[0,0], [2,2]]).
%valid_4_grid([[0,0], [1,2]]).
%valid_4_grid([[0,0], [2,1]]).
%valid_4_grid([[1,0], [1,0]]).
%valid_4_grid([[2,0], [2,0]]).
%valid_4_grid([[1,0], [2,0]]).
%valid_4_grid([[2,0], [1,0]]).
%valid_4_grid([[1,1], [0,0]]).
%valid_4_grid([[2,2], [0,0]]).
%valid_4_grid([[1,2], [0,0]]).
%valid_4_grid([[2,1], [0,0]]).
%valid_4_grid([[1,1], [0,2]]).
%valid_4_grid([[1,1], [2,0]]).
%valid_4_grid([[1,2], [1,0]]).
%valid_4_grid([[1,0], [1,2]]).
%valid_4_grid([[2,1], [0,1]]).
%valid_4_grid([[0,1], [2,1]]).
%valid_4_grid([[2,0], [1,1]]).
%valid_4_grid([[0,2], [1,1]]).
%valid_4_grid([[1,2], [0,1]]).
%valid_4_grid([[1,0], [2,1]]).
%valid_4_grid([[0,1], [1,2]]).
%valid_4_grid([[1,0], [0,1]]).
%valid_4_grid([[0,1], [1,0]]).

% valid_4_grid(Grid4) :-
%     not_case1(Grid4),
%     not_case2(Grid4),
%     not_case3(Grid4),
%     not_case4(Grid4),
%     not_case5(Grid4),
%     not_case6(Grid4),
%     not_case7(Grid4).

%not_case1([[A1, A2], [B1, B2]]). % :-
   
   % A1 + B2 =\= 3; A2 + B1 =\= 0.


%not_case2([[A1, A2], [B1, B2]]).% :-    A2 + B1 =\= 3; A1 + B2 =\= 0.
%not_case3([[A1, A2], [B1, B2]]). % :-    A1 + B2 =\= 4; A2 + B1 =\= 0.
%not_case4([[A1, A2], [B1, B2]]). % :-    A2 + B1 =\= 4; A1 + B2 =\= 0.
%not_case5([[A1, A2], [B1, B2]]). % :-    A1 + A2 + B1 + B2 =\= 8.
%not_case6([[A1, A2], [B1, B2]]). % :-    A1 =\= 1; B2 =\= 1; A2 + B1 =\= 0.
%not_case7([[A1, A2], [B1, B2]]). % :-    A2 =\= 1; B1 =\= 1; A1 + B2 =\= 0.