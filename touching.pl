%base case

nonTouching([Row1, Row2]) :- nonTouching_rows(Row1, Row2), !.

nonTouching([Row1, Row2 | Rows]) :-
    nonTouching_rows(Row1, Row2),
    nonTouching([Row2 | Rows]).

%base case
nonTouching_rows(R1, R2) :- 
    block_of_2([R1,R2]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([R1,R2]),!. % no head/tail with something in diagonal

nonTouching_rows([A1, A2 | AList], [B1, B2 | BList]) :- 
    block_of_2([[A1,A2],[B1,B2]]), % no 4 block of 2s (technically induced by no diag ?)
    nothing_in_diag_of_1([[A1,A2],[B1,B2]]), % no head/tail with something in diagonal
    diagonals_of_two([[A1,A2],[B1,B2]]),
    nonTouching_rows([A2 | AList], [B2 | BList]).



% constrained version, unlabeled variables
block_of_2([[A1,A2],[B1,B2]]) :- ((A1 #\= 2) #\/ (A2 #\= 2) #\/ (B1 #\= 2) #\/ (B2 #\= 2)).
    

diagonals_of_two([[A1,A2],[B1,B2]]) :- 
    ((A1 #= 2) #/\ (A2 #= 0) #/\ (B1 #= 0) #==> (B2 #= 0)),
    ((A2 #= 2) #/\ (A1 #= 0) #/\ (B2 #= 0) #==> (B1 #= 0)).
% non constraint version (labeled variables)
%nothing_in_diag_of_1([[1,_],[_,X]]):- X in 0 \/ 2. % 1 can only have a body part or nothing as diagonal
%nothing_in_diag_of_1([[_,1],[X,_]]):- X in 0 \/ 2. 
%nothing_in_diag_of_1([[_,X],[1,_]]):- X in 0 \/ 2. 
%nothing_in_diag_of_1([[X,_],[_,1]]):- X in 0 \/ 2. 

% constrained version, unlabeled variables
% ==> means if left hand side then right hand side
nothing_in_diag_of_1([[A1, A2], [B1, B2]]) :-
    % Diagonals: A1-B2 and A2-B1
    ((A1 #= 1) #/\ (A2 #= 0) #/\ (B1 #= 0) #==> (B2 #= 0)),
    ((A2 #= 1) #/\ (A1 #= 0) #/\ (B2 #= 0) #==> (B1 #= 0))
    %((B1 #= 1) #/\ (A1 #= 0) #/\ (B2 #= 0) #==> (A2 #= 0)),
    %((B2 #= 1) #/\ (A2 #= 0) #/\ (B1 #= 0) #==> (A1 #= 0))
    ,!
    .

% accept the case where no 1 is on the 4x4 chunk
nothing_in_diag_of_1([[A1,A2],[B1,B2]]) :- A1 #\=1, A2 #\=1, B1 #\=1, B2 #\=1.
