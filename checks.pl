% base function used in snake predicate
% enum and check rows
checkRowClues([],[]).

% skip case with no hint on row
checkRowClues([_|Rest_Of_Solution],[-1|Rest_Of_Hints]) :- 
                checkRowClues(Rest_Of_Solution,Rest_Of_Hints).

% case with an hint for this row
checkRowClues([Row|Rest_Of_Solution],[Hint|Rest_Of_Hints]) :- count_parts_in_row(Row,Count), 
                                Count #= Hint,
                                checkRowClues(Rest_Of_Solution,Rest_Of_Hints).

% sum in row

% base case, end of row
count_parts_in_row([],0).

count_parts_in_row([Cell_Value|Rest_Of_Row],CP1) :- 
                                Cell_Value #> 0, % prevent null values
                                count_parts_in_row(Rest_Of_Row,Count), 
                                CP1 is Count+1. % we found a cell containing a snake part,
                                                % increment counter

% case without snake in the Cell, do not increment the counter and skip to next iteration
count_parts_in_row([_|Rest_Of_Row],Count) :- count_parts_in_row(Rest_Of_Row,Count).


% base function used in snake predicate

checkColClues(S,C) :- doesAllColCluesMatch(S,C,0).

% requirements on cell sum on columns (match with clues)

% enum columns
doesAllColCluesMatch(_,[],_).

doesAllColCluesMatch(S,[Hint|Rest_Of_Hints],Col_Index) :- 
                                    count_parts_in_col(S,Col_Index,Count), 
                                    Count #= Hint,
                                    Next_Col_Index is Col_Index + 1,
                                    doesAllColCluesMatch(S, Rest_Of_Hints, Next_Col_Index).

% in case of absence of clue (-1), skip this (not so) hint
doesAllColCluesMatch(S,[-1|Rest_Of_Hints],Col_Index) :-
                                    Next_Col_Index is Col_Index + 1,
                                    doesAllColCluesMatch(S, Rest_Of_Hints, Next_Col_Index).

% sum in a column
% base case, no more rows to check
count_parts_in_col([],_,0).

% increment counter if we find snake part in the given cell
count_parts_in_col([Row|Rest_Of_Solution],Col_Index,CP1) :- 
                                    nth0(Col_Index,Row,Cell_Value),            
                                    Cell_Value #> 0, % make sure non-0 value
                                    count_parts_in_col(Rest_Of_Solution,Col_Index,Count),
                                    CP1 is Count+1.
% skip row if not
count_parts_in_col([_|Rest_Of_Solution],Col_Index,Count) :-
                                    count_parts_in_col(Rest_Of_Solution,Col_Index,Count).


% testing if the predicates are leading to a solution meeting hints 


% no hints
test1(S) :- copyGrid([[ 1, 1],[-1,-1]],S),
            % testRowRules([-1,-1],S), 
            % testColRules([-1,-1],S), 
            print_only_grid(S).


% must have 1 on row 1 of the grid and 2 (one or 2) on the first column, multiple solutions
test2(S) :- copyGrid([[-1,-1, 1],[-1,-1,-1],[ 1,-1,-1]],S),
             maplist(label,S), % force variable instanciation
             checkRowClues(S,[ 1,-1,-1]), 
             checkColClues(S,[ 2,-1,-1]),
            print_only_grid(S),
            nl.
