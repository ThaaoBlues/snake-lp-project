%snakeConnected(Solution) :-

% get a head and a tail
% start at the head
% go to the next cell in the snake
%     -> get neighbors and keep track of where you came from
% until you arrive at the tail

snakeConnected(Solution) :-
    getHead(Solution, H),
    nextSnake(Solution, [], H, Path),
    length(Path, Length),
    Snake is Length,
    count(Solution, Count),
    Snake = Count, !.

head(cell(_,_,1)).

getHead(Grid, cell(X,Y,1)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, 1), !.

getValue(Grid, cell(X,Y,V)) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, V), !.

neighboring(cell(Xc,Y,_), cell(Xn,Y,_)) :-
    Xn is Xc + 1;
    Xn is Xc - 1.

neighboring(cell(X,Yc,_), cell(X,Yn,_)) :-
    Yn is Yc + 1;
    Yn is Yc - 1.

nextSnake(Grid, _, Cell, [Cell]) :- 
    head(Cell), 
    getHead(Grid, H),
    not(Cell = H), !.
nextSnake(Grid, Visited, Current, [Current, cell(Xn,Yn,V)|Snake]) :-
    neighboring(Current, cell(Xn,Yn,V)),
    getValue(Grid, cell(Xn, Yn, V)),
    (V = 1 ; V = 2),
    not(member(cell(Xn,Yn,V), Visited)),
    nextSnake(Grid, [Current|Visited], cell(Xn,Yn,V), [cell(Xn,Yn,V)|Snake]).

count([], 0).
count([Row|Grid], Count) :-
    count(Grid, C),
    countRow(Row, Cr),
    Count is C + Cr.

countRow([], 0).
countRow([1|Row], Count) :- !,
    countRow(Row, C),
    Count is C + 1.
countRow([2|Row], Count) :- !,
    countRow(Row, C),
    Count is C + 1.
countRow([_|Row], Count) :-
    countRow(Row, Count).