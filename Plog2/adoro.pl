:- use_module(library(clpfd)).
:- use_module(library(lists)).

board(2,[[1,A],[B,1]]).

board1(6,[[A,B,4,C,D,1],[E,2,F,G,H,I],[3,J,K,1,L,M],[N,O,1,P,Q,2],[R,S,T,U,2,V],[2,X,Y,3,W,Z]]).

%board1(3,[[A,B,C],[D,E,F],[G,3,H],[I,J,K]]).

%board1(6,[[A,B,4,C,D,1],[E,2,F,G,H,I],[3,J,K,1,L,M],[N,O,1,P,Q,2],[R,S,T,U,3,V],[A1,B1,C1,D1,E1,F1]]).

restrainHor([L1,L2|LR],N):-restrainHor([L2|LR],N1),(L1#=L2)#<=>B,(B#=1 #/\N#=0#/\L1#=2)#\(B#=0 #/\N#=0#/\L1#=2)#\(L1#\=2#/\B#=1#/\ N#=N1+1)#\(B#=0 #/\N#=1#/\L1#=1).
restrainHor([L1],N):-(L1#=1#/\N#=1)#\(L1#=2#/\N#=0).
restrainHor([],0).

restrainVer([L1,L2|LR],N):-restrainVer([L2|LR],N1),(L1#=L2)#<=>B,(B#=1 #/\N#=0#/\L1#=1)#\(B#=0 #/\N#=0#/\L1#=1)#\(L1#\=1#/\B#=1#/\ N#=N1+2)#\(B#=0 #/\N#=2#/\L1#=2).
restrainVer([L1],N):-(L1#=1#/\N#=0)#\(L1#=2#/\N#=2).
restrainVer([],0).

display_board([L1|L2],Size):-display_line(L1),write(':'),nl,display_separator(Size,1),display_board(L2,Size).
display_board([],_).

display_line([L1|L2]):-write(':'),(var(L1),write(' '),!;write(L1)),display_line(L2).
display_line([]).

display_separator(Size,It):-It=<Size,!,write('..'),It1 is It+1,display_separator(Size,It1).
display_separator(_,_):-write('.'),nl.

init(L):-board1(Size,L),display_separator(Size,1),display_board(L,Size),nl,nl,main(L,Size),board1(_,OldPuzzle),display_separator(Size,1),draw_solution(OldPuzzle,L,Size).

main(Matrix,Size):-findNumberPaths(Matrix,1,Size,Paths,NumberArray),calculateDomain(Matrix,Domain),puzzle(Paths,NumberArray,Domain).

draw_solution([L1|L2],[L3|L4],Size):-draw_solution_line(L1,L3),write(':'),nl,display_separator(Size,1),draw_solution(L2,L4,Size).
draw_solution([],[],_).

draw_solution_line([L1|L2],[L3|L4]):-write(':'),(var(L1),(L3==1,write('-'),!;write('|')),!;write(L1)),draw_solution_line(L2,L4).
draw_solution_line([],[]).

puzzle(Paths,NumberArray,Domain):-domain(Domain,1,2),restrain(Paths,NumberArray),labeling([],Domain).

restrain([Path1,Path2,Path3,Path4|Paths],[N1|NR]):-
length(Path1,TotalSize1),domain([VSize1],0,TotalSize1),restrainHor(Path1,VSize1),
length(Path2,TotalSize2),domain([VSize2],0,TotalSize2),restrainHor(Path2,VSize2),
length(Path3,TotalSize3),TotalSize31 is 2*TotalSize3,domain([VSize3],0,TotalSize31),restrainVer(Path3,VSize3),
length(Path4,TotalSize4),TotalSize41 is 2*TotalSize4,domain([VSize4],0,TotalSize41),restrainVer(Path4,VSize4),
VSize31 #= VSize3/2, VSize41 #=VSize4/2,
N1#=VSize1+VSize2+VSize41+VSize31,restrain(Paths,NR).
restrain([],[]).

calculateDomain([L1|L2],R):-calculateDomainAux(L1,R1),calculateDomain(L2,R2),append(R1,R2,R).
calculateDomain([],[]).

calculateDomainAux([L1|L2],R):-number(L1),!,calculateDomainAux(L2,R).
calculateDomainAux([L1|L2],[L1|R2]):-calculateDomainAux(L2,R2).
calculateDomainAux([],[]).

findNumberPaths(Matrix,X,Size,LR,NumberPath):-length(Matrix,Tamanho),Tamanho>=X,!,findNumberPathsAux(Matrix,[X,1],Size,L1,Number1),X1 is X+1,findNumberPaths(Matrix,X1,Size,L2,Number2),
append(L1,L2,LR),append(Number1,Number2,NumberPath).
findNumberPaths(_,_,_,[],[]).

findNumberPathsAux(Matrix,[X,Y],Size,[L1,L2,L3,L4|LR],[Value|Nr]):-Y=<Size,getElement(Matrix, X, Y, Value), number(Value),!,
findHorizontalPath(Matrix,[X,Y],Size,L1,L2),length(Matrix,SizeVer),findVerticalPath(Matrix,[X,Y],SizeVer,L3,L4),
Y1 is Y+1,findNumberPathsAux(Matrix,[X,Y1],Size,LR,Nr).
findNumberPathsAux(Matrix,[X,Y],Size,LR,Nr):-Y=<Size,Y1 is Y+1,!,findNumberPathsAux(Matrix,[X,Y1],Size,LR,Nr).
findNumberPathsAux(_,[_,_],_,[],[]).

findHorizontalPath(Matrix,[X,Y],Size,L1,L2):-findRightPath(Matrix,[X,Y],Size,L1),findLeftPath(Matrix,[X,Y],Size,L2).

findRightPath(Matrix,[X,Y],Size,[Value|L2]):-Y1 is Y+1, Y1=<Size, getElement(Matrix, X, Y1, Value), \+ number(Value),!,findRightPath(Matrix, [X,Y1],Size,L2).
findRightPath(_,[_,_],_,[]).

findLeftPath(Matrix,[X,Y],_,[Value|L2]):-Y1 is Y-1, Y1>0, getElement(Matrix, X, Y1, Value), \+ number(Value),!,findLeftPath(Matrix, [X,Y1],_,L2).
findLeftPath(_,[_,_],_,[]).

findVerticalPath(Matrix,[X,Y],Size,L1,L2):-findUpPath(Matrix,[X,Y],Size,L1),findLowPath(Matrix,[X,Y],Size,L2).

findLowPath(Matrix,[X,Y],Size,[Value|L2]):-X1 is X+1, X1=<Size, getElement(Matrix, X1, Y, Value), \+ number(Value),!,findLowPath(Matrix, [X1,Y],Size,L2).
findLowPath(_,[_,_],_,[]).

findUpPath(Matrix,[X,Y],_,[Value|L2]):-X1 is X-1, X1>0, getElement(Matrix, X1, Y, Value), \+ number(Value),!,findUpPath(Matrix, [X1,Y],_,L2).
findUpPath(_,[_,_],_,[]).

% Retorna elemento de uma posicao X,Y
getElement(Matrix, Row, Col, Value):-
  nth1(Row, Matrix, MatrixRow),
  nth1(Col, MatrixRow, Value).

specialElement([L1|_],X,It,L1):-X==It,!.
specialElement([],_,_,A):-!.
specialElement([L1|L2],X,It,Ret):-It1 is It+1,specialElement(L2,X,I1,Ret).
