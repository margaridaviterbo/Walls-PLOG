:- use_module(library(clpfd)).
:- use_module(library(lists)).


:-dynamic board1/2.

board1(2,[[1,A],[B,1]]).

board1(6,[[A,B,4,C,D,1],[E,2,F,G,H,I],[3,J,K,1,L,M],[N,O,1,P,Q,2],[R,S,T,U,2,V],[2,X,Y,3,W,Z]]).

%board1(15,[[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14],[B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14],
%[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14],[D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14],
%[E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14],[F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14],
%[H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H14],[G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14],
%[I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14],[J1,J2,J3,J4,J5,J6,J7,J8,J9,J10,J11,J12,J13,J14],
%[K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14],[L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14],
%[M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14],[N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14],
%[O1,O2,O3,O4,O5,O6,O7,O8,O9,O10,O11,O12,O13,O14]]).

board1(15,[[14,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15],[14,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15],
[14,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15],[14,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15],
[14,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14,E15],[14,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15],
[14,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H14,H15],[14,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,G13,G14,G15],
[14,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15],[14,J2,J3,J4,J5,J6,J7,J8,J9,J10,J11,J12,J13,J14,J15],
[14,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14,K15],[14,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15],
[14,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15],[14,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15],
[14,O2,O3,O4,O5,O6,O7,O8,O9,O10,O11,O12,O13,O14,O15]]).

board1(3,[[A,B,C],[D,E,F],[G,3,H],[I,J,K]]).



restrainHor(L,N):-automaton(L, _, L,[source(s),sink(i),sink(s)],[arc(s,1,s,[C+1]),arc(s,2,i),arc(i,1,i),arc(i,2,i)],[C],[0],[N],[]).

restrainVer(L,N):-automaton(L, _, L,[source(s),sink(i),sink(s)],[arc(s,2,s,[C+2]),arc(s,1,i),arc(i,1,i),arc(i,2,i)],[C],[0],[N],[]).

display_board([L1|L2],Size):-display_line(L1),write(':'),nl,display_separator(Size,1),display_board(L2,Size).
display_board([],_).

display_line([L1|L2]):-write(':'),(var(L1),write(' '),!;write(L1)),display_line(L2).
display_line([]).

display_separator(Size,It):-It=<Size,!,write('..'),It1 is It+1,display_separator(Size,It1).
display_separator(_,_):-write('.'),nl.

init(Size,L):-retract(board1(_,_)),asserta(board1(Size,L)),display_separator(Size,1),display_board(L,Size),nl,nl,main(L,Size),board1(_,OldPuzzle),display_separator(Size,1),draw_solution(OldPuzzle,L,Size).

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