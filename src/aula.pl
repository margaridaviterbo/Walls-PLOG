:- use_module(library(clpfd)).
:- use_module(library(lists)).


:-dynamic board1/2.

board1(2,[[1,1],[B,1]]).

board1(6,[[A,B,4,C,D,1],[E,2,F,G,H,I],[3,J,K,1,L,M],[N,O,1,P,Q,2],[R,S,T,U,2,V],[2,X,Y,3,W,Z]]).

board1(10,[[A,A1,2,A2,A3,A4,A5,A6,A7,6],[B,B1,B2,B3,6,B4,B5,B6,B7,B8],
[C,6,C1,C2,3,C4,C5,8,7,6],[D,D1,1,D2,D3,D4,D5,D6,D7,D8],[1,E1,E2,E3,E4,7,E5,E6,E7,E8],
[1,F1,1,F2,1,F4,F5,F6,F7,F8],[G,G1,4,G2,G3,G4,3,G6,G7,G8],[1,4,H1,H2,H3,H4,4,H6,H7,H8],
[I,I1,I2,I3,I4,6,I5,I6,I7,6],[1,J1,6,J2,J3,J4,J5,J6,J7,J8]]).

board1(15,[[A1,4,A3,A4,A5,7,A7,A8,A9,A10,4,3,A13,A14,A15],[B1,B2,B3,B4,B5,B6,B7,B8,B9,11,B11,B12,B13,B14,1],
[7,C2,C3,C4,C5,C6,C7,1,C9,C10,C11,3,C13,4,C15],[D1,D2,D3,D4,D5,8,6,D8,D9,D10,D11,D12,5,D14,D15],
[E1,E2,1,E4,2,E6,4,E8,E9,E10,E11,E12,E13,6,E15],[F1,3,F3,F4,F5,F6,F7,F8,6,F10,F11,3,F13,F14,F15],
[2,H2,H3,4,H5,H6,H7,H8,H9,H10,H11,2,H13,H14,H15],[2,G2,2,G4,1,G6,3,G8,G9,3,G11,G12,G13,G14,G15],
[I1,I2,4,I4,I5,I6,I7,I8,3,I10,I11,I12,6,I14,6],[8,J2,J3,J4,J5,5,J7,J8,J9,J10,5,J12,4,J14,J15],
[K1,K2,K3,10,K5,K6,K7,7,K9,K10,K11,K12,K13,K14,K15],[L1,L2,L3,L4,L5,L6,L7,L8,7,L10,L11,L12,L13,L14,3],
[M1,M2,1,M4,3,M6,6,M8,M9,M10,M11,M12,9,M14,M15],[N1,4,N3,N4,N5,1,N7,8,N9,N10,N11,N12,N13,N14,N15],
[O1,O2,O3,O4,O5,O6,O7,O8,O9,O10,O11,9,O13,O14,O15]]).

board1(20,[[1,A2,2,A4,A5,A6,1,A8,A9,9,A11,A12,A13,A14,A15,A16,A17,A18,9,A20],
[B1,B2,B3,B4,B5,5,B7,B8,1,B10,B11,B12,6,B14,B15,B16,B17,B18,B19,B20],
[1,C2,C3,C4,C5,C6,C7,13,C9,C10,C11,C12,C13,C14,C15,C16,C17,5,C19,3],
[D1,12,D3,D4,D5,D6,D7,9,D9,D10,D11,D12,D13,D14,13,D16,D17,D18,1,D20],
[2,E2,E3,E4,E5,E6,E7,E8,E9,11,E11,E12,E13,E14,E15,E16,1,E18,1,E20],
[F1,F2,12,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,6,5,F19,F20],
[H1,H2,H3,5,H5,H6,H7,H8,H9,6,2,H12,2,H14,H15,H16,H17,H18,H19,H20],
[2,10,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12,2,G14,G15,3,G17,4,3,5],
[10,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,4,I14,I15,I16,I17,I18,I19,I20],
[J1,J2,2,2,J5,J6,2,3,J9,J10,J11,5,J13,1,J15,J16,J17,J18,J19,J20],
[10,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14,K15,2,K17,K18,4,K20],
[L1,L2,L3,L4,5,2,L7,L8,3,L10,2,L12,L13,2,L15,L16,L17,12,L19,L20],
[M1,M2,M3,3,M5,M6,M7,M8,M9,7,M11,M12,M13,M14,M15,M16,M17,M18,M19,M20],
[N1,N2,N3,N4,N5,N6,14,N8,N9,N10,N11,N12,N13,N14,N15,3,N17,N18,N19,7],
[3,O2,O3,O4,4,O6,O7,O8,7,O10,O11,O12,O13,O14,O15,O16,O17,O18,2,O20],
[P1,P2,P3,P4,P5,P6,15,P8,P9,P10,P11,P12,P13,P14,P15,2,P17,P18,P19,5],
[Q1,Q2,Q3,4,Q5,Q6,Q7,4,Q9,Q10,Q11,Q12,6,Q14,3,1,Q17,Q18,Q19,Q20],
[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,10,R13,R14,R15,1,R17,R18,4,R20],
[6,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,11,S16,1,S18,S19,S20],
[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,13,T14,T15,4,T17,T18,T19,T20]]).

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


%aplicar restrições horizontais a uma lista
restrainHor(L,N):-automaton(L, _, L,[source(s),sink(i),sink(s)],[arc(s,1,s,[C+1]),arc(s,2,i),arc(i,1,i),arc(i,2,i)],[C],[0],[N],[]).

%aplicar restrições verticais a uma lista
restrainVer(L,N):-automaton(L, _, L,[source(s),sink(i),sink(s)],[arc(s,2,s,[C+2]),arc(s,1,i),arc(i,1,i),arc(i,2,i)],[C],[0],[N],[]).

%mostrar o puzzle no ecrã
display_board([L1|L2],Size):-display_line(L1),write(':'),nl,display_separator(Size,1),display_board(L2,Size).
display_board([],_).

%auxiliar para mostrar o puzzle no ecrã
display_line([L1|L2]):-write(':'),(var(L1),write(' '),!;write(L1)),display_line(L2).
display_line([]).

%linha separadora (efeito visual)
display_separator(Size,It):-It=<Size,!,write('..'),It1 is It+1,display_separator(Size,It1).
display_separator(_,_):-write('.'),nl.

%inicia os devidos campos e pede para se resolver o puzzle
init(Size,L):-retract(board1(_,_)),asserta(board1(Size,L)),display_separator(Size,1),display_board(L,Size),nl,nl,main(L,Size),board1(_,OldPuzzle),display_separator(Size,1),draw_solution(OldPuzzle,L,Size).

%resolve o puzzle
main(Matrix,Size):-findNumberPaths(Matrix,1,Size,Paths,NumberArray),calculateDomain(Matrix,Domain),puzzle(Paths,NumberArray,Domain).

%predicado para desenhar a solução do puzzle
draw_solution([L1|L2],[L3|L4],Size):-draw_solution_line(L1,L3),write(':'),nl,display_separator(Size,1),draw_solution(L2,L4,Size).
draw_solution([],[],_).

%auxiliar para desenhar a solução do puzzle
draw_solution_line([L1|L2],[L3|L4]):-write(':'),(var(L1),(L3==1,write('-'),!;write('|')),!;write(L1)),draw_solution_line(L2,L4).
draw_solution_line([],[]).

%declarar dominio, restrições e o labeling
puzzle(Paths,NumberArray,Domain):-domain(Domain,1,2),restrain(Paths,NumberArray),labeling([],Domain).

%todas as restriçoes relativas ao puzzle estão aqui e são chamadas por este predicado (apenas faz restrições)
restrain([Path1,Path2,Path3,Path4|Paths],[N1|NR]):-
length(Path1,TotalSize1),domain([VSize1],0,TotalSize1),restrainHor(Path1,VSize1),
length(Path2,TotalSize2),domain([VSize2],0,TotalSize2),restrainHor(Path2,VSize2),
length(Path3,TotalSize3),TotalSize31 is 2*TotalSize3,domain([VSize3],0,TotalSize31),restrainVer(Path3,VSize3),
length(Path4,TotalSize4),TotalSize41 is 2*TotalSize4,domain([VSize4],0,TotalSize41),restrainVer(Path4,VSize4),
VSize31 #= VSize3/2, VSize41 #=VSize4/2,
N1#=VSize1+VSize2+VSize41+VSize31,restrain(Paths,NR).
restrain([],[]).

%predicado que recebe um tabuleiro e retorna uma lista com as variáveis não declaradas sem repetições (= tabuleiro sem os números)
calculateDomain([L1|L2],R):-calculateDomainAux(L1,R1),calculateDomain(L2,R2),append(R1,R2,R).
calculateDomain([],[]).

%predicado auxiliar de caulculateDomain
calculateDomainAux([L1|L2],R):-number(L1),!,calculateDomainAux(L2,R).
calculateDomainAux([L1|L2],[L1|R2]):-calculateDomainAux(L2,R2).
calculateDomainAux([],[]).

%retorna uma lista de listas com as variáveis não declaradas
findNumberPaths(Matrix,X,Size,LR,NumberPath):-length(Matrix,Tamanho),Tamanho>=X,!,findNumberPathsAux(Matrix,[X,1],Size,L1,Number1),X1 is X+1,findNumberPaths(Matrix,X1,Size,L2,Number2),
append(L1,L2,LR),append(Number1,Number2,NumberPath).
findNumberPaths(_,_,_,[],[]).

%para um dado número do puzzle encontra todas as variáveis que estão à sua esquerda, direita,cima e baixo e coloca numa lista de listas, sendo cada caminho uma lista
findNumberPathsAux(Matrix,[X,Y],Size,[L1,L2,L3,L4|LR],[Value|Nr]):-Y=<Size,getElement(Matrix, X, Y, Value), number(Value),!,
findHorizontalPath(Matrix,[X,Y],Size,L1,L2),length(Matrix,SizeVer),findVerticalPath(Matrix,[X,Y],SizeVer,L3,L4),
Y1 is Y+1,findNumberPathsAux(Matrix,[X,Y1],Size,LR,Nr).

findNumberPathsAux(Matrix,[X,Y],Size,LR,Nr):-Y=<Size,Y1 is Y+1,!,findNumberPathsAux(Matrix,[X,Y1],Size,LR,Nr).
findNumberPathsAux(_,[_,_],_,[],[]).

%pede os caminhos de um número para a esquerda e para a direita e devolve-os
findHorizontalPath(Matrix,[X,Y],Size,L1,L2):-findRightPath(Matrix,[X,Y],Size,L1),findLeftPath(Matrix,[X,Y],Size,L2).

%procura o caminho para a direita e devolve
findRightPath(Matrix,[X,Y],Size,[Value|L2]):-Y1 is Y+1, Y1=<Size, getElement(Matrix, X, Y1, Value), \+ number(Value),!,findRightPath(Matrix, [X,Y1],Size,L2).
findRightPath(_,[_,_],_,[]).

%procura o caimnho para a esquerda e devolve
findLeftPath(Matrix,[X,Y],_,[Value|L2]):-Y1 is Y-1, Y1>0, getElement(Matrix, X, Y1, Value), \+ number(Value),!,findLeftPath(Matrix, [X,Y1],_,L2).
findLeftPath(_,[_,_],_,[]).

%pede os caminhos de um número para cim e para baixo e devolve-os
findVerticalPath(Matrix,[X,Y],Size,L1,L2):-findUpPath(Matrix,[X,Y],Size,L1),findLowPath(Matrix,[X,Y],Size,L2).

%procura o caminho para baixo e devolve
findLowPath(Matrix,[X,Y],Size,[Value|L2]):-X1 is X+1, X1=<Size, getElement(Matrix, X1, Y, Value), \+ number(Value),!,findLowPath(Matrix, [X1,Y],Size,L2).
findLowPath(_,[_,_],_,[]).

%procura o caminho para cima e devolve
findUpPath(Matrix,[X,Y],_,[Value|L2]):-X1 is X-1, X1>0, getElement(Matrix, X1, Y, Value), \+ number(Value),!,findUpPath(Matrix, [X1,Y],_,L2).
findUpPath(_,[_,_],_,[]).

% Retorna elemento de uma posicao X,Y
getElement(Matrix, Row, Col, Value):-
  nth1(Row, Matrix, MatrixRow),
  nth1(Col, MatrixRow, Value).
