:- include('generator.pl').
:- include('aula.pl').

choosePuzzleStyle(X):-write('1-Choose board'),nl,write('2-RandomBoard'),nl,prcss_ans(1,2,X).

start:-choosePuzzleStyle(X),playThepuzzle(X).

playThepuzzle(1):-askId(ID),board1(ID,L),init(ID,L).

askId(ID):-write('Choose Id'),nl,prcss_ans(2,50,ID).

playThepuzzle(2):-askSize(Size),generate(Size,L),init(Size,L).

askSize(Size):-write('Choose Size of Board'),nl,prcss_ans(2,50,Size).


prcss_ans(Min,Max,Ans):-read_line(X),lineToNumber(X,Ans),Ans>=Min,Ans=<Max.

lineToNumber([X1|X2],Ans):-lineToNumber(X2,Ans1),(Ans1 \= -1,Ans is (X1-48)*10+Ans1;Ans is X1-48).
lineToNumber([],-1).
