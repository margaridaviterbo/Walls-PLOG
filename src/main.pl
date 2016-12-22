:- include('generator.pl').
:- include('aula.pl').

%modo de puzzle
choosePuzzleStyle(X):-write('1-Choose board'),nl,write('2-RandomBoard'),nl,prcss_ans(1,2,X).

%predicado de começo
start:-choosePuzzleStyle(X),playThepuzzle(X).

%pergunta qual o tabuleiro que quer ver resolvido e pede para se resolver
playThepuzzle(1):-askId(ID),board1(ID,L),init(ID,L).

%pede id do tabuleiro que quer ver resolvido ao utilizador
askId(ID):-write('Choose Id'),nl,prcss_ans(2,50,ID).

%gera um tabuleiro com um tamanho arbitrário e pede para resolver
playThepuzzle(2):-askSize(Size),generate(Size,L),init(Size,L).

%pergunta o tamanho ao utilizador
askSize(Size):-write('Choose Size of Board'),nl,prcss_ans(2,50,Size).

%processa resposta do utilizador
prcss_ans(Min,Max,Ans):-read_line(X),lineToNumber(X,Ans),Ans>=Min,Ans=<Max.

%auxiliar para processar resposta do utilizador
lineToNumber([X1|X2],Ans):-lineToNumber(X2,Ans1),(Ans1 \= -1,Ans is (X1-48)*10+Ans1;Ans is X1-48).
lineToNumber([],-1).
