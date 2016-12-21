:- use_module(library(random)).
:- use_module(library(timeout)).

%cria uma matriz (SizexSize) vazia e devolve na Variavel List
createEmptyMatrix(Size,List):-createEmptyMatrixAux(Size,List,Size).

%predicado auxiliar para criar uma matrix vazia
createEmptyMatrixAux(Size,[NewList|List2],Counter):-Counter>0,Counter1 is Counter-1,length(NewList,Size),createEmptyMatrixAux(Size,List2,Counter1).
createEmptyMatrixAux(_,[],0).

:-dynamic matrixPos/1.
matrixPos([]).

%põe todas as posições do tabuleiro num facto dinâmico matrixPos
createMatrixPositions([L1|L2],X):-createMatrixPositionsLine(L1,X,1),X1 is X+1,createMatrixPositions(L2,X1).
createMatrixPositions([],_).

%auxiliar para preencher matrixPos
createMatrixPositionsLine([_|L2],X,Y):-matrixPos(List),append(List,[[X,Y]],NewList),retract(matrixPos(_)),asserta(matrixPos(NewList)),Y1 is Y+1,createMatrixPositionsLine(L2,X,Y1).
createMatrixPositionsLine([],_,_).

%devolve um puzzle aleatório em L, com o tamanho SizexSize
generate(Size,L):-createEmptyMatrix(Size,L),createMatrixPositions(L,1),createBoard(L,Size).

%vai a uma posição aleatória do tabuleiro para colocar lá um número
createBoard(L,Size):-matrixPos(List),length(List,Length),Length>0,Range is Length+1,MaxNumber is round(3*Size/4)+1,random(1,Range,Random),placeElement(L,Random,MaxNumber,Size),!,createBoard(L,Size).
createBoard(_,_).

%pede pra colocar um número random na posição escolhida anteriormente
placeElement(L,Element,MaxNumber,Size):-random(1,MaxNumber,Random),matrixPos(List),nth1(Element,List,RandomPos),!,tryToPlace(L,RandomPos,Random,Size).

%tenta colocar o número escolhido na posiçao escolhida e verifica se é possivel, se não fôr, reduz o número até ser impossível colocar na posição um número
tryToPlace(L,[X,Y],Random,Size):-Random>0,copy(L,LL),getElement(LL, X, Y, Value),Value=Random,(time_out(main(LL,Size), 100,TimeOut),TimeOut=success,!,getElement(L, X, Y, ValueReal),ValueReal=Random,matrixPos(ListPositions),delete(ListPositions,[X,Y],NewList),retract(matrixPos(_)),asserta(matrixPos(NewList))
;!,Random1 is Random -1,tryToPlace(L,[X,Y],Random1,Size)).

tryToPlace(_,[X,Y],0,_):-!,matrixPos(ListPositions),delete(ListPositions,[X,Y],NewList),retract(matrixPos(_)),asserta(matrixPos(NewList)).

%recebe uma matriz e devolve a mesma matrix com variaveis diferentes (números mantêm-se iguais)
copy([L1|L2],[NewList1|NewList2]):-copyLine(L1,NewList1),copy(L2,NewList2).
copy([],[]).

%preidicado auxiliar para copiar a linha de uma matriz
copyLine([L1|L2],[NewList1|NewList2]):-var(L1),NewList1=Var,copyLine(L2,NewList2).
copyLine([L1|L2],[NewList1|NewList2]):-NewList1=L1,copyLine(L2,NewList2).
copyLine([],[]).
