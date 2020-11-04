testBoard([[0,1,1,0,0,1],
	[0,1,1,1,0,1],
	[0,1,0,0,2,1],
	[1,2,0,2,1,2],
	[1,0,2,0,0,1],
	[1,0,2,0,1,0],
	[2,2,0,2,2,2]]).

board([[],[],[],[],[],[],[]]).

case(X,Y,Board,Val):- nth0(X,Board,Col), nth0(Y,Col,Val).

%Victoire sur colonnes
colOf4(Sub,Main):-prefix(Sub,Main).
colOf4(Sub,[_|L]):-colOf4(Sub,L).
winCol([H|_], Val):- colOf4([Val,Val,Val,Val],H),!.
winCol([_|L],Val):- winCol(L,Val).

%Victoire sur lignes
linOf4(Val,Board,Y):-case(0,Y,Board,A),case(1,Y,Board,B),case(2,Y,Board,C),case(3,Y,Board,D),Val=A,A==B,B==C,C==D.
linOf4(Val,Board, Y):- Y1 is Y+1, Y1<7, linOf4(Val,Board,Y1).
winLin(Board, Val):-linOf4(Val,Board,0).
winLin([_|L], Val):- winLin(L,Val),!.

%Victoire sur diag1
diag1Of4(Val,Board,Y):-Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,case(0,Y,Board,A),case(1,Y1,Board,B),case(2,Y2,Board,C),case(3,Y3,Board,D),Val=A,A==B,B==C,C==D.
diag1Of4(Val,Board, Y):- Y1 is Y+1, Y1<4, diag1Of4(Val,Board,Y1).
winDiag1(Board, Val):-diag1Of4(Val,Board,0).
winDiag1([_|L], Val):- winDiag1(L,Val).

%Victoire sur diag2
diag2Of4(Val,Board,Y):-Y1 is Y-1, Y2 is Y-2, Y3 is Y-3,case(0,Y,Board,A),case(1,Y1,Board,B),case(2,Y2,Board,C),case(3,Y3,Board,D),Val=A,A==B,B==C,C==D.
diag2Of4(Val,Board, Y):- Y1 is Y+1, Y1<7, diag2Of4(Val,Board,Y1).
winDiag2(Board, Val):-diag2Of4(Val,Board,3).
winDiag2([_|L], Val):- winDiag2(L,Val).

%Victoire
victory(Board,Val) :- winCol(Board,Val).
victory(Board,Val) :- winLin(Board,Val).
victory(Board,Val) :- winDiag1(Board,Val) ; winDiag2(Board,Val).

%Ajout en fin de liste
appendToList(X,[],[X]).
appendToList(X,[H|L1],[H|L2]):-appendToList(X,L1,L2).

%Verif Coup valide
legalMove(Board, N):-nth0(N,Board,Col),length(Col,Size),Size<6.

% teste si le jeu nest pas bloqué
unstuckGame(Board,N) :- legalMove(Board,N).
unstuckGame(Board,N) :- N1 is N+1, N1<7, unstuckGame(Board,N1).

% teste si le jeu est bloqué
stuckGame(Board,N) :- not(unstuckGame(Board,N)).

%Joueur humain
playHuman(Board,Coin, NewBoard) :- write('\n'), write(Board),
		write("\nOn what column do you want to play ?"), read(NCol),
		legalMove(Board,NCol), move(Board,Coin,NCol,NewBoard).

playHuman(Board,Coin, NewBoard) :- write("Unauthorized move, please select another column !"),
		playHuman(Board,Coin, NewBoard).

%Jouer avec 2 joueurs Humains
playRound(Board, _, Board, _) :- victory(Board, "x"),write("\nJ1 won").
playRound(Board, _, Board, _) :- victory(Board, "o"),write("\nJ2 won").
playRound(Board, "x", NewBoard, Window) :- drawBoard(Window,Board,0), playHuman(Board, "x", X), playRound(X,"o",NewBoard,Window).
playRound(Board, "o", NewBoard, Window) :- drawBoard(Window,Board,0), playHuman(Board, "o", X), playRound(X,"x",NewBoard,Window).



%Lancer partie 2 humains
humanGame():-board(X),new(Window, picture("Puissance 4")),send(Window,open),playRound(X, "x", NewBoard, Window),write("\nGame ended"),write(NewBoard), send(Window,destroy).

%Afficher le board dans window
drawBoard(Window, _,7):-send(Window,flush).
drawBoard(Window,[H|L],Col):-drawCol(Window, H,Col,0), Col1 is Col+1,drawBoard(Window, L,Col1).
drawCol(_,[],_, _).
drawCol(Window,[H|L],Col,Lin):- send(Window,display,text(H),point((Col+1)*10,80-(Lin+1)*10)), Lin1 is Lin+1, drawCol(Window,L,Col,Lin1).



%Choix IA
aiChoice(AIType):- write('\n'),
	write("\nChoose IA type :"),
	write("\n1- Random"),
	write("\n"),
	write("\n2 - Heuristic 1 (bonne défense, mauvaise attaque"),
	write("\n"),
		write("\n3 - Heuristic 2"),
		write("\n"),
	write("\n4 - Heuristic3"),
	write("\n"),
	read(AIType).

%IA 1 - jouer coup aleatoire
playAI(Board,Coin,1,NewBoard):- random(0,7,NCol),
		write("\nAI "), write(Coin), write(" played in col "),
		write(NCol),
		legalMove(Board,NCol),
		move(Board,Coin,NCol,NewBoard).
playAI(Board,Coin,1,NewBoard):- playAI(Board,Coin,1,NewBoard) .
playAI(Board,Coin,2,NewBoard):- iaMinMax(Board,NewBoard,4,_,Coin,2).
playAI(Board,Coin,3,NewBoard):- iaMinMax(Board,NewBoard,4,_,Coin,3).
playAI(Board,Coin,4,NewBoard):- iaMinMax(Board,NewBoard,4,_,Coin,4).

%Jouer contre une IA
playRoundAI(Board, _, _, Board,_) :- victory(Board, "x"),write("\nPlayer won").
playRoundAI(Board, _, _, Board,_) :- victory(Board, "o"),write("\nAI won").
playRoundAI(Board, _, _, Board,_) :- stuckGame(Board,0),write("\nGame is stuck no one wins").
playRoundAI(Board, "x", AIType, NewBoard,Window) :- playHuman(Board, "x", X),drawBoard(Window,X,0), playRoundAI(X,"o", AIType, NewBoard,Window).
playRoundAI(Board, "o", AIType, NewBoard,Window) :- playAI(Board, "o", AIType, X),drawBoard(Window,X,0), playRoundAI(X,"x", AIType,NewBoard,Window).

%Lancer partie contre IA
againstAIGame():-board(X),new(Window, picture("Puissance 4")),send(Window,open), aiChoice(AIType), playRoundAI(X, "x", AIType, NewBoard,Window),write("\nGame ended"),write(NewBoard), send(Window,destroy).

%IA vs IA
playAIvsAI(Board, _, _, _, Board, _) :- victory(Board, "x"),write("\nAI 1 won").
playAIvsAI(Board, _, _, _, Board, _) :- victory(Board, "o"),write("\nAI 2 won").
playAIvsAI(Board, "x", AIType1, AIType2, NewBoard, Window) :- playAI(Board, "x", AIType1, X),
				drawBoard(Window,X,0),
				playAIvsAI(X, "o", AIType1, AIType2, NewBoard, Window).
playAIvsAI(Board, "o", AIType1, AIType2, NewBoard, Window) :- playAI(Board, "o", AIType2, X),
				drawBoard(Window,X,0),
				playAIvsAI(X, "x", AIType1, AIType2, NewBoard, Window).



%Lancer partie IA vs IA
aiVSaiGame():-board(X),new(Window, picture("Puissance 4")),send(Window,open),write("\nIA 1 -"), aiChoice(AIType1),write("\nIA 2 -"), aiChoice(AIType2), playAIvsAI(X,"x",AIType1,AIType2,NewBoard, Window),write("\nGame ended"),write(NewBoard), send(Window,destroy).

%anciennement appelé PlayCoin
moveCol(Col,Player,NewCol):-append(Col,[Player],NewCol).
insererInList(List,Indice,Element,NewList):-removeNth(List,Indice,RemovedList),nth0(Indice,NewList,Element,RemovedList).
removeNth(List,Index,NewList):-nth0(Index,List,_,NewList).
move(Board,Player,IndexCol,NewBoard):-legalMove(Board,IndexCol),nth0(IndexCol,Board,MovedCol),moveCol(MovedCol,Player,NewCol),insererInList(Board,IndexCol,NewCol,NewBoard).


%vrai si Board est l’enfant de parent
isChild(Board,Parent,PlayerTurn):-move(Parent,PlayerTurn,_,Board).

%vrai si ChildrenList est la liste de tous les enfants de board
allChildren(Board,ChildrenList,PlayerTurn):-findall(ChildBoard,isChild(ChildBoard,Board,PlayerTurn),ChildrenList).

%Fonction heurisitique qui détermine l'intérêt d'une situation
heuristic1(Board,Score):-(victory(Board,"o"),Score=50);(not(victory(Board,"o")),not(victory(Board,"x")),Score=20);((not(victory(Board,"o")),victory(Board,"x"),Score=0)),!.
heuristic2(Board, Depth, Score) :- victory(Board, "o"), Score is (60 + Depth * 50).
heuristic2(Board, Depth, Score) :- victory(Board, "x"), Score is (40 - Depth*10).
heuristic2(_, _, Score) :- Score is 50.

heuristic3(Board,NewNewScore):-((victory(Board,"o"),Score is 50);(not(victory(Board,"o")),not(victory(Board,"x")),Score is 20);(not(victory(Board,"o")),victory(Board,"x"),Score is 0)),(nth0(3,Board,ColMilieu),count(ColMilieu,"o",NumberAiCoin),NewScore is (NumberAiCoin+Score),count(ColMilieu,"x",NumberXcoin),NewNewScore is (NewScore-NumberXcoin)).




% ValuesList c’est la liste de tout les valeurs des minmax de chaque board de la list [T|Q]
%[T|Q] c’est la ChildrenList
allMinMaxValues([T|Q],ValuesList,Depth,Player,Type):-iaMinMax(T,_,Depth,HeuristicValue,Player,Type),allMinMaxValues(Q,NewValuesList,Depth,Player,Type),append([HeuristicValue],NewValuesList,ValuesList),!.
allMinMaxValues([],_,_,_,_).

%Vérifie si le jeu est en état terminal
terminalNode(Board) :- victory(Board,_); stuckGame(Board,0).

%min max
iaMinMax(Board,NewBoard,Depth,HeuristicValue,"o",Type):-Depth =\=0,not(terminalNode(Board)),allChildren(Board,ChildrenList,"o"),NewDepth is Depth-1 ,allMinMaxValues(ChildrenList,AllMinMaxValues,NewDepth,"x",Type),!,max_list(AllMinMaxValues,HeuristicValue),nth0(IndiceMax,AllMinMaxValues,HeuristicValue),nth0(IndiceMax,ChildrenList,NewBoard).

iaMinMax(Board,NewBoard,Depth,HeuristicValue,"x",Type):-Depth =\=0,not(terminalNode(Board)),allChildren(Board,ChildrenList,"x"),NewDepth is Depth-1 ,allMinMaxValues(ChildrenList,AllMinMaxValues,NewDepth,"o",Type),!,min_list(AllMinMaxValues,HeuristicValue),nth0(IndiceMax,AllMinMaxValues,HeuristicValue),nth0(IndiceMax,ChildrenList,NewBoard).

%Attention, "o" et "x" ecrits en dur probleme pour IA vs IA !
iaMinMax(Board,NewBoard,Depth,HeuristicValue,_,Type):-terminalNode(Board), (Type is 2, heuristic1(Board,HeuristicValue));(Type is 3, heuristic2(Board,Depth,HeuristicValue));(Type is 4 ,heuristic3(Board,HeuristicValue)),NewBoard=Board,!.

%min max base case
iaMinMax(Board,NewBoard,0,HeuristicValue,_,Type):- (Type is 2, heuristic1(Board,HeuristicValue));(Type is 3, heuristic2(Board,0,HeuristicValue));(Type is 4 ,heuristic3(Board,HeuristicValue)),NewBoard=Board,!.

%Number c'est le nombre doccurence de Player dans Col
count([T|Q],Player,NewNumber):-(count(Q,Player,Number),T==Player,NewNumber is Number+1);(count(Q,Player,Number),T=\=Player,NewNumber=Number).
count([],_,Number):-Number=0.

%start menu
start():-write("Lancer une partie : \n"),
	write("1 - joueur contre joueur\n"),
	write("2 - joueur contre ia\n"),
	write("3 - ia contre ia\n"),
	read(T),
	launchGame(T).

launchGame(1):-humanGame().
launchGame(2):-againstAIGame().
launchGame(3):-aiVSaiGame().
