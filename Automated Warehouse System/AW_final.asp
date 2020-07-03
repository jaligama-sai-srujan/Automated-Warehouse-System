%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Variable %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(move(0,1);move(0,-1);move(1,0);move(-1,0);putdown; pickup; deliver).
robot(R, X, Y, 0) :- init(object(robot,R),value(at,pair(X,Y))).
node(N,X,Y) :- init(object(node,N),value(at,pair(X,Y))).
shelf(S, X, Y, 0) :- init(object(shelf,S),value(at,pair(X,Y))).
pickingStation(P, X ,Y) :- init(object(pickingStation,P),value(at,pair(X,Y))).
highway(X,Y) :- init(object(highway,_),value(at,pair(X,Y))).
order(O,I,U,P,0) :- init(object(order,O),value(line,pair(I,U))),init(object(order,O),value(pickingStation,P)).
product(P,S,U,0):-init(object(product,P),value(on,pair(S,U))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% State Constraint %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shelf(S,X,Y,T) :- hasShelf(R, S, t, T), robot(R,X,Y,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Effect and Preconditions of Action %%%%%%%%%%%%%%%%%%%%%%%%%


:- robot(R, X, Y, T), not node(_, X, Y).
%Robot Movement constraints
:- robot(R1, X, Y, T), robot(R2, X, Y, T), R1!=R2.
:- robot(R1, X1, Y1, T), robot(R2, X2, Y2, T), robot(R2, X1, Y1, T+1), robot(R1, X2, Y2, T+1), R1!=R2.
%Shelf Movement constraints
:- shelf(S1, X, Y, T), shelf(S2, X, Y, T), S1!=S2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Action %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


{task(R,A,T):action(A)}1:- robot(R,X,Y,T-1), T=1..m.
2{occur(object(robot ,R),move(DX,DY),T); robot(R,X+DX,Y+DY,T)}2:-task(R,move(DX,DY),T),robot(R,X,Y,T-1), T=1..m.
2{occur(object(robot ,R),pickup ,T) ; hasShelf(R,S,t,T)}2 :- task(R,pickup,T), robot(R,X,Y,T-1), shelf(S,X,Y,T-1),T=1..m.
2{occur(object(robot ,R),putdown ,T); hasShelf(R,S,f,T)}2 :- task(R,putdown,T), robot(R,X,Y,T-1), shelf(S,X,Y,T-1), T=1..m.
3{occur(object(robot ,R),deliver(O,I,UP),T); order(O,I,UO-UP,P,T);product(I,S,0,T)}3:-task(R,deliver,T), robot(R,X,Y,T-1), shelf(S,X,Y,T-1), pickingStation(P, X ,Y), order(O,I,UO,P,T-1), product(I, S, UP, T-1), UO>=UP, UO!=0, UP!=0, T=1..m.
3{occur(object(robot ,R),deliver(O,I,UO),T); order(O,I,0,P,T);product(I,S,UP-UO,T)}3:-task(R,deliver,T), robot(R,X,Y,T-1), shelf(S,X,Y,T-1), pickingStation(P, X ,Y), order(O,I,UO,P,T-1), product(I, S, UP, T-1), UO<UP, UO!=0, UP!=0, T=1..m.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Action Constraints  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-task(R,pickup,T), robot(R,X,Y,T-1),not shelf(_, X, Y, T-1).
:-task(R,pickup,T), robot(R,X,Y,T-1), hasShelf(R,_,t,T-1).
:-task(R,putdown,T), robot(R,X,Y,T-1),not shelf(_, X, Y, T-1).
:-task(R,putdown,T), robot(R,X,Y,T-1), shelf(S,X,Y,T-1),highway(X,Y).
:-task(R,putdown,T), robot(R,X,Y,T-1), hasShelf(R,S,f,T-1), shelf(S, X, Y, T-1).
:-task(R,deliver,T), robot(R,X,Y,T-1),not pickingStation(_, X ,Y).
:-task(R,deliver,T), robot(R,X,Y,T-1),not shelf(_, X, Y, T-1).
:-task(R,deliver,T), robot(R,X,Y,T-1), shelf(S, X, Y, T-1), hasShelf(R, S, f, T-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Fluent %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hasShelf(R, S, f, 0):- robot(R, X, Y, 0), shelf(S, X1, Y1, 0).

%uniqueness and existense of fluent values

:- not 1{hasShelf(R, S, BB, T)}1, hasShelf(R, S, _, T-1), T = 1..m.
:- not 1{shelf(S, _, _, T)}1, shelf(S,_, _, T-1), T = 1..m.
:- not 1{robot(R,_, _, T)}1, robot(R,_, _, T-1), T = 1..m.
:- not 1{order(O,I,_,P,T)}1, order(O,I,_,P,T-1), T = 1..m.
:- not 1{product(P,S,_,T)}1, product(P,S,_,T-1), T = 1..m.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Common Sense Law Of Intertia  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{robot(R, X, Y, T)} :- robot(R, X, Y, T-1), T=1..m.
{shelf(R, X, Y, T)} :- shelf(R, X, Y, T-1), T=1..m.
{hasShelf(R, S, BB, T)} :- hasShelf(R, S, BB, T-1), T=1..m.
{order(O,I,U,P,T)} :- order(O,I,U,P,T-1), T=1..m.
{product(P,S,U,T)} :- product(P,S,U,T-1), T=1..m.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Goal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- not order(O,I,0,_,_), order(O,I,_,P,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Show %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#show occur/3.




