% Knowledge base
human(john).
human(paul).
human(mary).

male(john).
male(paul).

female(mary).

father(john, mary).

mother(mary, paul).

% Horn Clauses

man(X) :- human(X), male(X).
woman(X) :- human(X), female(X).

parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- ancestor(X, Y), parent(Y, Z).

