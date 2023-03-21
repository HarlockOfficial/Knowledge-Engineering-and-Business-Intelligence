% Knowledge Base
arc(a, b).
arc(a, c).
arc(b, e).
arc(b, f).
arc(b, c).
arc(a, d).
arc(e, f).
arc(f, g).

% Horn Clause
path(X, Y) :- arc(X, Y).
path(X, Z) :- arc(X, Y), path(Y,Z).

path(X, Y, R) :- arc(X, Y), R = [X, Y].
path(X, Z, R) :- arc(X, Y), path(Y, Z, R1), R = [X | R1].

% Queries
?- path(a, g)
?- path(a, g, R)

