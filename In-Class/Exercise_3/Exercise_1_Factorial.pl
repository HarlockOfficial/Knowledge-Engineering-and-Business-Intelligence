% Knowledge Base
fact(0, 1).
fact(1, 1).

% Horn Clause
fact(N, R) :- N1 is N-1, fact(N1, R1), R is N * R1.

% Query
?- fact(5, R) 
