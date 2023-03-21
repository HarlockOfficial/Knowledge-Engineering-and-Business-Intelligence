% Knowledge Base
fibonacci(1, 1).
fibonacci(2, 1).

% Horn Clause
fibonacci(X, Y) :- X1 is X-1, X2 is X-2, fibonacci(X1, Y1), fibonacci(X2, Y2), Y is Y1 + Y2.

% Query
?- fibonacci(5, R)
