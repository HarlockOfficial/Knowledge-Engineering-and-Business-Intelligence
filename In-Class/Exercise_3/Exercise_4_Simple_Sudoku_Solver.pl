% Simple Sudoku Field
%  A1 | A2 | A3
%  B1 | B2 | B3
%  C1 | C2 | C3

% Knowledge Base
valid_number(1).
valid_number(2).
valid_number(3).

% Horn Clause
is_number(A1, A2, A3, B1, B2, B3, C1, C2, C3) :-
	% valid value
    valid_number(A1), valid_number(A2), valid_number(A3), 
    valid_number(B1), valid_number(B2), valid_number(B3), 
    valid_number(C1), valid_number(C2), valid_number(C3).

% number "not equal" definition depends on specific prolog dialect
different(A, B, C) :- A =\= B, A =\= C, B =\= C.

check_field(A1, A2, A3, B1, B2, B3, C1, C2, C3) :-
    % different rows    
    different(A1, A2, A3), different(B1, B2, B3), different(C1, C2, C3), 
    % different columns    
    different(A1, B1, C1), different(A2, B2, C2), different(A3, B3, C3).

  
solve(A1, A2, A3, B1, B2, B3, C1, C2, C3) :- 
    is_number(A1, A2, A3, B1, B2, B3, C1, C2, C3),
    check_field(A1, A2, A3, B1, B2, B3, C1, C2, C3).

% Query
?- solve(1, A2, 3, B1, 3, B3, 3, C2, 2)

