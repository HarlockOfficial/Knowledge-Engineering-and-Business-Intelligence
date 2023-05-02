:- ensure_loaded(knowledge_base)
:- ensure_loaded(horn_clause)

% Queries
?- query([italy, spain], [sicily, catalunia], [merlot], 6, B, C, D, E, [beef], OUT).

