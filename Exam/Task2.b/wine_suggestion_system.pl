:- ensure_loaded(knowledge_base)
:- ensure_loaded(horn_clause)

% Queries

?- query([italy], [B], [merlot, cabernetfranc], D, E, F, G, H, [beef], [I], [marches], [K], [L], OUT).

?- {WINE1,WINE2}/(q2([italy], [REGION], [GRAPE], FRUITY, BOLD, SAVORY, C, TANNIN, [A], [NOTCOUNTRY], [NOTREGION], [NOTGRAPE], [NOTDISH], WINE1, WINE2)).

?- q2([italy], [_REGION], [_GRAPE], _FRUITY, _BOLD, _SAVORY, _C, _TANNIN, [_A], [_NOTCOUNTRY], [_NOTREGION], [_NOTGRAPE], [_NOTDISH], WINE1, WINE2).
