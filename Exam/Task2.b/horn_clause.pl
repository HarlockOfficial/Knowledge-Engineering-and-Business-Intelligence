:- use_module(library(lists))
:- ensure_loaded(knowledge_base)

% GRAPE and DISH are passed as lists
findwine(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, WINE) :- 
    country(COUNTRY), region(REGION, CAUNTRY), validgrape(GRAPE), validdish(DISH), 
    number(FRUITY), fruity(WINE, FRUITY), 
    number(BOLD), bold(WINE, BOLD), 
    number(SAVORY), savory(WINE, SAVORY),
    number(DRY), dry(WINE, DRY),
    number(TANNIN), tannin(WINE, TANNIN).

validgrape([]).
validgrape([H|T]):- grape(H), validgrape(T).

validdish([]).
validdish([H|T]):- dish(H), validdish(T).
