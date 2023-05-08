:- use_module(library(lists)).
:- ensure_loaded(knowledge_base).

% COUNTRY, REGION, GRAPE and DISH are passed as lists
query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, WINE) :- 
    validcountry(COUNTRY), 
    validregion(REGION, COUNTRY), 
    validgrape(GRAPE), 
    validdish(DISH),  
    fruity(X, FRUITY), 
    bold(X, BOLD), 
    savory(X, SAVORY),
    dry(X, DRY),
    tannin(X, TANNIN),
    findwinebelongs(X, REGION),
    findwine(X, GRAPE),
    finddish(X, DISH),
    winename(X, WINE).


softened_query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, WINE, THRESHOLD) :-
    validcountry(COUNTRY), 
    validregion(REGION, COUNTRY), 
    validgrape(GRAPE), 
    validdish(DISH),  
    fruity(X, Y), Y - THRESHOLD =< FRUITY, FRUITY =< Y + THRESHOLD, 
    bold(X, Z), Z - THRESHOLD =< BOLD, BOLD =< Z + THRESHOLD,
    savory(X, W), W - THRESHOLD =< SAVORY, SAVORY =< W + THRESHOLD,
    dry(X, H), H - THRESHOLD =< DRY, DRY =< H + THRESHOLD,
    tannin(X, K), K - THRESHOLD =< TANNIN, TANNIN =< K + THRESHOLD,
    findwinebelongs(X, REGION),
    findwine(X, GRAPE),
    finddish(X, DISH),
    winename(X, WINE).


validgrape([]).
validgrape([H|T]):- grape(H), validgrape(T).

validdish([]).
validdish([H|T]):- dish(H), validdish(T).

validcountry([]).
validcountry([H|T]) :- country(H), validcountry(T).

validregion([], []).
validregion([], COUNTRYLIST).
validregion(REGIONLIST, []).
validregion([H|T], [A|B]):- region(H, A), validregion(T, B).
validregion([H|T], [A|B]):- region(H, A), validregion(H, B).
validregion([H|T], [A|B]):- region(H, A), validregion(T, A).


findwinebelongs(WINE, [H|T]):- winebelongs(WINE, H).
findwinebelongs(WINE, [H|T]):- findwinebelongs(WINE, T).

findwine(WINE, []).
findwine(WINE, [H|T]):- containsgrape(WINE, H), findwine(WINE, T).

finddish(WINE, [H|T]):- wineusedwith(WINE, H).
finddish(WINE, [H|T]):- finddish(WINE, T).
