:- use_module(library(lists)).
:- ensure_loaded(knowledge_base).

% COUNTRY, REGION, GRAPE and DISH are passed as lists
query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE) :-
    validcountry(COUNTRY),
    validcountry(NOTCOUNTRY),
    validregion(REGION, COUNTRY),
    validregion(NOTREGION, NOTCOUNTRY),
    validgrape(GRAPE),
    validgrape(NOTGRAPE),
    validdish(DISH),
    validdish(NOTDISH),
    fruity(X, FRUITY),
    bold(X, BOLD),
    savory(X, SAVORY),
    dry(X, DRY),
    tannin(X, TANNIN),
    findwinebelongs(X, REGION),
    findwinenotbelongs(X, NOTREGION),
    findwine(X, GRAPE),
    notfindwine(X, NOTGRAPE),
    finddish(X, DISH),
    notfinddish(X, NOTDISH),
    winename(X, WINE).


softened_query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE, THRESHOLD) :-
    validcountry(COUNTRY),
    validcountry(NOTCOUNTRY),
    validregion(REGION, COUNTRY),
    validregion(NOTREGION, NOTCOUNTRY),
    validgrape(GRAPE),
    validgrape(NOTGRAPE),
    validdish(DISH),
    validdish(NOTDISH),
    fruity(X, Y), Y - THRESHOLD =< FRUITY, FRUITY =< Y + THRESHOLD,
    bold(X, Z), Z - THRESHOLD =< BOLD, BOLD =< Z + THRESHOLD,
    savory(X, W), W - THRESHOLD =< SAVORY, SAVORY =< W + THRESHOLD,
    dry(X, H), H - THRESHOLD =< DRY, DRY =< H + THRESHOLD,
    tannin(X, K), K - THRESHOLD =< TANNIN, TANNIN =< K + THRESHOLD,
    findwinebelongs(X, REGION),
    findwinenotbelongs(X, NOTREGION),
    findwine(X, GRAPE),
    notfindwine(X, NOTGRAPE),
    finddish(X, DISH),
    notfinddish(X, NOTDISH),
    winename(X, WINE).

validgrape([]).
validgrape([H|T]):- grape(H), validgrape(T).

validdish([]).
validdish([H|T]):- dish(H), validdish(T).

validcountry([]).
validcountry([H|T]) :- country(H), validcountry(T).

validregion([], []).
validregion([], _).
validregion(_, []).
validregion([H|T], [A|B]):- region(H, A), validregion(T, B).
validregion([H|_], [A|B]):- region(H, A), validregion(H, B).
validregion([H|T], [A|_]):- region(H, A), validregion(T, A).

findwinenotbelongs(_, []).
findwinenotbelongs(WINE, [H|T]):- not(winebelongs(WINE, H)), findwinenotbelongs(WINE, T).

findwinebelongs(WINE, [H|_]):- winebelongs(WINE, H).
findwinebelongs(WINE, [_|T]):- findwinebelongs(WINE, T).

notfindwine(_, []).
notfindwine(WINE, [H|T]):- not(containsgrape(WINE,H)), notfindwine(WINE,T).

findwine(_, []).
findwine(WINE, [H|T]):- containsgrape(WINE, H), findwine(WINE, T).

notfinddish(WINE, [H|_]):- not(wineusedwith(WINE, H)).
notfinddish(WINE, [_|T]):- notfinddish(WINE,T).

finddish(WINE, [H|_]):- wineusedwith(WINE, H).
finddish(WINE, [_|T]):- finddish(WINE, T).
