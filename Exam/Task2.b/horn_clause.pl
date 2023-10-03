:- use_module(library(lists)).
:- use_module(library(pairs)).
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

weakquery(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE) :-
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
    weakfinddish(X, DISH),
    weaknotfinddish(X, NOTDISH),
    winename(X, WINE).

q2(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, STRONG_WINE, WEAK_WINE) :-
    (
    query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, STRONG_WINE) ->
    	true
    ;
    	true
    ),
    (
    weakquery(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WEAK_WINE) ->
    	true
    ;
    	true
    ).

softened_query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE, LIKELIHOOD, THRESHOLD) :-
    LIKELIHOOD is THRESHOLD,
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

list_contains([E|_], E).
list_contains([_|T], E) :- list_contains(T, E).

for_element_in_list([], _, _, _).
for_element_in_list([H|T], LIKELIHOOD, CURRENT_LIST, OUTPUT_LIST) :-
    append_to_list((LIKELIHOOD, H), CURRENT_LIST, OUTPUT_LIST), for_element_in_list(T, LIKELIHOOD, CURRENT_LIST, OUTPUT_LIST).

append_to_list((LIKELIHOOD, WINE), INTERNAL_LIST, OUTPUT_LIST) :-
    is_list(WINE) ->
    	for_element_in_list(WINE, LIKELIHOOD, INTERNAL_LIST, OUTPUT_LIST)
    ;
    	not(list_contains(INTERNAL_LIST, WINE)) -> append(INTERNAL_LIST, [(LIKELIHOOD-WINE)], OUTPUT_LIST); true.

validgrape([]).
validgrape([H|T]):- grape(H), validgrape(T).

validdish([]).
validdish([H|T]):- food_contains(H, _), validdish(T).

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

notfinddish(WINE, [H|_]):- food_contains(H, INGREDIENT), ingredient(INGREDIENT), not(wineusedwith(WINE, INGREDIENT)).
notfinddish(WINE, [_|T]):- notfinddish(WINE,T).

finddish(WINE, [H|_]):- food_contains(H, INGREDIENT), ingredient(INGREDIENT), wineusedwith(WINE, INGREDIENT).
finddish(WINE, [_|T]):- finddish(WINE, T).

weaknotfinddish(WINE, [H|_]):- food_contains(H, INGREDIENT), ingredient(INGREDIENT), not(weakwineusedwith(WINE, INGREDIENT)).
weaknotfinddish(WINE, [_|T]):- weaknotfinddish(WINE,T).

weakfinddish(WINE, [H|_]):- food_contains(H, INGREDIENT), ingredient(INGREDIENT), weakwineusedwith(WINE, INGREDIENT).
weakfinddish(WINE, [_|T]):- weakfinddish(WINE, T).
