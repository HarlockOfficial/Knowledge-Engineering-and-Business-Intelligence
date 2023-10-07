:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(yall)).
:- use_module(library(solution_sequences)).
:- ensure_loaded(knowledge_base).

% COUNTRY, REGION, GRAPE and DISH are passed as lists
query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE) :-
    print("Called Query With Params: "), print(COUNTRY), print(REGION), print(GRAPE), print(FRUITY), print(BOLD), print(SAVORY), print(DRY), print(TANNIN), print(DISH), print(NOTCOUNTRY), print(NOTREGION), print(NOTGRAPE), print(NOTDISH), nl,
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
    winename(X, WINE),
    (
      not(atom(WINE)) ->
      (
        print("No Result")
      )
      ;
      (
      	true
      )
    ),
    winename(X, WINE).

weakquery(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE) :-
    print("Called Weak Query With Params: "), print(COUNTRY), print(REGION), print(GRAPE), print(FRUITY), print(BOLD), print(SAVORY), print(DRY), print(TANNIN), print(DISH), print(NOTCOUNTRY), print(NOTREGION), print(NOTGRAPE), print(NOTDISH), nl,
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
    winename(X, WINE),
    (
      not(atom(WINE)) ->
      (
      	print("No Weak Result")
      )
      ;
      (
      	true
      )
    ),
    winename(X, WINE).

query2(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, STRONG_WINE, WEAK_WINE) :-
    print("Called Q2 With Params: "), print(COUNTRY), print(REGION), print(GRAPE), print(FRUITY), print(BOLD), print(SAVORY), print(DRY), print(TANNIN), print(DISH), print(NOTCOUNTRY), print(NOTREGION), print(NOTGRAPE), print(NOTDISH), nl,
    duplicate_term(COUNTRY, COUNTRY_ORIG),
    duplicate_term(REGION, REGION_ORIG),
    duplicate_term(GRAPE, GRAPE_ORIG),
    duplicate_term(FRUITY, FRUITY_ORIG),
    duplicate_term(BOLD, BOLD_ORIG),
    duplicate_term(SAVORY, SAVORY_ORIG),
    duplicate_term(DRY, DRY_ORIG),
    duplicate_term(TANNIN, TANNIN_ORIG),
    duplicate_term(DISH, DISH_ORIG),
    duplicate_term(NOTCOUNTRY, NOTCOUNTRY_ORIG),
    duplicate_term(NOTREGION, NOTREGION_ORIG),
    duplicate_term(NOTGRAPE, NOTGRAPE_ORIG),
    duplicate_term(NOTDISH, NOTDISH_ORIG),
    setof(WINE1, query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, WINE1), STRONG_WINE),
    setof(WINE2, weakquery(COUNTRY_ORIG, REGION_ORIG, GRAPE_ORIG, FRUITY_ORIG, BOLD_ORIG, SAVORY_ORIG, DRY_ORIG, TANNIN_ORIG, DISH_ORIG, NOTCOUNTRY_ORIG, NOTREGION_ORIG, NOTGRAPE_ORIG, NOTDISH_ORIG, WINE2), WEAK_WINE).

query3(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, STRONG_WINE, WEAK_WINE) :-
    print("Called Q3 With Params: "), print(COUNTRY), print(REGION), print(GRAPE), print(FRUITY), print(BOLD), print(SAVORY), print(DRY), print(TANNIN), print(DISH), print(NOTCOUNTRY), print(NOTREGION), print(NOTGRAPE), print(NOTDISH), nl,
    duplicate_term(COUNTRY, COUNTRY_ORIG),
    duplicate_term(REGION, REGION_ORIG),
    duplicate_term(GRAPE, GRAPE_ORIG),
    duplicate_term(FRUITY, FRUITY_ORIG),
    duplicate_term(BOLD, BOLD_ORIG),
    duplicate_term(SAVORY, SAVORY_ORIG),
    duplicate_term(DRY, DRY_ORIG),
    duplicate_term(TANNIN, TANNIN_ORIG),
    duplicate_term(DISH, DISH_ORIG),
    duplicate_term(NOTCOUNTRY, NOTCOUNTRY_ORIG),
    duplicate_term(NOTREGION, NOTREGION_ORIG),
    duplicate_term(NOTGRAPE, NOTGRAPE_ORIG),
    duplicate_term(NOTDISH, NOTDISH_ORIG),
    distinct(query(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, STRONG_WINE)),
    distinct(weakquery(COUNTRY_ORIG, REGION_ORIG, GRAPE_ORIG, FRUITY_ORIG, BOLD_ORIG, SAVORY_ORIG, DRY_ORIG, TANNIN_ORIG, DISH_ORIG, NOTCOUNTRY_ORIG, NOTREGION_ORIG, NOTGRAPE_ORIG, NOTDISH_ORIG, WEAK_WINE)).

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
