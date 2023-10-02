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

merge_lists([], List2, List2).
merge_lists(LIST1, [], LIST1).
merge_lists([Head|Tail], List2, [Head|MergedTail]) :-
    merge_lists(Tail, List2, MergedTail).

sort_by_likelihood(LIST_INPUT, LIST_OUTPUT):-
	keysort(LIST_INPUT, LIST_OUTPUT).


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

notfinddish(WINE, [H|_]):- food_contains(H, INGREDIENT), not(wineusedwith(WINE, INGREDIENT)).
notfinddish(WINE, [_|T]):- notfinddish(WINE,T).

finddish(WINE, [H|_]):- food_contains(H, INGREDIENT), wineusedwith(WINE, INGREDIENT).
finddish(WINE, [_|T]):- finddish(WINE, T).


unbound_list([H|T]):- var(H), length(T, LEN),
    (
    	LEN > 0 ->
    		unbound_list(T)
    	;
    	true
    ).

compute_new_parameters(COUNTRY, REGION, GRAPE, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, NEW_COUNTRY, NEW_REGION, NEW_GRAPE, NEW_DISH, NEW_NOTCOUNTRY, NEW_NOTREGION, NEW_NOTGRAPE, NEW_NOTDISH, CHANGED) :-
    (
        (
        	is_list(NOTREGION), not(unbound_list(NOTREGION)) ->
        	(
            	length(NOTREGION, NOTREGIONLENGTH)
            )
        	;
        	(
            	NOTREGIONLENGTH = 0
            )
        ),
        (not(var(NOTREGION)), NOTREGIONLENGTH > 0) ->
        (
            NEW_COUNTRY = COUNTRY,
            NEW_REGION = REGION,
            NEW_GRAPE = GRAPE,
            NEW_DISH = DISH,
            NEW_NOTCOUNTRY = NOTCOUNTRY,
            NEW_NOTREGION = [],
            NEW_NOTGRAPE = NOTGRAPE,
            NEW_NOTDISH = NOTDISH,
            CHANGED = 1
        )
        ;
        (
            (
                is_list(REGION), not(unbound_list(REGION)) ->
                (
                    length(REGION, REGIONLENGTH)
                )
                ;
                (
                    REGIONLENGTH = 0
                )
            ),
            (not(var(REGION)), REGIONLENGTH > 0)->
            (
                NEW_COUNTRY = COUNTRY,
                NEW_REGION = [],
                NEW_GRAPE = GRAPE,
                NEW_DISH = DISH,
                NEW_NOTCOUNTRY = NOTCOUNTRY,
                NEW_NOTREGION = NOTREGION,
                NEW_NOTGRAPE = NOTGRAPE,
                NEW_NOTDISH = NOTDISH,
                CHANGED = 1
            )
            ;
            (
                (
                    is_list(NOTDISH), not(unbound_list(NOTDISH)) ->
                    (
                        length(NOTDISH, NOTDISHLENGTH)
                    )
                    ;
                    (
                        NOTDISHLENGTH = 0
                    )
                ),
                (not(var(NOTDISH)), NOTDISHLENGTH > 0) ->
                (
                    NEW_COUNTRY = COUNTRY,
                    NEW_REGION = REGION,
                    NEW_GRAPE = GRAPE,
                    NEW_DISH = DISH,
                    NEW_NOTCOUNTRY = NOTCOUNTRY,
                    NEW_NOTREGION = NOTREGION,
                    NEW_NOTGRAPE = NOTGRAPE,
                    NEW_NOTDISH = [],
                    CHANGED = 1
                )
                ;
                (
                    (
                        is_list(DISH), not(unbound_list(DISH)) ->
                        (
                            length(DISH, DISHLENGTH)
                        )
                        ;
                        (
                            DISHLENGTH = 0
                        )
                    ),
                    (not(var(DISH)), DISHLENGTH > 0) ->
                    (
                        NEW_COUNTRY = COUNTRY,
                        NEW_REGION = REGION,
                        NEW_GRAPE = GRAPE,
                        NEW_DISH = [],
                        NEW_NOTCOUNTRY = NOTCOUNTRY,
                        NEW_NOTREGION = NOTREGION,
                        NEW_NOTGRAPE = NOTGRAPE,
                        NEW_NOTDISH = NOTDISH,
                        CHANGED = 1
                    )
                    ;
                    (
                        (
                            is_list(NOTCOUNTRY), not(unbound_list(NOTCOUNTRY)) ->
                            (
                                length(NOTCOUNTRY, NOTCOUNTRYLENGTH)
                            )
                            ;
                            (
                                NOTCOUNTRYLENGTH = 0
                            )
                        ),
                        (not(var(NOTCOUNTRY)), NOTCOUNTRYLENGTH > 0) ->
                        (
                            NEW_COUNTRY = COUNTRY,
                            NEW_REGION = REGION,
                            NEW_GRAPE = GRAPE,
                            NEW_DISH = DISH,
                            NEW_NOTCOUNTRY = [],
                            NEW_NOTREGION = NOTREGION,
                            NEW_NOTGRAPE = NOTGRAPE,
                            NEW_NOTDISH = NOTDISH,
                            CHANGED = 1
                        )
                        ;
                        (
                            (
                                is_list(COUNTRY), not(unbound_list(COUNTRY)) ->
                                (
                                    length(COUNTRY, COUNTRYLENGTH)
                                )
                                ;
                                (
                                    COUNTRYLENGTH = 0
                                )
                            ),
                            (not(var(COUNTRY)), COUNTRYLENGTH > 0) ->
                            (
                                NEW_COUNTRY = [],
                                NEW_REGION = REGION,
                                NEW_GRAPE = GRAPE,
                                NEW_DISH = DISH,
                                NEW_NOTCOUNTRY = NOTCOUNTRY,
                                NEW_NOTREGION = NOTREGION,
                                NEW_NOTGRAPE = NOTGRAPE,
                                NEW_NOTDISH = NOTDISH,
                                CHANGED = 1
                            )
                            ;
                            (
                                (
                                    is_list(NOTGRAPE), not(unbound_list(NOTGRAPE)) ->
                                    (
                                        length(NOTGRAPE, NOTGRAPELENGTH)
                                    )
                                    ;
                                    (
                                        NOTGRAPELENGTH = 0
                                    )
                                ),
                                (not(var(NOTGRAPE)), NOTGRAPELENGTH > 0) ->
                                (
                                    NEW_COUNTRY = COUNTRY,
                                    NEW_REGION = REGION,
                                    NEW_GRAPE = GRAPE,
                                    NEW_DISH = DISH,
                                    NEW_NOTCOUNTRY = NOTCOUNTRY,
                                    NEW_NOTREGION = NOTREGION,
                                    NEW_NOTGRAPE = [],
                                    NEW_NOTDISH = NOTDISH,
                                    CHANGED = 1
                                )
                                ;
                                (
                                    (
                                        is_list(GRAPE), not(unbound_list(GRAPE)) ->
                                        (
                                            length(GRAPE, GRAPELENGTH)
                                        )
                                        ;
                                        (
                                            GRAPELENGTH = 0
                                        )
                                    ),
                                    (not(var(GRAPE)), GRAPELENGTH > 0) ->
                                    (
                                        NEW_COUNTRY = COUNTRY,
                                        NEW_REGION = REGION,
                                        NEW_GRAPE = [],
                                        NEW_DISH = DISH,
                                        NEW_NOTCOUNTRY = NOTCOUNTRY,
                                        NEW_NOTREGION = NOTREGION,
                                        NEW_NOTGRAPE = NOTGRAPE,
                                        NEW_NOTDISH = NOTDISH,
                                        CHANGED = 1
                                    )
                                    ;
                                    (
                                        CHANGED = 0
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ).

main(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, THRESHOLD, OLD_SORTED_OUT, FINAL_SORTED_OUT, ACCUMULATOR) :-
    print("Search started, please wait\n"),
    (
        var(THRESHOLD) ->
        (
            THRESHOLD = 0,
            ACCUMULATOR = 0,
            main(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, THRESHOLD, OLD_SORTED_OUT, FINAL_SORTED_OUT, ACCUMULATOR),
            print("Final result: "),print(FINAL_SORTED_OUT),print("\n")
        )
        ;
        (
        	duplicate_term(COUNTRY, CURR_COUNTRY),
            duplicate_term(REGION, CURR_REGION),
            duplicate_term(GRAPE, CURR_GRAPE),
            duplicate_term(FRUITY, CURR_FRUITY),
            duplicate_term(BOLD, CURR_BOLD),
            duplicate_term(SAVORY, CURR_SAVORY),
            duplicate_term(DRY, CURR_DRY),
            duplicate_term(TANNIN, CURR_TANNIN),
            duplicate_term(DISH, CURR_DISH),
            duplicate_term(NOTCOUNTRY, CURR_NOTCOUNTRY),
            duplicate_term(NOTREGION, CURR_NOTREGION),
            duplicate_term(NOTGRAPE, CURR_NOTGRAPE),
            duplicate_term(NOTDISH, CURR_NOTDISH),
            duplicate_term(THRESHOLD, CURR_THRESHOLD),
            setof(ACCUMULATOR-(LIKELIHOOD-WINE), softened_query(CURR_COUNTRY, CURR_REGION, CURR_GRAPE, CURR_FRUITY, CURR_BOLD, CURR_SAVORY, CURR_DRY, CURR_TANNIN, CURR_DISH, CURR_NOTCOUNTRY, CURR_NOTREGION, CURR_NOTGRAPE, CURR_NOTDISH, WINE, LIKELIHOOD, CURR_THRESHOLD), NEW_INTERNAL_LIST),
            % print("Threshold: "),print(THRESHOLD),print(" Search Result: "),print(NEW_INTERNAL_LIST),print("\n"),
        	length(NEW_INTERNAL_LIST, NEW_INTERNAL_LIST_LEN),
            NEW_INTERNAL_LIST_LEN > 0 ->
            (
                sort_by_likelihood(NEW_INTERNAL_LIST, NEW_SORTED_OUT),
                merge_lists(OLD_SORTED_OUT, NEW_SORTED_OUT, TMP_FINAL_SORTED_OUT),
            	THRESHOLD_NEW is THRESHOLD + 1,
                (
                    THRESHOLD_NEW < 5 ->
                	(
                      main(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, THRESHOLD_NEW, OLD_SORTED_OUT, OUT, ACCUMULATOR),
                      merge_lists(TMP_FINAL_SORTED_OUT, OUT, NEW_OUT),
                      sort_by_likelihood(NEW_OUT, FINAL_SORTED_OUT),
                      print("Current List before returning (after recursive call): "),print(FINAL_SORTED_OUT),print("\n")
                    )
                	;
                	(
                	    compute_new_parameters(COUNTRY, REGION, GRAPE, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, NEW_COUNTRY, NEW_REGION, NEW_GRAPE, NEW_DISH, NEW_NOTCOUNTRY, NEW_NOTREGION, NEW_NOTGRAPE, NEW_NOTDISH, CHANGED),
                        (
                            CHANGED > 0 ->
                            (
                                print("Old parameters: "),print(COUNTRY),print(" "),print(REGION),print(" "),print(GRAPE),print(" "),print(DISH),print(" "),print(NOTCOUNTRY),print(" "),print(NOTREGION),print(" "),print(NOTGRAPE),print(" "),print(NOTDISH),print("\n"),
                                print("New parameters: "),print(NEW_COUNTRY),print(" "),print(NEW_REGION),print(" "),print(NEW_GRAPE),print(" "),print(NEW_DISH),print(" "),print(NEW_NOTCOUNTRY),print(" "),print(NEW_NOTREGION),print(" "),print(NEW_NOTGRAPE),print(" "),print(NEW_NOTDISH),print("\n"),
                                NEW_ACCUMULATOR is ACCUMULATOR + THRESHOLD,
                                main(NEW_COUNTRY, NEW_REGION, NEW_GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, NEW_DISH, NEW_NOTCOUNTRY, NEW_NOTREGION, NEW_NOTGRAPE, NEW_NOTDISH, 0, OLD_SORTED_OUT, OUT, NEW_ACCUMULATOR),
                                merge_lists(TMP_FINAL_SORTED_OUT, OUT, NEW_OUT),
                                sort_by_likelihood(NEW_OUT, FINAL_SORTED_OUT)
                                %, print("Current List before returning (after recursive call): "),print(FINAL_SORTED_OUT),print("\n")
                            )
                            ;
                            (
                                print("No new parameters found, returning current list: "),print(FINAL_SORTED_OUT),print("\n"),
                                FINAL_SORTED_OUT = TMP_FINAL_SORTED_OUT
                            )
                        )
                        ;
                        (
                            FINAL_SORTED_OUT = TMP_FINAL_SORTED_OUT
                            %, print("Current List before returning: "),print(FINAL_SORTED_OUT),print("\n")
                        )
                    )
                )
            )
        	;
        	%print("No Result found for threshold: "),print(THRESHOLD),print("\n"),
            THRESHOLD_NEW is THRESHOLD + 1,
            (
                THRESHOLD_NEW < 5 ->
                main(COUNTRY, REGION, GRAPE, FRUITY, BOLD, SAVORY, DRY, TANNIN, DISH, NOTCOUNTRY, NOTREGION, NOTGRAPE, NOTDISH, THRESHOLD_NEW, OLD_SORTED_OUT, FINAL_SORTED_OUT, ACCUMULATOR)
            )
        )
    ).
