%countries

country(italy).
country(spain).
country(france).

%regions

region(tuscany, italy).
region(veneto, italy).
region(sicily, italy).

region(provence, france).
region(bordeaux, france).
region(loirevalley, france).
region(champagne, france).
region(burgundy, france).

region(rhonevalley, spain).
region(catalunia, spain).
region(rioja, spain).

%grapes

grape(merlot).
grape(pinotnoire).
grape(viura).
grape(cabernetfranc).
grape(syrah).
grape(chardonnay).

%ingredients

ingredient(beef).
ingredient(crab).
ingredient(turkey).
ingredient(parmesan).
ingredient(maitakemushroom).
ingredient(mozzarella).
ingredient(tomato).
ingredient(shrimp).
ingredient(salmon).

%wine id - grapes

containsgrape(a, merlot).
containsgrape(a, cabernetfranc).
containsgrape(a, syrah).

containsgrape(b, viura).
containsgrape(b, pinotnoir).

containsgrape(c, syrah).

containsgrape(d, merlot).

containsgrape(e, cabernetfranc).

containsgrape(f, viura).

containsgrape(g, pinotnoir).

containsgrape(h, cabernetfranc).
containsgrape(h, merlot).
containsgrape(h, syrah).

containsgrape(i, merlot).
containsgrape(i, syrah).

containsgrape(j, pinotnoir).
containsgrape(j, syrah).

containsgrape(k, chardonnay).

%wine id - region

winebelongs(a, tuscany).

winebelongs(b, provence).
winebelongs(b, loirevalley).
winebelongs(b, champagne).

winebelongs(c, rhonevalley).
winebelongs(c, sicily).

winebelongs(d, bordeaux).
winebelongs(d, tuscany).

winebelongs(e, bordeaux).
winebelongs(e, loirevalley).
winebelongs(e, tuscany).
winebelongs(e, veneto).

winebelongs(f, rioja).
winebelongs(f, catalunia).

winebelongs(g, catalunia).
winebelongs(g, veneto).

winebelongs(h, bordeaux).

winebelongs(i, rhonevalley).

winebelongs(j, bordeaux).

winebelongs(k, burgundy).

%wine id - taste intensity

fruity(a, 4).
fruity(b, 5).
fruity(c, 4).
fruity(d, 4).
fruity(e, 3).
fruity(f, 3).
fruity(g, 4).
fruity(h, 3).
fruity(i, 4).
fruity(j, 4).
fruity(k, 3).

bold(a, 4).
bold(b, 2).
bold(c, 5).
bold(d, 3).
bold(e, 4).
bold(f, 2).
bold(g, 4).
bold(h, 4).
bold(i, 4).
bold(j, 3).
bold(k, 2).

savory(a, 3).
savory(b, 2).
savory(c, 4).
savory(d, 3).
savory(e, 4).
savory(f, 4).
savory(g, 3).
savory(h, 4).
savory(i, 4).
savory(j, 3).
savory(k, 2).

dry(a, 4).
dry(b, 4).
dry(c, 2).
dry(d, 4).
dry(e, 4).
dry(f, 2).
dry(g, 5).
dry(h, 4).
dry(i, 4).
dry(j, 4).
dry(k, 4).

tannin(a, 4).
tannin(b, 1).
tannin(c, 5).
tannin(d, 3).
tannin(e, 4).
tannin(f, 5).
tannin(g, 2).
tannin(h, 4).
tannin(i, 4).
tannin(j, 4).
tannin(k, 1).

% wine id - ingredient
wineusedwith(a, beef).
wineusedwith(a, parmesan).

wineusedwith(c, beef).
wineusedwith(c, parmesan).

wineusedwith(d, maitakemushroom).
wineusedwith(d, parmesan).
wineusedwith(d, tomato).

wineusedwith(e, maitakemushroom).
wineusedwith(e, tomato).

wineusedwith(f, salmon).

wineusedwith(g, maitakemushroom).
wineusedwith(g, mozzarella).

wineusedwith(g, turkey).

wineusedwith(h, beef).
wineusedwith(h, parmesan).

wineusedwith(j, maitakemushroom).

wineusedwith(k, crab).
wineusedwith(k, mozzarella).
wineusedwith(k, maitakemushroom).
wineusedwith(k, shrimp).
wineusedwith(k, turkey).

%weak wine id - ingredient suggestion

weakwineusedwith(b, crab).
weakwineusedwith(b, parmesan).
weakwineusedwith(b, shrimp).
weakwineusedwith(b, tomato).
weakwineusedwith(b, turkey).

weakwineusedwith(c, maitakemushroom).
weakwineusedwith(c, tomato).

weakwineusedwith(d, beef).
weakwineusedwith(d, mozzarella).
weakwineusedwith(d, parmesan).
weakwineusedwith(d, turkey).

weakwineusedwith(e, beef).
weakwineusedwith(e, maitakemushroom).
weakwineusedwith(e, mozzarella).
weakwineusedwith(e, parmesan).
weakwineusedwith(e, tomato).
weakwineusedwith(e, turkey).

weakwineusedwith(f, crab).
weakwineusedwith(f, parmesan).
weakwineusedwith(f, shrimp).

weakwineusedwith(g, mozzarella).

weakwineusedwith(j, beef).
weakwineusedwith(j, parmesan).
weakwineusedwith(j, mozzarella).
weakwineusedwith(j, turkey).

weakwineusedwith(k, parmesan).
weakwineusedwith(k, salmon).


%wine id - wine name

winename(a, supertuscan).
winename(b, rose).
winename(c, syrah).
winename(d, merlot).
winename(e, cabernetfranc).
winename(f, viura).
winename(g, pinotnoir).
winename(h, bordeaux).
winename(i, rhone).
winename(j, northwest).
winename(k, chardonnay).

%dish id - ingredients

food_contains(crab_parmesan_stuffed_shrimp, crab).
food_contains(crab_parmesan_stuffed_shrimp, parmesan).
food_contains(crab_parmesan_stuffed_shrimp, shrimp).
food_contains(turkey_maitakemushroom_risotto, turkey).
food_contains(turkey_maitakemushroom_risotto, maitakemushroom).
food_contains(beef_parmesan_meatballs_with_maitakemushroom_sauce, beef).
food_contains(beef_parmesan_meatballs_with_maitakemushroom_sauce, parmesan).
food_contains(beef_parmesan_meatballs_with_maitakemushroom_sauce, maitakemushroom).
food_contains(caprese_salad, tomato).
food_contains(caprese_salad, mozzarella).
food_contains(grilled_salmon, salmon).
food_contains(grilled_steak, beef).