%countries

country(italy).
country(spain).
country(france).

%regions

region(marches, italy).
region(tuscany, italy).
region(veneto, italy).
region(sicily, italy).

region(provence, france).
region(bordeaux, france).
region(loirevalley, france).
region(champagne, france).

region(rhonevalley, spain).
region(levante, spain).
region(catalunia, spain).
region(rioja, spain).

%grapes

grape(merlot).
grape(pinotnoire).
grape(viura).
grape(cabernetfranc).
grape(syrah).

%ingredients

ingredient(beef).
ingredient(crab).
ingredient(turkey).
ingredient(parmesan).
ingredient(maitakemushroom).

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

%wine id - ingredient

wineusedwith(a, beef).
wineusedwith(b, turkey).
wineusedwith(c, beef).
wineusedwith(c, parmesan).
wineusedwith(d, maitakemushroom).
wineusedwith(e, maitakemushroom).
wineusedwith(f, crab).
wineusedwith(f, turkey).
wineusedwith(g, maitakemushroom).
wineusedwith(g, turkey).
wineusedwith(g, crab).
wineusedwith(h, beef).
wineusedwith(i, beef).
wineusedwith(j, maitakemushroom).
wineusedwith(j, turkey).

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

%dish id - ingredients

food_contains(risotto_beef, beef).
food_contains(risotto_beef, parmesan).
food_contains(steamed_turkey, turkey).
food_contains(steamed_turkey, maitakemushroom).
food_contains(sea_soup, crab).
food_contains(sea_soup, maitakemushroom).
