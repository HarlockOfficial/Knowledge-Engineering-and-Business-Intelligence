country(italy).
country(spain).
country(france).

region(marche, italy).
region(tuscany, italy).
region(veneto, italy).
region(sicily, italy).

region(provence, france).
region(bordeaux, france).
region(loirevalley, france).
region(champagne, france).

region(meseta, spain).
region(andalusia, spain).
region(levante, spain).
region(catalunia, spain).

grape(merlot).
grape(pinotnoire).
grape(viura).
grape(cabernetfranc).
grape(syrah).

dish(beef).
dish(crab).
dish(turkey).
dish(parmesan).
dish(maitakemushroom).

% wine(unique_identifier, grape_variety)
containsgrape(a, merlot).
containsgrape(a, cabernetfranc).
containsgrape(a, syrah)

winebelongs(a, tuscany).

fruity(a, 3).
bold(a, 7).
savory(a, 4).
dry(a, 2).
tannin(a, 9).

wineusedwith(a, beef).

winename(a, supertuscan).

