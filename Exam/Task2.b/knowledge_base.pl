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
wine(a, merlot).
wine(a, viura).

winebelongs(a, sicily).
winebelongs(a, catalunia).

fruity(a, 6).
bold(a, 1).
savory(a, 8).
dry(a, 4).
tannin(a, 10).

