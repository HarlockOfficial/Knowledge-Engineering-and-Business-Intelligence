# The following query lists all the wines from france that can be paired with "caprese salad" and have a dryness of 4:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?ingredient ?dish ?wine WHERE {
	?region kebi:belongs_to ?country.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	
	?dish kebi:dish_matches ?wine.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.

	FILTER(
		?dry = 4 && ?country = kebi:france && ?dish = kebi:caprese_salad
	)
}
ORDER BY ?wine
```

# The following query lists all the wines which are not from france that can be paired with "caprese salad" and have a dryness of 4:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?ingredient ?dish ?wine WHERE {
	?region kebi:belongs_to ?country.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	
	?dish kebi:dish_matches ?wine.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.

	FILTER(
		?dry = 4 && ?country = kebi:france && ?dish = kebi:caprese_salad
	)
}
ORDER BY ?wine
```


# The following query lists all the possible matches between dishes and wines, along with the ingredient triggering the match:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?ingredient ?dish ?wine WHERE {
	?region kebi:belongs_to ?country.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	
	?wine kebi:paired_with ?ingredient.

	?ingredient kebi:contained_in_dish ?dish.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
}
ORDER BY ?wine
```

# The following query lists all the possible perfect matches between dishes and wines, along with the ingredient triggering the match:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?ingredient ?dish ?wine WHERE {
	?region kebi:belongs_to ?country.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	
	?wine kebi:perfectly_paired_with ?ingredient.

	?ingredient kebi:contained_in_dish ?dish.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
}
ORDER BY ?wine
```

# The following query lists all the possible matches between dishes and wines:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?ingredient ?dish ?wine WHERE {
	?region kebi:belongs_to ?country.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	
	?dish kebi:dish_matches ?wine.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
}
ORDER BY ?wine
```

# The following query lists all the possible wines:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?ingredient ?dish ?wine WHERE {
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
}
ORDER BY ?wine
```
