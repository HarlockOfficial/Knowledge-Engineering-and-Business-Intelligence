# The following query lists all the wines and sorts them following the constraints specified in the BIND clauses
## Note: for the sorting to work correctly is important to fill the BIND clauses appropriately.
## Examples of valid BIND clauses are:
 - ```BIND(IF(?country = kebi:italy || ?country = kebi:france, 1, 0) as ?v1).``` IF at least one of multiple entries have to match
 - ```BIND(IF(?fruity = 3, 1, 0) as ?v4).``` If the fruity level has to be 3
 - ```BIND(IF(?fruity > 2  && ?fruity < 5, 1, 0) as ?v4).``` If the fruity level has to be in the Integer range (2; 5)

Is also possible to add new BINDings but then the variables have to be add in the last BIND.
Is also possible to modify the BINDings and have some of them weight more or less.

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?country ?region ?grape ?fruity ?bold ?savory ?dry ?tannin ?dish ?wine ?preference WHERE {
	?region kebi:belongs_to ?country.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	
	?dish kebi:dish_matches ?wine.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
	
	BIND(IF(?country = kebi:italy || ?country = kebi:france, 1, 0) as ?v1).
	BIND(IF(?region = kebi:veneto, 1, 0) as ?v2).
	BIND(IF(?grape = kebi:merlot, 1, 0) as ?v3).
	BIND(IF(?fruity = 3, 1, 0) as ?v4).
	BIND(IF(?bold = 4, 1, 0) as ?v5).
	BIND(IF(?savory = 4, 1, 0) as ?v6).
	BIND(IF(?dry = 4, 1, 0) as ?v7).
	BIND(IF(?tannin = 4, 1, 0) as ?v8).
	BIND(IF(?dish = kebi:caprese_salad, 1, 0) as ?v9).
	
	BIND((?v1 + ?v2 + ?v3 + ?v4 + ?v5 +?v6 + ?v7 + ?v8 +?v9) as ?preference)
	
	FILTER (?preference > 0)

}
ORDER BY DESC(?preference)
```

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
