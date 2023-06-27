# The following query lists all wines available:
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?countryname ?regionname ?grapename ?fruity ?bold ?savory ?dry ?tannin ?dishname ?winename WHERE {
	?country rdf:type kebi:country.
	?region rdf:type kebi:region.
	?region kebi:belongs_to ?country.
	?grape rdf:type kebi:grape.
	?dish rdf:type kebi:dish.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	?wine kebi:paired_with ?dish.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
	
	?country kebi:name ?countryname.
	?region kebi:name ?regionname.
	?grape kebi:name ?grapename.
	?dish kebi:name ?dishname.
	
	?wine kebi:name ?winename.
}
ORDER BY ?winename
```

# The following query lists all the wines that can be paired with "Beef":
```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX kebi: <urn:absolute:it.unicam.kebi.exam.task2.c#>

SELECT ?countryname ?regionname ?grapename ?fruity ?bold ?savory ?dry ?tannin ?dishname ?winename WHERE {
	?country rdf:type kebi:country.
	?region rdf:type kebi:region.
	?region kebi:belongs_to ?country.
	?grape rdf:type kebi:grape.
	?dish rdf:type kebi:dish.
	
	?wine kebi:belongs_to_region ?region.
	?wine kebi:contains_grape ?grape.
	?wine kebi:paired_with ?dish.
	
	?wine kebi:fruity ?fruity.
	?wine kebi:bold ?bold.
	?wine kebi:savory ?savory.
	?wine kebi:dry ?dry.
	?wine kebi:tannin ?tannin.
	
	?country kebi:name ?countryname.
	?region kebi:name ?regionname.
	?grape kebi:name ?grapename.
	?dish kebi:name "Beef".
	?dish kebi:name ?dishname.
	
	?wine kebi:name ?winename.
}
ORDER BY ?winename
```