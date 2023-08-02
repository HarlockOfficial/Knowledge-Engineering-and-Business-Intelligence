from owlready2 import get_ontology, onto_path, default_world, get_namespace, set_log_level, World


def create_world(path_to_ontology: str):
    """Create a query_world from an ontology file"""
    onto_path.append(path_to_ontology)
    ontology = get_ontology(path_to_ontology)
    ontology.load()
    get_namespace(ontology.base_iri)
    return default_world


def query(world: World(), query_str: str):
    """Run a SPARQL query_str on a query_world"""
    return world.sparql_query(query_str)
