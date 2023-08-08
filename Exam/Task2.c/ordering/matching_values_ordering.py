import json

from owlready2 import World

import OWLManager


def run_query(query_world: World, query_content: str):
    if query_content.strip() == '':
        raise ValueError('Query cannot be empty')
    return list(OWLManager.query(query_world, query_content))


def prepare_query_content(query_file: str, **kwargs) -> str:
    if query_file is None:
        raise ValueError('query_file must be provided')
    with open(query_file, 'r') as f:
        query_content = f.read()
    return query_content.format(**kwargs)


def merge_results(input_list: list):
    """
    Assuming the meaningful output value of the query is the last parameter
    :param input_list: a list of tuples containing the results of the query
    :return: a list of tuples containing the merged results of the query
    """
    output_list = list()
    output_index_mapping = dict()
    for element in input_list:
        if element[-1] not in output_index_mapping:
            output_index_mapping[element[-1]] = len(output_list)
            output_list.append([])
        index = output_index_mapping[element[-1]]
        for result_index, result in enumerate(element):
            if len(output_list[index]) <= result_index:
                output_list[index].append(list())
            if result not in output_list[index][result_index]:
                output_list[index][result_index].append(result)
    for index, element in enumerate(output_list):
        output_list[index] = tuple(tuple(x) for x in element)
    return output_list


def execute_query(input_file, query_file, kwargs):
    thresholds = kwargs['THRESHOLD'].copy()
    world = OWLManager.create_world(input_file)
    ordered_output = list()
    for value in thresholds:
        kwargs['THRESHOLD'] = value
        query_content = prepare_query_content(query_file, **kwargs)
        print("Running query with args: {}".format(kwargs))
        results = run_query(world, query_content)
        results = merge_results(results)
        ordered_output.append(results)

    final_out = []
    for element in ordered_output[0]:
        final_out.append(element)

    for out_list in ordered_output:
        for element in out_list:
            if element not in final_out:
                final_out.append(element)
    return final_out


def to_string(input_list):
    output_list = []
    for element in input_list:
        output_list.append([])
        for entry in element:
            output_list[-1].append([])
            for x in entry:
                if isinstance(x, float):
                    output_list[-1][-1].append(x)
                else:
                    output_list[-1][-1].append(str(x).replace('wine_choosing_system.', ''))
    return output_list


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', type=str, required=True, help='Input RDF file path or URL')
    parser.add_argument('--query_file', type=str, help='path to a file containing a SPARQL query_str')
    parser.add_argument('--kwargs', type=json.loads, help='dictionary containing the parameters for the query_content \nNOTE:the dictionary keys must match the values in the results_ordering_mechanism list')

    args = parser.parse_args()
    ordered_response = execute_query(args.input, args.query_file, args.kwargs)
    string_response = to_string(ordered_response)
    with open("query_results.json", "w") as f:
        json.dump(string_response, f, indent=4)
