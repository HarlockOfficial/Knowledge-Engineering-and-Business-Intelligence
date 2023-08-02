import itertools
import json

from query_knowledgebase import query


def find_free_letter(kwargs):
    dot_product = itertools.product('ABCDEFGHIJKLMNOPQRSTUVWXYZ', repeat=1)
    for letter in dot_product:
        if letter not in kwargs.values():
            return letter
    return False


def sanitise_kwargs(kwargs: dict, results_ordering_mechanism: list) -> dict:
    for key in results_ordering_mechanism:
        if key not in kwargs.keys():
            free_letter = find_free_letter(kwargs)
            # for simplicity, we assume that if the string THRESHOLD is present
            # in the list of ordering mechanisms, then the value has to be a number,
            # therefore defaulting to 0, otherwise the value is a letter
            if 'THRESHOLD' in key.upper():
                kwargs[key] = [0]
            elif free_letter:
                kwargs[key] = [free_letter]
            else:
                kwargs[key] = []

    return kwargs


def extract_all_permutations(kwargs, results_ordering_mechanism):
    all_permutations = kwargs.copy()
    for mechanism in results_ordering_mechanism:
        mechanism_value = kwargs[mechanism]
        if [isinstance(value, int) or isinstance(value, float) for value in mechanism_value] == [True] * len(
                mechanism_value):
            all_permutations[mechanism] = []
            all_permutations[mechanism].extend(mechanism_value)
            continue
        all_permutations[mechanism] = []
        for index in range(len(mechanism_value)):
            all_permutations[mechanism].extend(itertools.permutations(mechanism_value, len(mechanism_value) - index))
        if len(all_permutations[mechanism]) > 1:
            all_permutations[mechanism].append((find_free_letter(kwargs),))

    all_permutations = {key: all_permutations[key] for key in reversed(results_ordering_mechanism)}
    return itertools.product(*all_permutations.values())


def find_index(removed_duplicates, element, result_key):
    for index, element_in_list in enumerate(removed_duplicates):
        if element[result_key][0] == element_in_list[result_key][0]:
            return index
    return -1


def merge_duplicates_per_list(list_with_duplicates: list[list[dict[str, str]]], result_key: str) -> list[list[dict[str, list[str]]]]:
    removed_duplicates = []
    for list_of_dict in list_with_duplicates:
        removed_duplicates.append([])
        for element in list_of_dict:
            index = find_index(removed_duplicates[-1], element, result_key)
            if index == -1:
                removed_duplicates[-1].append(element)
            else:
                for key in removed_duplicates[-1][index].keys():
                    if key != result_key:
                        if [x not in removed_duplicates[-1][index][key] for x in element[key]] == [True] * len(element[key]):
                            removed_duplicates[-1][index][key].extend(element[key])
    return removed_duplicates


def merge_lists(list_to_merge: list[list[dict[str, list[str]]]], result_key: str):
    elements_dict = {}
    ordered_out = []

    for single_list in list_to_merge:
        for element in single_list:
            if element[result_key][0] not in ordered_out:
                ordered_out.append(element[result_key][0])

            if element[result_key][0] not in elements_dict.keys():
                elements_dict[element[result_key][0]] = element

    return elements_dict, ordered_out


def execute_query(path_to_prolog_file_with_query_definition: str, query_function_name: str,
                  results_ordering_mechanism: list = None, logger=None, **kwargs):
    """
        :param path_to_prolog_file_with_query_definition: string containing the path to the prolog file with the query definition
        :param query_function_name: string containing the name of the query function to call
        :param logger: logger function to use for logging
        :param kwargs: dictionary containing the parameters for the query
        :param results_ordering_mechanism: list containing the order of importance in which the list_with_duplicates should be queried and ordered
        :return: a list containing the list_with_duplicates of the query ordered following the criteria defined in results_ordering_mechanism
    """
    if results_ordering_mechanism is None:
        results_ordering_mechanism = []

    if kwargs is None:
        kwargs = {}

    kwargs = sanitise_kwargs(kwargs, results_ordering_mechanism)

    all_permutations = extract_all_permutations(kwargs, results_ordering_mechanism)

    results = []

    for values in all_permutations:
        for index, param in enumerate(reversed(values)):
            if isinstance(param, tuple) or isinstance(param, list):
                kwargs[results_ordering_mechanism[index]] = list(param)
            elif hasattr(param, 'len'):
                kwargs[results_ordering_mechanism[index]] = param[0]
            else:
                kwargs[results_ordering_mechanism[index]] = param

        print("executing query with data:", kwargs)
        result, results_per_changing_values = query(path_to_prolog_file_with_query_definition, query_function_name, logger,
                                            **kwargs)
        if result:
            results.append([])
            results[-1].extend(results_per_changing_values)
    with open("query_results.json", "w") as f:
        json.dump(results, f, indent=4)
    return results


def fix_results(result_key: str, results=None):
    if results is None:
        with open("query_results.json", "r") as f:
            results = json.load(f)
    results = merge_duplicates_per_list(results, result_key)
    results, ordered_wine_list = merge_lists(results, result_key)
    with open("final_suggestions.json", "w") as f:
        json.dump(results, f, indent=4)
    with open("final_ordered_wine_list_suggestion.json", "w") as f:
        json.dump(ordered_wine_list, f, indent=4)
    return results


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--path_to_prolog_file_with_query_definition', type=str, default='../horn_clause.pl', help='path to the prolog file with the query definition')
    parser.add_argument('--query_function_name', type=str, default='softened_query', help='name of the query function to call')
    parser.add_argument('--results_ordering_mechanism', type=list, default=['THRESHOLD', 'REGION', 'COUNTRY', 'DISH', 'NOTREGION', 'NOTCOUNTRY', 'NOTDISH', 'NOTGRAPE', 'GRAPE'], help='list containing the order of importance in which the constraints have to be softened')
    parser.add_argument('--results_key', type=str, default='WINE', help='name of the column containing the most important result of the query')
    parser.add_argument('--obtain_results', action='store_true', help='if true, the query will be executed and the results will be saved in a json file')
    parser.add_argument('--fix_results', action='store_true', help='if true, the ordered list will be extracted from the query result json file')
    parser.add_argument('--kwargs', type=json.loads, default="{\"COUNTRY\": [\"italy\"], \"REGION\": [\"REGION\"], \"GRAPE\": [\"GRAPE\"], \"DISH\": [\"DISH\"], \"NOTCOUNTRY\": [\"NOTCOUNTRY\"], \"NOTREGION\": [\"NOTREGION\"], \"NOTGRAPE\": [\"NOTGRAPE\"], \"NOTDISH\": [\"NOTDISH\"], \"THRESHOLD\": [0, 1, 2], \"FRUITY\": 3, \"BOLD\": 3, \"SAVORY\": 3, \"DRY\": 3, \"TANNIN\": 3}", help='dictionary containing the parameters for the query')

    args = parser.parse_args()

    query_results = None
    if args.obtain_results:
        query_results = execute_query(args.path_to_prolog_file_with_query_definition,
                                      args.query_function_name,
                                      logger=None,
                                      results_ordering_mechanism=args.results_ordering_mechanism,
                                      **args.kwargs)
    if args.fix_results:
        fix_results(result_key=args.results_key, results=query_results)
