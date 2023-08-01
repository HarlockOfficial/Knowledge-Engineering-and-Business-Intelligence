import json
import os.path
import re
from typing import Union

from swiplserver import PrologMQI


def remove_duplicates(list_of_dicts: list[dict[str, str]]) -> list[dict[str, str]]:
    removed_duplicates = []
    for element in list_of_dicts:
        if element not in removed_duplicates:
            removed_duplicates.append(element)
    return removed_duplicates


def partial_contains(list_of_dicts: list[dict[str, str]], dict_to_check: dict[str, str]) -> tuple[bool, int]:
    for index, element in enumerate(list_of_dicts):
        if any(element[key] == dict_to_check[key] for key in dict_to_check.keys()):
            return True, index
    return False, -1


def update_dict(dictionary: Union[dict[str, str], dict[str, list[str]]], element: dict[str, str]) -> dict[str, list[str]]:
    for key, value in element.items():
        if not isinstance(dictionary[key], list):
            dictionary[key] = [dictionary[key]]
            assert isinstance(dictionary[key], list)
        if value not in dictionary[key]:
            assert isinstance(dictionary[key], list)
            dictionary[key].append(value)
    return dictionary


def merge_lists(list_of_dicts: list[dict[str, str]]) -> list[dict[str, list[str]]]:
    merged_list = []
    for element in list_of_dicts:
        result, index = partial_contains(merged_list, element)
        if result:
            merged_list[index] = update_dict(merged_list[index], element)
        else:
            merged_list.append(element)
    return merged_list


def clean_query(query_name: str) -> str:
    query_name = query_name.strip('?-')
    query_name = query_name.strip('.')
    return query_name.strip()


def find_query_to_call(path_to_prolog_file_with_query_definition: str, query_function_name: str) -> str:
    if not os.path.isfile(path_to_prolog_file_with_query_definition):
        raise Exception('File not found')

    query_to_call = ''

    with open(path_to_prolog_file_with_query_definition, 'r') as f:
        for line in f.readlines():
            if query_function_name in line:
                query_to_call = line
                break

    if query_to_call == '':
        raise Exception('Query function name not found in file')

    return query_to_call


def prepare_query(query_to_call: str, **kwargs) -> str:
    # Replace the parameters in the query with the values from kwargs
    for key, value in kwargs.items():
        decoded_value = str(value).encode('utf-8', 'ignore').decode('utf-8', 'ignore')
        query_to_call = re.sub(r'\b' + key + r'\b', decoded_value, query_to_call)

    query_to_call = query_to_call.replace('\'', '')

    return query_to_call.strip()[:-3] + '.'


def query(path_to_prolog_file_with_query_definition: str, query_function_name: str, logger=None, **kwargs) ->\
        tuple[bool, list[dict[str, str]]]:
    # kwargs contains all the parameters for the prolog query
    if kwargs is None:
        kwargs = {}

    query_function_name = clean_query(query_function_name)

    query_to_call = find_query_to_call(path_to_prolog_file_with_query_definition, query_function_name)

    query_to_call = clean_query(query_to_call)

    if query_to_call == '':
        raise Exception('Query function name not existent in file')

    query_to_call = prepare_query(query_to_call, **kwargs)

    if logger is not None:
        logger("Ensure Loaded: " + path_to_prolog_file_with_query_definition)
        logger("Executing query: " + query_to_call)

    # Execute the query
    with PrologMQI() as mqi:
        with mqi.create_thread() as prolog:
            prolog.query('ensure_loaded("' + path_to_prolog_file_with_query_definition + '").')
            result = prolog.query(query_to_call)
    # eventually clean the list_with_duplicates
    if result:
        result = remove_duplicates(result)
        result = merge_lists(result)
        result = True, result
    else:
        result = False, []
    if logger is not None:
        logger("Result: " + str(result))
    return result


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('path_to_prolog_file_with_query_definition', type=str, help='path to the prolog file with the query definition')
    parser.add_argument('query_function_name', type=str, help='name of the query function to call')
    parser.add_argument('kwargs', type=json.loads, help='dictionary containing the parameters for the query')
    parser.add_argument('--logger', action='store_true', help='if present, the results will be printed')

    args = parser.parse_args()

    logger = None
    if args.logger:
        logger = print
    query(args.path_to_prolog_file_with_query_definition, args.query_function_name, logger, **args.kwargs)
