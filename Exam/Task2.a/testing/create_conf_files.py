import itertools
import os
from typing import Set, Dict, Tuple, List, Union

from xml.etree import ElementTree as ET

from testing.extract_xml import extract_xml


def find_all_values(element: ET.Element) -> str:
    """
        Having a xml in the form:
        <input id="Input_1" label="Nation">
            <inputExpression id="InputExpression_1" typeRef="string">
                <text>nation</text>
            </inputExpression>
        </input>

        This function returns a string in the form:
        "italy",
        "france",
        ...
    """
    out = ''
    print("element:", element)
    for child in element:
        if child.tag.endswith('inputExpression'):
            for text in child:
                out += f'"{text.text}",\n'
    return out[:-2]


def extract_keys_and_values_dict(decision: ET.Element, namespace: str):
    try:
        decision_table = decision.findall(namespace + 'decisionTable')[0]
    except IndexError:
        return None, None
    inputs = decision_table.findall(namespace + 'input')
    outputs = decision_table.findall(namespace + 'output')
    rules = decision_table.findall(namespace + 'rule')
    map_keys_to_values = dict()
    ordered_keys_list = list()

    output_column_list = list()

    for output_element in outputs:
        output_column_list.append(output_element.attrib['name'])

    for input_element in inputs:
        """
            Extract the text value from an xml in the form:
            <input id="..." label="...">
                <inputExpression id="..." typeRef="...">
                    <text>nation</text>
                </inputExpression>
            </input>
            and saves it as key for the dictionary map_keys_to_values
        """
        for child in input_element:
            if child.tag.endswith('inputExpression'):
                for text in child:
                    map_keys_to_values[text.text] = set()
                    ordered_keys_list.append(text.text)

    for rule in rules:
        """
            Extract the text value from an xml in the form:
            <rule id="...">
                <inputEntry id="...">
                    <text>"italy"</text>
                </inputEntry>
                <inputEntry id="...">
                    <text>not("italy")</text>
                </inputEntry>
                ...
            </rule>
            and saves it as value for the dictionary map_keys_to_values
            Note that inputEntries can also be in the form:
                - not("italy")
                - not("italy", "france")
            in these cases, the values to be stored are "italy" and "france" 
        """
        input_entries = rule.findall(namespace + 'inputEntry')
        for i, entry in enumerate(input_entries):
            for text in entry:
                if text.text is not None and text.text.strip() != '':
                    text_to_add = text.text.strip()
                    if ' ' not in text_to_add:
                        text_to_add = text_to_add.replace('"', '')
                    if text_to_add.startswith('not('):
                        list_of_elements_to_add = text_to_add.split(',')
                        for element_to_add in list_of_elements_to_add:
                            element_to_add = element_to_add.strip()
                            element_to_add = element_to_add.replace('not(', '')
                            element_to_add = element_to_add.replace(')', '')
                            if ' ' not in element_to_add:
                                element_to_add = element_to_add.replace('"', '')
                            if element_to_add in ['true', 'false']:
                                element_to_add = f'"{element_to_add}"'
                            map_keys_to_values[ordered_keys_list[i]].add(element_to_add)
                    else:
                        if text_to_add in ['true', 'false']:
                            text_to_add = f'"{text_to_add}"'
                        if ',' in text_to_add:
                            values_to_add = list()
                            input_values_list = text_to_add.split(',')
                            for input_value in input_values_list:
                                input_value = input_value.strip()
                                if ' ' not in input_value:
                                    input_value = input_value.replace('"', '')
                                values_to_add.append(input_value)
                            map_keys_to_values[ordered_keys_list[i]].update(values_to_add)
                        else:
                            map_keys_to_values[ordered_keys_list[i]].add(text_to_add)
    return map_keys_to_values, output_column_list


def extract_inputs(decision: ET.Element, namespace: str) -> Tuple[Union[str, None], Union[Dict[str, Set[str]], None], Union[List[str], None]]:
    """
        Having a xml in the form:
        <decision id="Decision_1vu0ist" name="Decide Country">
            ....
            <decisionTable id="DecisionTable_1tg23j0">
              <input id="Input_1" label="Nation">
                <inputExpression id="InputExpression_1" typeRef="string">
                  <text>nation</text>
                </inputExpression>
              </input>
              ...
              <input id="InputClause_0okom14" label="ExcludedNation">
                <inputExpression id="LiteralExpression_059okd9" typeRef="string">
                  <text>excludednation</text>
                </inputExpression>
              </input>
              <output id="Output_1" label="SuggestedWineForCountry" name="suggestedwineforcountry" typeRef="string" biodi:width="238" />
              <rule id="DecisionRule_16vfcbu">
                <inputEntry id="UnaryTests_0umm79a">
                  <text>"italy"</text>
                </inputEntry>
                <inputEntry id="UnaryTests_0pqpi6z">
                  <text>not("italy")</text>
                </inputEntry>
                <outputEntry id="LiteralExpression_0u87uc5">
                  <text>"supertuscan"</text>
                </outputEntry>
              </rule>
              <rule id="DecisionRule_0q8q2qo">
                ...
              </rule>
              ...
            </decisionTable>
        </decision>

        This function returns a string in the form:
        {
            key=nation
            values=[
                "italy",
                ...
            ]
        },
        ...,
        {
            key=excludednation
            values=[
                not("italy"),
                ...
            ]
        }
    """
    keys_and_values, output_column_list = extract_keys_and_values_dict(decision, namespace)
    if keys_and_values is None and output_column_list is None:
        return None, None, None
    out = ""
    for key, values in keys_and_values.items():
        out += f'\t\t{{\n\t\t\tkey={key}\n\t\t\tvalues=[\n'
        for value in values:
            out += f'\t\t\t\t{value},\n'
        out = out[:-2]
        out += '\n\t\t\t]\n\t\t},\n'
    return out[:-2], keys_and_values, output_column_list


def extract_all_rules(rules, namespace: str, key_value_dict: Dict[str, Set[str]]):
    """
            Each rule is in the format:
            <rule id="...">
                <inputEntry id="...">
                  <text>"italy"</text>
                </inputEntry>
                <inputEntry id="...">
                  <text>not("italy")</text>
                </inputEntry>
                <outputEntry id="...">
                  <text>"supertuscan"</text>
                </outputEntry>
            </rule>

            from this rule we know that:
            - the first inputEntry/text.text is a value present in key_value_dict.keys()[0]
            - the second inputEntry/text.text:
                - if contains "not", the value inside not("...") is present in key_value_dict.keys()[1]
                    and we have to consider all the values present in key_value_dict.keys()[1] but the one specified
                - if there is not "not", the specified value is present in key_value_dict.keys()[1]
            This generalises to a rule with n inputEntry

            The outputEntry/text.text is the value of the expected output column for the provided input
                Note: there could be more than one outputEntry, but we can ignore this for now

            Given a set of rules, we want to extract all the possible combinations of inputs and the expected output
        """
    keys_list = list(key_value_dict.keys())
    all_values = list()
    for rule in rules:
        input_entries = rule.findall(namespace + 'inputEntry')
        output_entries = rule.findall(namespace + 'outputEntry')
        # list containing sets, each set has inputs and related expected output
        values = list()
        output_column_length = len(output_entries)
        values.append(output_column_length)
        for index, input_entry in enumerate(input_entries):
            # process input entry
            input_value = input_entry.find(namespace + 'text').text
            if input_value is None:
                values.append(None)
                continue
            if ' ' not in input_value:
                input_value = input_value.replace('"', '')
            if input_value.startswith('not('):
                list_of_elements_to_add = input_value.split(',')
                elements_to_not_add = set()
                for element_to_add in list_of_elements_to_add:
                    element_to_add = element_to_add.strip()
                    element_to_add = element_to_add.replace('not(', '')
                    element_to_add = element_to_add.replace(')', '')
                    if ' ' not in element_to_add:
                        element_to_add = element_to_add.replace('"', '')
                    if element_to_add in ['true', 'false']:
                        element_to_add = f'"{element_to_add}"'
                    elements_to_not_add.add(element_to_add)
                diff_between_sets = key_value_dict[keys_list[index]] - elements_to_not_add
                if len(diff_between_sets) == 1:
                    input_value = diff_between_sets.pop()
                    values.append(input_value)
                else:
                    values.append(tuple(diff_between_sets))
            else:
                if input_value in ['true', 'false']:
                    input_value = f'"{input_value}"'
                if ',' in input_value:
                    values_to_add = list()
                    input_values_list = input_value.split(',')
                    for input_value in input_values_list:
                        input_value = input_value.strip()
                        if ' ' not in input_value:
                            input_value = input_value.replace('"', '')
                        values_to_add.append(input_value)
                    input_value = tuple(values_to_add)
                values.append(input_value)

        # process output entry
        for output_entry in output_entries:
            output_value = output_entry.find(namespace + 'text').text
            if ' ' not in output_value:
                output_value = output_value.replace('"', '')
            if output_value in ['true', 'false']:
                output_value = f'"{output_value}"'
            values.append(output_value)
        all_values.append(tuple(values))

    return all_values


def compute_expected_result(combination, all_rules_tuples):
    """
        Given a combination of inputs, compute the expected output
    """
    valid_results = list()
    for rule_index, rule_tuple in enumerate(all_rules_tuples):
        if len(rule_tuple)-1-rule_tuple[0] != len(combination):
            print(f'rule_tuple: {rule_tuple}')
            print(f'combination: {combination}')
            input()
            continue
        for index, value in enumerate(rule_tuple[1:-rule_tuple[0]]):
            if value != combination[index]:
                break
        else:
            valid_results.append((tuple(rule_tuple[-rule_tuple[0]:]), rule_index))
    return valid_results


def extract_test_cases(rules, key_value_dict, namespace):
    all_rules_tuples = extract_all_rules(rules, namespace, key_value_dict)
    """
        Using the list of tuples in the format (expected_input1, (expected_input2.a, expected_input2.b), ..., expected_input_n, expected_output)
        generate all the possible combinations of inputs and expected output and save them in a list of tuples in the format
        (amount_of_expected_output, input1, input2, ..., input_n, expected_output_1, ..., expected_output_n)
        
        The input columns are thematic, therefore is not allowed to mix values from different columns
        
        Each test case will have a subset of elements from the first input column as first value, a subset of elements from the second input column as second value, and so on
        The expected output will be computed using the rules.
        Note: the expected output can also be a list or empty
    """
    all_test_cases = list()
    assert [(len(rule_tuple) == len(key_value_dict.keys()) + rule_tuple[0] + 1) for rule_tuple in all_rules_tuples]
    all_possible_combinations = list()
    for index in range(len(key_value_dict.keys())):
        all_possible_combinations.append(list())
        for rule_tuple in all_rules_tuples:
            all_possible_combinations[index].append(rule_tuple[1:][index])
        all_possible_combinations[index] = set(all_possible_combinations[index])
    all_possible_combinations = list(itertools.product(*all_possible_combinations))
    for combination in all_possible_combinations:
        expected_result_list = compute_expected_result(combination, all_rules_tuples)
        combination = list(combination)
        if len(expected_result_list) == 0:
            none_tuple = (None, -1, )
            combination.append((none_tuple,))
        else:
            combination.append(tuple(expected_result_list))
        all_test_cases.append(tuple(combination))
    return all_test_cases


def generate_test_cases(decision: ET.Element, namespace: str, key_value_dict: Dict[str, Set[str]], output_column_list: list) -> str:
    template_single_test_case_input = '\t\t{{\n\t\t\tinputs {{\n{test_case_input}\n\t\t\t}}\n'
    template_for_each_single_test_case_input_or_output = '\t\t\t\t{key}={value}\n'
    template_single_test_result_list_output = '\t\t\tresults=[\n{test_case_output}\n\t\t\t]\n\t\t}},\n'
    template_for_each_single_test_case_output = '\t\t\t\t{{\n\t\t\t\t\toutputs {{\n{test_case_multiple_output}\t\t\t\t\t}}\n\t\t\t\t\trowIndex=\"{row_index}\"\n\t\t\t\t}},\n'

    rules = decision.findall(namespace + 'decisionTable/' + namespace + 'rule')
    all_test_cases = extract_test_cases(rules, key_value_dict, namespace)
    out = ""
    for test_case in all_test_cases:
        inputs = ""
        for index in range(len(test_case) - 1):
            string_version = None
            if test_case[index] is not None and isinstance(test_case[index], str):
                string_version = test_case[index]
            elif test_case[index] is not None and isinstance(test_case[index], tuple) and len(test_case[index]) > 1:
                string_version = '"' + ','.join(list(test_case[index])) + '"'
            elif string_version is None:
                string_version = '""'
            inputs += template_for_each_single_test_case_input_or_output.format(key=list(key_value_dict.keys())[index], value=string_version)
        inputs = template_single_test_case_input.format(test_case_input=inputs)

        all_outputs = ""
        for index, output in enumerate(list(test_case[-1])):
            if output[0] is None:
                outputs = template_for_each_single_test_case_output.format(output_column_name=output_column_list[0], output_value='\"\"', row_index='-1')
                outputs = template_single_test_result_list_output.format(test_case_output=outputs[:-2])
            else:
                list_output = ""
                row_index = -1
                for element_index, element in enumerate(output):
                    if type(element) is tuple and len(element) == 2:
                        expected_result_tuple, row_index = element
                    else:
                        expected_result_tuple = element
                    output_column_name = output_column_list[element_index]
                    list_output += '\t\t' + template_for_each_single_test_case_input_or_output.format(key=output_column_name, value=expected_result_tuple)
                outputs = template_for_each_single_test_case_output.format(test_case_multiple_output=list_output, row_index=row_index)
                outputs = template_single_test_result_list_output.format(test_case_output=outputs[:-2])
            all_outputs += outputs
        out += inputs + all_outputs
    return out[:-2]


def get_conf_file_content(decision: ET.Element, decision_id: str, dmn_file_name: str, namespace: str) -> Union[str, None]:
    out = 'data {\n'
    inputs, key_value_dict, output_column_list = extract_inputs(decision, namespace)
    if inputs is None and key_value_dict is None and output_column_list is None:
        return None
    out += '\tinputs=[\n' + inputs + '\n\t]\n'
    out += '\ttestCases=[\n' + generate_test_cases(decision, namespace, key_value_dict, output_column_list) + '\n\t]\n'
    out += '\tvariables=[]\n}\n'
    out += f'decisionId={decision_id}\n'
    out += f'dmnPath=[\n\tdmns,\n\t\"{dmn_file_name}\"\n]\n'
    out += 'isActive=\"true\"'
    return out


def save_conf_file(conf_content: str, save_file_path: str):
    with open(save_file_path, "w") as f:
        f.write(conf_content)


def save_decision_id_to_conf_file_mapping(path_to_mapping_file: str, conf_file_path: str, decision_id: str):
    with open(path_to_mapping_file, 'a') as f:
        f.write(f'{conf_file_path} {decision_id}\n')


def parse_dmn(dmn_path: str, save_conf_file_path: str, path_to_mapping_file: str):
    f = open(path_to_mapping_file, 'w')
    f.close()
    xml_root = extract_xml(dmn_path)
    dmn_file_name = os.path.basename(dmn_path)
    namespace = '{https://www.omg.org/spec/DMN/20191111/MODEL/}'
    for child in xml_root:
        if child.tag.endswith('decision'):
            decision_id = str(child.attrib['id'])
            conf_content = get_conf_file_content(child, decision_id, dmn_file_name, namespace)
            if conf_content is None:
                continue
            conf_file_path = save_conf_file_path + '/' + str(child.attrib['name']).replace(' ', '_') + '.conf'
            save_conf_file(conf_content, conf_file_path)
            save_decision_id_to_conf_file_mapping(path_to_mapping_file, conf_file_path, decision_id)


if __name__ == '__main__':
    print('Starting...')
    parse_dmn('/home/administrator/Desktop/Knowledge-Engineering-and-Business-Intelligence/Exam/Task2.a/tmp.dmn',
              '/home/administrator/Desktop/Knowledge-Engineering-and-Business-Intelligence/Exam/Task2.a/documents',
              '/home/administrator/Desktop/Knowledge-Engineering-and-Business-Intelligence/Exam/Task2.a/documents/mapping2.txt')
    print('Done!')
