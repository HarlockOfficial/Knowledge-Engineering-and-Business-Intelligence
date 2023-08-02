In this file we present the content of the ordering folder.
This folder contains two main files, ```matching_values_ordering.py``` and ```query_knowledgebase.py```
The first file is used to order the values of the knowledgebase, the second file is used to query the knowledgebase.

The main interaction points is provided by the first file.

To execute it, is necessary to install the required dependencies present in requirements.txt.
To do so, run:
```
pip3 install -r requirements.txt
```
Other than the python dependencies, it is necessary to have SWI-Prolog installed on the machine.
Is possible to use as a reference this [link](https://www.swi-prolog.org/packages/mqi/prologmqi.html) where all the installation steps are explained.

To execute the file, run: 
```
python3 matching_values_ordering.py --obtain_results --fix_results "{\"COUNTRY\": [\"italy\"], \"REGION\": [\"REGION\"], \"GRAPE\": [\"GRAPE\"], \"DISH\": [\"DISH\"], \"NOTCOUNTRY\": [\"NOTCOUNTRY\"], \"NOTREGION\": [\"NOTREGION\"], \"NOTGRAPE\": [\"NOTGRAPE\"], \"NOTDISH\": [\"NOTDISH\"], \"THRESHOLD\": [0, 1, 2], \"FRUITY\": 3, \"BOLD\": 3, \"SAVORY\": 3, \"DRY\": 3, \"TANNIN\": 3}"
```

Other parameters are also present to provide freedom to the user.
To see them, run:
```
python3 matching_values_ordering.py --help
```

After the execution, at most three new files have been created:
- ```query_results.json```: contains the results of the runned Prolog queries
- ```final_ordered_wine_list_suggestion.json```: a short ordered list of wines that match the queries and the user choices, ordered from the best match, to the worst one
- ```final_suggestion.json```: a list of wines with related parameters that allow to verify the correctness of the ordering and the matching parameters.

Please note that the ```query_results.json``` file can be used as input for the generation of the other two.

Indeed, the whole process can be run in separate moments, by executing the following commands:
```
python3 matching_values_ordering.py --obtain_results "{\"COUNTRY\": [\"italy\"], \"REGION\": [\"REGION\"], \"GRAPE\": [\"GRAPE\"], \"DISH\": [\"DISH\"], \"NOTCOUNTRY\": [\"NOTCOUNTRY\"], \"NOTREGION\": [\"NOTREGION\"], \"NOTGRAPE\": [\"NOTGRAPE\"], \"NOTDISH\": [\"NOTDISH\"], \"THRESHOLD\": [0, 1, 2], \"FRUITY\": 3, \"BOLD\": 3, \"SAVORY\": 3, \"DRY\": 3, \"TANNIN\": 3}"
```
To generate the query results.

And then:
```
python3 matching_values_ordering.py --fix_results "[]"
```
To generate the final ordered wine list suggestion.
