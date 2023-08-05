In this file we present the content of the ordering folder.
This folder contains two main files, ```matching_values_ordering.py``` and ```query_knowledgebase.py```
The first file is used to order the values of the knowledgebase, the second file is used to query the knowledgebase.

The main interaction points is provided by the first file.

To execute it, is necessary to install the required dependencies present in requirements.txt.
To do so, open a terminal within the folder containing this file and run:
```
pip3 install -r requirements.txt
```
To execute the file, run: 
```
python matching_values_ordering.py --input "[PATH_TO_RDF_FILE]" --query_file "[PATH_TO_QUERY_FILE]" --kwargs "{[PARAMS_FOR_QUERY]]}"     

```

For instance, to query the wines paired with beef, with boldness 3, run:

```

python matching_values_ordering.py --input "[PATH_TO_RDF_FILE]" --query_file "[PATH_TO_QUERY_FILE]" --kwargs "{\"DISH\": \"kebi:beef\", \"THRESHOLD\": [0, 1, 2], \"BOLDNESS\": \"3\"}"   

```

Other parameters are also present to provide freedom to the user.
To see them, run:
```
python3 matching_values_ordering.py --help
```

After the execution, one file named ```query_results.json``` has been created, containing the ordered results of the runned Prolog queries

