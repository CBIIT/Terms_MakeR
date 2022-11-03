# Terms_MakeR
This script takes the model property file from a CBIIT data model and creates a set of term lists, term dictionaries and a list of terms to add to specific properties, specific to caDSR values at this time.


Run the following command in a terminal where R is installed for help.
```
Rscript --vanilla Terms_MakeR.R -h
Usage: Terms_MakeR.R [options]

Terms_MakeR.R version 2.0.1

Options:
	-p CHARACTER, --property=CHARACTER
		Model property file yaml

	-h, --help
		Show this help message and exit
```

An example model property yaml file for this script can be found in the CCDI data model directory: https://github.com/CBIIT/ccdi-model/tree/main/model-desc
