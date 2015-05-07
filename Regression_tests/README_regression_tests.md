Readme on running and writing regression tests for FUDGE. 

The main driver for running regression tests is run_FUDGE_regression_tests.py. It takes as an argument: 

-i : a list of files to be input, with '#' marking comments to be ignored by the parser
-s : whether or not to store results. If set to false (the default), results are written to a temporary directory. This is useful for the pre-push scripts, but bad for the full regression tests - logs are useful for that. 
-o : if -s is set to true, the location of a directory to which to write the results. If you are running a full test for the ESD team, should be /archive/esd/REGRESSION_TESTS
-r : the file to which the summary results (including PASSED and FAILED) will be written. Useful for figuring out what exactly went wrong, and how it differs from last time.

Only the -i and -r options are required for running the regression tests.

Example without save option: 
python Regression_tests/run_FUDGE_regression_tests.py -i /home/cew/Code/fudge2014/Regression_tests/pre_push_regtests -r /home/cew/Code/fudge_regtests.txt

Example with save option: 



Adding new tests: 

Tests are organized in the Regression_tests directory by file type: xmls are located in the Regression_tests/xmls/ directory, and runcodes are located in Regression_tests/runcodes/


#Expected dir structure of /archive/regression tests

new_output: The latest output from the 

old_output: The previous output from the downscaling operation. Essentially consists of the content of new_output underneath a timestamped file. 

orig_output : The original downscaled output.  The XML results should include the modified directory structure to go along with it.
