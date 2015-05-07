  Readme for Regression_tests

  What is it?
  -----------
  The regression tests for FUDGE are based off of bitwise comparisons of downscaled NetCDF output, 
  with limited metadata comparisons via the nccp utility. 

   Usage
   -----

  The main driver for running regression tests is run_FUDGE_regression_tests.py. It takes as an argument: 

  ``` -i : a list of files to be input, with '#' marking comments to be ignored by the parser
  ``` -s : whether or not to store results. If set to false (the default), results are written to a temporary directory. This is useful for the pre-push scripts, but bad for the full regression tests - logs are useful for that. 
  ``` -o : if -s is set to true, the location of a directory to which to write the results. If you are running a full test for the ESD team, should be /archive/esd/REGRESSION_TESTS
  ``` -r : the file to which the summary results (including PASSED and FAILED) will be written. Useful for figuring out what exactly went wrong, and how it differs from last time.

  Only the -i and -r options are required for running the regression tests.

  Example without save option: 
  python Regression_tests/run_FUDGE_regression_tests.py -i /home/cew/Code/fudge2014/Regression_tests/pre_push_regtests -r /home/cew/Code/fudge_regtests.txt

  Example with save option: 
  python Regression_tests/run_FUDGE_regression_tests.py -i /home/cew/Code/fudge2014/Regression_tests/full_regtests -s True -o /archive/esd/REGRESSION_TESTS/new_output -r /home/cew/Code/fudge_regtests.txt

  Adding new tests
  ---------------- 

  Tests are organized in the Regression_tests directory by file type: xmls are located in the Regression_tests/xmls/ directory, and runcodes are located in Regression_tests/runcodes/. Each test consists of a line of text of the following type: 

  $name_of_test_file $name_of_comparison_file $type_of_file
  
  name_of_test_file: The name of the file containing the test instructions, either located in Regression_tests/xmls, or Regression_tests/runcodes
  name_of_comparison_file: The name of the file that the results will be compared against. For the xmls, it is only neccessary to include the basename of the comparison file; the rest of the directory structures should be deduced from the directory structure output by the xml.
  type_of_file: one of 'runcode' for a runcode test designed to exercise the R code only, or 'xml' for an xml designed to exercise the entire FUDGE workflow. Runcode tests are all located in the Regression_tests/runcodes directory, and xml tests are all located in the Regression_tests/xmls directory.

  Note that the sample runcodes and xmls have OUTPUT_DIR and SCRIPT_DIR marked in the instructions; these get replaced depending upon whether tests are being added to the records or added to a temporary directory. If adding more tests, please add the OUTPUT_DIR and SCRIPT_DIR in the correct places.

  Please note that extra blank lines at the end of the file will cause the python utility to exit with an error. 

  #Expected dir structure of /archive/regression tests

  new_output: The latest output from the downscaling experiment, located underneath a timestamped directory

  old_output: The original output. Individual minfile runs for the R code are located under the top level, while the xml tests use the output directory structure
