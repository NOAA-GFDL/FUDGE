#!/usr/bin/python

import sys,re

def get_commit_severity(commitstring):
  '''
   Gets the severity of the FUDGE regression tests that we are running
   depending on the structure of the tag (or branch) that was pushed.
   Severity 1: All tags and branches; no specific form
     These run the minifile tests. Most features in the FUDGE R code have at
     least one test in the minifile tests to make sure that they still run;
     these run on minifile slices of the entire grid, so coverage isn't great.
     Still, if it fails in the minifile tests and you haven't knowingly changed answers, you do NOT want to go on to the next step.
   Severity 2: Testing tags, general for ${version}-testing-${datestring}
     These tests include the postproc workflow, which includes anomaly addition, ensemble generation and some kinds of cross-validated experiment generation. They operate on the entire grid, so there's generally mroe coverage of possibly problematic points;
   Severity 3: Production tags, of the general form ${version}-${datestring}
     These run the tests that are expensive; right now, the only test that qualifies is our large-data test for the USAR6 data. It takes a long time to run and used to break parts of our workflow impressively; it's worth the effort for production, but anything else is pushing it.
  More levels may be added as events warrant.
  '''
  sev = 1
  csplit = re.split('-', commitstring)
  if len(csplit) > 2:
    if csplit[1] == 'testing' and is_datestring(csplit[2]):
      sev = 2
  elif len(csplit) == 2:
    if is_datestring(csplit[1]):
        sev = 3
  print(sev)

def is_datestring(datestr):
    # Is the string parseable as a YYYMMDD date?
    ndigit = sum(c.isdigit() for c in datestr)
    return(ndigit == 8)

def run_commit_tests():
    '''
    Runs a set of tests to make sure the severity interpretation
    is working as expected
    '''
    test1 = "jamoca-20190425"
    test1_sev = get_commit_severity(test1)
    print(test1)
    print((str(test1_sev)))
    test2 = 'icing-testing-20190401'
    test2_sev = get_commit_severity(test2)
    print(test2)
    print((str(test2_sev)))
    test3 = "icing-testing-20190301-01"
    test3_sev = get_commit_severity(test3)
    print(test3)
    print((str(test3_sev)))
    test4 = "icing-testing-cew-20190405"
    test4_sev = get_commit_severity(test4)
    print(test4)
    print((str(test4_sev)))
    test5 = 'parallel_postproc'
    test5_sev = get_commit_severity(test5)
    print(test5)
    print((str(test5_sev)))

if __name__ == '__main__':
    if len(sys.argv) > 1:
      get_commit_severity(sys.argv[1])
    else:
      run_commit_tests()
