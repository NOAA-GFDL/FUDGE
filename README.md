       Framework for Unified Downscaling of GCMs Empirically (FUDGE)

This file is part of the NOAA-GFDL FUDGE project (Framework for Unified Downscaling of GCMs Empirically), referred to as NOAA-GFDL/FUDGE. The majority of this code was written by authors at the Geophysical Fluid Dynamics Laboratory (GFDL), who are providing the code with the disclaimer shown below.

Disclaimer
----------

The United States Department of Commerce (DOC) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

  What is it?
  -----------

  FUDGE is a tool for exploring the possible space of statistical downscaling methods by 
  running experiments that vary over downscaling method, downscaling method parameters, 
  data used in the downscaling process, and pre- and post-downscaling adjustment
  of the data. 

  The Latest Version
  ------------------

  The latest verified version is the darkchocolate release. For more
  information, please consult the release notes.

  Documentation
  -------------

  The documentation included with this release is located in 
  the documentation/ directory. 

  Requirements
  ------------
  This code has been running on Red Hat Enterprise Linux Server release 6.6 (Santiago)

  This code requires access to:
  - R 2.15 or higher (has been tested in 3.0, but not with the entire workflow)
  -- R packages ncdf4, ncdf4.helpers, CDFt, PCICt, udunits2
  - nco 4.0.3 or higher
  - netcdf 4.0.1 or higher
  - nccmp 1.1 or higher
  - python 2.7.1
  -- python packages pprint,datetime,getopt, os, shutil, shlex, 
     sys, subprocess, optparse, argparse
  - GFDL in-house transfer tools in HPC (gcp 2.3 or higher)
  - moab 7 or higher

  Licensing
  ---------

  Please see the file called license.md

  Warnings
  -----------------------------

  1. This code has been designed for work with the GDFL machines. We make no guarantee that it 
  will still work properly outside of the GFDL. 

  2. This code is a work in progress. The darkchocolate workflow has passed its current tests, 
  but is still undergoing revision. 

  3. In the course of development, we found that the internal data structures were changing too
  fast for unit regression tests to be useful, and switched to regression tests upon the downscaled
  output. However, those regression tests rely on the GFDL filesystem for input data, and the incusion
  of the end-products of downscaling files to check against would have dramatically increased the size 
  of the repository. There is an explanation of the current regression testing procedure in the file 
  Regression_tests/README_regression_tests.md
  

  Contacts
  --------
     o Principle Investigator: Keith Dixon, Keith.Dixon@NOAA.gov
