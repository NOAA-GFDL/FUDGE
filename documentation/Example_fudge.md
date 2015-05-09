The below workflow is designed to run on the Geophysical Fluid Dynamics Laboratory (GFDL) Post-Processing and Analysis Nodes (PAN) only at this time. Attempts to run the entire workflow outside of the GFDL are not reccomended at this time.

1. First, initialize the environment variables that you need for your run. This can be done one of two ways: 
1a. Load the appropriate module from the ESD role account
  First source the local .cshrc file: 

     source ~/local/.cshrc_esd

  Then, check for module availability: 

     module avail fudge

  Load the module of your choice and go on from that point
 
1b. Set BASEDIR in setenv_fudge and source setenv_fudge
  setenv_fudge is located within the top level of the FUDGE directory structure. Edit the $BASEDIR variable to
  point to the current location of the FUDGE repository on the system. 

     setenv BASEDIR "/home/a1r/gitlab/tasmax/fudge2014/" 

  Then, source the setenv_fudge file to set the environment variables and aliases. 
     source setenv_fudge


2. Prepare experiment XML
sample: /home/a1r/gitlab/tasmax/fudge2014/utils/xml/cew/vanilla.seasonal2.tasmin.xml


3. Run expergen to produce runscripts:
expergen is now found within fudge2014/bin/

Usage:

   expergen -i ../utils/xml/vanilla.seasonal2.tasmin.xml

Running the above will out a list of master scripts that can be submitted as a MOAB job. Next steps found below- 

Runscripts and R-log files are now placed in [script_root]/scripts/$subproject/$fudgeexperimentname/
The script_root is retrieved from the XML as a user-defined prefix.

4. Script to Submit MOAB Jobs
The submit_job script is found in fudge2014/bin/
Usage:
   Please provide 3 arguments: master_script_dir lon_start lon_end.
   submit_job /nbhome/a1r/tests/scripts/RR/RRtna1r-CDFt-A38z-mL01K00/master/ 181 370

It is also possible to use the --msub option with expergen in order to automate submission of the job scripts. 

5. Monitor Job
showq | grep USERNAME

  showq -c |grep Aparna

  gfdl.5965056        C 0             p-0 ------      1.0 wi Aparna.Ra        f pp047.princeton.     2    00:01:02   Sat Sep  6 17:45:54

sample output location:
/work/a1r/testing/tn//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtnp1-CDFt-A38sL01K00/tasmin/RR/OneD/v20140108/
 
MOAB job logs can be found in : /work/$USER/fudge/stdout/

You can tell how many of the jobs have been successfully completed from the number of output minifiles in the output directory: 
ls /work/a1r/testing/tn//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtnp1-CDFt-A38sL01K00/tasmin/RR/OneD/v20140108/ | wc -l

An empty folder should return 1. A complete job over the entire Red River region should return 191. 

If there are fewer minifiles in the output directory than the XMl specifies (via lons and lone), postProc will return an error stating the number of files that it found, and the bumber of files that it expected. 

6. After the MOAB jobs complete successfully, you should be post-process the output by using the tool: postProc. When expergen was finished running, a message should have printed to the screen showing the full form of a postProc command that can be used to concatenate the output. If using post Proc on your own , the -i option is used to specify a path to the XML that described your experiment, and the -v option is used to specify the variables of interest for concatenation.

postProc -i ../utils/xml/vanilla.seasonal2.tasmin.xml -v "tasmin,tasmin_qcmask"

If you are re-post-processing a set of minfiles that failed, either remove the previous catted output, or use the -f option to force writing the output:

postProc -fi ../utils/xml/vanilla.seasonal2.tasmin.xml

Sample final output directory structure: 

/work/a1r/testing/tn//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRtnp1-CDFt-A38sL01K00/tasmin/RR/v20140108/tasmin_day_RRtnp1-CDFt-A38sL01K00_rcp85_r1i1p1_RR_20060101-20991231.nc 
