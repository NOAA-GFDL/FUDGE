    XMLgen.csh

    What is it?
    -----------

    XMLgen is a darkchocolate-version tool to create XML describing a downscaling experiment. XMLgen is a multi-script tool run using $xmlgenDir/XMLgen.csh. It can create XML for Synthetic or SCCSC-region data used for RedRiver (0.1x0.1grid) or Perfect Model C360. It can be run interactively or, if you already have a file containing keyboard inputs, run with that keyboard input file as standard input.

    User parameters
    ---------------
    Before running XMLgen, you need to edit the setenv_fudge file to point to the current location of the top-lvel FUDGE directory on your system, to set the $BASEDIR. You also need to set $workDir in line 16 of XMLgen.csh.

    Usage
    -----

    For interactive use, type: $xmlgenDir/XMLgen.csh  , where $xmlgenDir is the directory in which XMLgen.csh and the component scripts are located. If using the setenv_fudge file, this is equivalent to $BASEDIR/XMLgen/XMLgen.csh. 

    Once run, the script tells you where the XML file and other supplemental files are located. These files are:
        $experimentName.xml ==> XML file, placed in /home/esd/PROJECTS/DOWNSCALING/SUBPROJECTS/$projectID/XML, which $projectID is prompted for by XMLgen.csh
        $experimentName.input.txt ==> captured keyboard inputs, placed in /home/esd/PROJECTS/DOWNSCALING/SUBPROJECTS/$projectID/XMLtxt
        This file captures the responses to XMLgen.csh's queries, whether interactive keyboard inputs or standard input from a file. Using it as standard input to XMLgen.csh should produce the indentical XML file as produced with the initial inputs. Edited, it will create a modified XML file, but BE VERY CAREFUL and ONLY do this if you know what you are doing.
        IT IS STRONGLY RECOMMENDED THAT YOU RUN XMLgen.csh INTERACTIVELY TO PRODUCE XML.
        Different variables may require different processing options, so you may not get the experimental design you need by doing this! tasmax and tasmin are easily interchangable, but tasmax and, say, pr, are not.
        $experimentName.input.txt.key ==> a more verbose & informative keyboard input version, placed in /home/esd/PROJECTS/DOWNSCALING/scripts/$projectID/XMLtxt
        This file has the keyboard inputs plus information about which variables are set in the XMLgen code and in which function/sub-script the input was queried. Using this requires becoming familiar with XMLgen. The ".input.txt.key" file CAN NOT be used as standard input because of the extra information it contains.)
        $experimentName.log ==> XMLchecker.py output parsing the XML that was just created, place in /home/esd/PROJECTS/DOWNSCALING/scripts/$projectID/XMLtxt. Check this file to confirm the input files and settings for the XML are what you are expecting. (fullpath provided at end of XMLgen.csh run.)
    XMLchecker.csh (NOTE: Runs on workstation or Analysis Nodes)

    For Parsing/Checking XML; Used by XMLgen, but can be used stand-alone. Lets user parse darkchocolate XML files and outputs the list of input files & more

    To USE, type: $xmlgenDir/XMLchecker.py [your_XML_file]
    Output is placed in /home/esd/PROJECTS/DOWNSCALING/scripts/...[project_ID in XML].../XMLtxt .

  Warnings
  --------

  1. This code has been designed for work with the GDFL machines. We make no guarantee that it 
  will still work properly outside of the GFDL. 

