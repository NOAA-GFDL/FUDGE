#!/usr/bin/python
#
import xml.etree.ElementTree as ET
import sys, subprocess
from subprocess import PIPE
#
#
# This python script constructs the path of SYNTHETIC DATA input files as defined for Fudge darkchocolate.
# If the file is properly set and exists, you will see an '\ls -l' listing.
# If the file does n ot exist, you will not see the listing.
#
# NOTE: If having problems running this script, try doing:  module unload cdat
# 

if len(sys.argv) < 1 :
  print ("!!!!!!!!!!!!!!!!!!!!!!!!!!")
  print ("Need to input XMLfile path")
  print ("Exiting.")
  print ("!!!!!!!!!!!!!!!!!!!!!!!!!!")
  exit(1)

xmlFile = sys.argv[1]
if xmlFile == "":
  print ("!!!!!!!!!!!!!!!!!!!!!!!!!!")
  print ("Need to input XMLfile path")
  print ("Exiting.")
  print ("!!!!!!!!!!!!!!!!!!!!!!!!!!")
  exit(1)


root = ET.parse(xmlFile)
input = root.findall("./input")
grid = root.findall("./input/grid")

# getting values
target         = input[0].get("target")
target_ID      = input[0].get("target_ID")
predictor_list = input[0].get("predictor_list")
#smaskDir       = input[0].get("spat_mask")[0:-5]
smaskDir       = input[0].get("spat_mask")
smask_ID       = input[0].get("spat_mask_ID")
smask          = input[0].get("maskvar")
region    = grid[0].get("region")
lons = root.findtext("./input/grid/lons")
lone = root.findtext("./input/grid/lone")
nOneDFiles = int(lone)-int(lons)+1
lats = root.findtext("./input/grid/lats")
late = root.findtext("./input/grid/late")
file_j_range = root.findtext("./input/grid/file_j_range")
training = root.findall("./input/training")
histPred = root.findall("./input/training/historical_predictor")
histTarg = root.findall("./input/training/historical_target")
futPred  = root.findall("./input/training/future_predictor")
core = root.findall("./core")
methodInfo = root.findall("./core/method")
method = methodInfo[0].get("name")
kfold = int(root.findtext("./core/kfold"))
if kfold <= 9:
  kfold = "0" + str(kfold)

dsMiniArcDir = root.findtext("./core/output/out_dir")
dsDirs = dsMiniArcDir.split("/")
nd = len(dsDirs)
nDimDir = nd - 2
dimDir = dsDirs[nDimDir]

inRootDir = root.findtext("./core/output/in_dir")
scriptRootDir = root.findtext("./core/output/script_root")
exper_series = root.findtext("./core/exper_series")
project = root.findtext("./core/project")
project_ID = root.findtext("./core/project_ID")

varname = target_ID

experiment = project_ID+varname+"p1-"+method+"-"+str(exper_series)+"K"+kfold

#===================================
# get Historical Predicton attributes
#===================================
#
hpTimeWindow = histPred[0].get("time_window")
hpFileYrBeg = histPred[0].get("file_start_time")
hpFileYrEnd = histPred[0].get("file_end_time")
hpTrainYrBeg = histPred[0].get("train_start_time")
hpTrainYrEnd = histPred[0].get("train_end_time")
""" Alternate way to get at attributes and values
hpKeys = histPred[0].keys()
#print hpKeys
hpItems = histPred[0].items()
#print hpItems
#hpTimeWindow = hpItems[0][1]
#hpFileYrEnd = hpItems[1][1]
#hpTrainYrBeg = hpItems[2][1]
#hpFileYrBeg = hpItems[3][1]
#hpTrainYrEnd = hpItems[4][1]
#k=0
#for tags in hpKeys:
#  print k, tags, hpItems[k][0], hpItems[k][1]
#  k=k+1

"""
# create historical predictor path
hpRootDir = "/archive/esd/PROJECTS/DOWNSCALING"
hpSubDirs = histPred[0].findtext("dataset").split(".")
#hpSubDirs.insert(4,"day")
hpVars = predictor_list.split(".")

np=len(hpSubDirs)-1

# create historical predictor path
k=0
hpArcDir = hpRootDir
for dir in hpSubDirs:
   hpArcDir = hpArcDir + "/" + dir
   k=k+1

k=0
for pvar in hpVars:
  hpPath = hpArcDir + "/" + pvar + "/" + region + "/" + dimDir
# print ("HistPredictor Path = " + hpPath)
  hpFileName = hpPath+'/*'+hpFileYrBeg+'*'+hpFileYrEnd+'*.nc'
  p1 = subprocess.Popen('\ls -l '+hpPath+'/*'+hpFileYrBeg+'*'+hpFileYrEnd+'*.nc',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
  tmpFile,hpError = p1.communicate()
  hpFile = tmpFile.strip()
  if(hpError!=""):
    print(hpError)
#   print("Exiting.")
#   sys.exit()
# print ("HistPredictor File "+str(k)+": "+hpFile)
  if k == 0:
    hpFiles = [hpFile]
    hpFileNames = [hpFileName]
  else:
    hpFiles.append(hpFile)
    hpFileNames.append(hpFileName)
  k=k+1 

#print hpFiles
#===================================
# get Historical Target attributes
#===================================
#
htTimeWindow = histTarg[0].get("time_window")
htFileYrBeg = histTarg[0].get("file_start_time")
htFileYrEnd = histTarg[0].get("file_end_time")
htTrainYrBeg = histTarg[0].get("train_start_time")
htTrainYrEnd = histTarg[0].get("train_end_time")

# create historical target path
htArcDir = "/archive/esd/PROJECTS/DOWNSCALING"
htSubDirs = histTarg[0].findtext("dataset").split(".")
#htSubDirs.insert(4,"day")

np=len(htSubDirs)-1

# create historical targ path
k=0
for dir in htSubDirs:
   htArcDir = htArcDir + "/" + dir
   k=k+1

htArcDir = htArcDir + "/" + target + "/" + region + "/" + dimDir
htFileName = htArcDir+'/*'+htFileYrBeg+'*'+htFileYrEnd+'*.nc'
#print ("HistTarget Path = " + htArcDir)
p2 = subprocess.Popen('\ls -l '+htArcDir+'/*'+htFileYrBeg+'*'+htFileYrEnd+'*.nc',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
htInfo = p2.communicate()
htFile = htInfo[0][0:-1]
#print ("htFile = "+htFile)

#===================================
# get Future predictor attributes
#===================================
#
fpTimeWindow = futPred[0].get("time_window")
fpFileYrBeg = futPred[0].get("file_start_time")
fpFileYrEnd = futPred[0].get("file_end_time")
fpTrainYrBeg = futPred[0].get("train_start_time")
fpTrainYrEnd = futPred[0].get("train_end_time")

# create future predictor path
fpArcDir = "/archive/esd/PROJECTS/DOWNSCALING"
fpSubDirs = futPred[0].findtext("dataset").split(".")
#fpSubDirs.insert(4,"day")

np=len(fpSubDirs)-1
fpDataSource=fpSubDirs[2]
fpEpoch=fpSubDirs[3]
fpFreq=fpSubDirs[4]
fpRealm=fpSubDirs[5]
fpMisc=fpSubDirs[6]
fpRIP=fpSubDirs[7]
#print ("fpDataSource = " + fpDataSource)
#print ("fpEpoch = " + fpEpoch)
#print ("fpFreq = " + fpFreq)
#print ("fpRealm = " + fpRealm)
#print ("fpMisc = " + fpMisc)
#print ("fpRIP = " + fpRIP)

# create future predictor path
k=0
for dir in fpSubDirs:
   fpArcDir = fpArcDir + "/" + dir
   k=k+1

fpArcDir = fpArcDir + "/" + target + "/" + region + "/" + dimDir
fpFileName = fpArcDir+'/*'+fpFileYrBeg+'*'+fpFileYrEnd+'*.nc'
#print ("FuturePredictor Path = " + fpArcDir)
p3 = subprocess.Popen('\ls -l '+fpArcDir+'/*'+fpFileYrBeg+'*'+fpFileYrEnd+'*.nc',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
fpInfo = p3.communicate()
fpFile = fpInfo[0][0:-1]
#fpSubDirs.insert(4,"day")

# create downscaled output path
k=0
dsArcDir = dsMiniArcDir+'../'
qcMiniArcDir = dsMiniArcDir+target+'_qcmask/'

p41 = subprocess.Popen('\ls -l '+dsMiniArcDir+'| grep '+target+'_day_'+experiment+'| grep nc | wc -l',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
ds1DFiles,ds1DError = p41.communicate()
numds1DFiles = ds1DFiles.strip()
numds1DError = ds1DError.strip()
strnOneDFiles = str(nOneDFiles)

p42 = subprocess.Popen('\ls -l '+qcMiniArcDir+'| grep '+target+'_qcmask_day_'+experiment+'| grep nc | wc -l',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
qc1DFiles,qc1DError = p42.communicate()
numqc1DFiles = qc1DFiles.strip()
numqc1DError = qc1DError.strip()

if smask!="na":
  p5 = subprocess.Popen('\ls -l '+smaskDir+'/../'+smask+'.nc',shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
  spFileName = smaskDir+'/../'+smask+'.nc'
  spInfo = p5.communicate()
  spFile = spInfo[0][0:-1]

htwFileName = htTimeWindow
p6 = subprocess.Popen('\ls -l '+htTimeWindow,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
htwInfo = p6.communicate()
htwFile = htwInfo[0][0:-1]

p7 = subprocess.Popen('\ls -l '+fpTimeWindow,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
ftwFileName = fpTimeWindow
ftwInfo = p7.communicate()
ftwFile = ftwInfo[0][0:-1]

#===================================
#===================================

logFileName = experiment+".log"
XMLtxt_logFileName = logFileName
logfile = open(logFileName, 'w') 
logfile.write(' ' + "\n")
logfile.write('................................................................' + "\n")
logfile.write('Experiment: ' + experiment + "\n")
logfile.write('XML: ' + xmlFile + "\n")
logfile.write('................................................................' + "\n")
logfile.write(' ' + "\n")
logfile.write('hist_Target_training_yrbeg: ' + htTrainYrBeg + "\n")
logfile.write('hist_Target_training_yrend: ' + htTrainYrEnd + "\n")
logfile.write('hist_Predictor_training_yrbeg: ' + hpTrainYrBeg + "\n")
logfile.write('hist_Predictor_training_yrend: ' + hpTrainYrEnd + "\n")
logfile.write('fut_Predictor_training_yrbeg: ' + fpTrainYrBeg + "\n")
logfile.write('fut_Predictor_training_yrend: ' + fpTrainYrEnd + "\n")
logfile.write('lons: ' + lons + "\n")
logfile.write('lone: ' + lone + "\n")
logfile.write('lats: ' + lats + "\n")
logfile.write('late: ' + late + "\n")
logfile.write("\n")
logfile.write("--------------------\n")
logfile.write("INPUT FILES CHECKING\n")
logfile.write("-------------------- \n")
logfile.write("historical_target_file:  \n")
logfile.write('   ' + htFile + "\n")
fmissing = False
if not htFile:
  logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  logfile.write(">>>>>>>  MISSING  historical_target_file:"+htFileName.strip()+"\n")
  logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  print(">>>>>>>  MISSING  historical_target_file:"+htFileName.strip()+"\n")
  fmissing = True
logfile.write("\n")
k=0
for hpFile in hpFiles:
  logfile.write('historical_predictor_file_'+str(k)+': ' + '\n')
  logfile.write('   ' + hpFiles[k] + '\n')
  if not hpFiles[k]:
    logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    logfile.write(">>>>>>>  MISSING  historical_predictor_file:"+hpFileNames[k].strip()+"\n")
    logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    print(">>>>>>>  MISSING  historical_predictor_file:"+hpFileNames[k].strip()+"\n")
    fmissing = True
  k=k+1
logfile.write("\n")
logfile.write('future_pedictor_file: \n')
logfile.write('   ' + fpFile + "\n")
if not fpFile:
  logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  logfile.write(">>>>>>>  MISSING   future_pedictor_file:"+fpFileName.strip()+"\n")
  logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  print(">>>>>>>  MISSING   future_pedictor_file:"+fpFileName.strip()+"\n")
  fmissing = True
logfile.write("\n")
if smask!="na":
  logfile.write("spatial_mask_file: \n")
  if not spFile:
    logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    logfile.write(">>>>>>> MISSING   spatial_mask_file: " + spFileName.strip() + ": \n")
    logfile.write(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    print(">>>>>>>  MISSING   spatial_mask_file: " + spFileName.strip() + " \n")
    fmissing = True
  logfile.write("   " + spFile + "\n")
  logfile.write("\n")
logfile.write("historical_time_window_file: " + htwFile + "\n")
logfile.write("future_time_window_file: " + ftwFile+ "\n")
logfile.write("\n")
logfile.write("-------------------------- \n")
logfile.write("DOWNSCALED OUTPUT CHECKING \n")
logfile.write("-------------------------- \n")
logfile.write("\n")
logfile.write("downscaled minifile dir:"+dsMiniArcDir+"\n")
logfile.write("qc mask minifile dir:"+qcMiniArcDir+"\n")
logfile.write("\n")
logfile.write("%%%%% NOTE: CHECKS ON DOWNSCALED OUTPUT ARE CONTINGENT ON WHETHER DOWNSCALING JOB HAS RUN.  \n")
logfile.write("\n")
logfile.write('Number_minifiles_expected: ' + str(nOneDFiles) + "\n")
logfile.write('Number_downscaled_minifiles_found: ' + numds1DFiles + "\n")
logfile.write('Number_qcmask_____minifiles_found: ' + numqc1DFiles + "\n")

logfile.write('................................................................' + "\n")
logfile.close()
#print file(logFileName).read()
print ("\n")
print ("      XMLchecker LogFilename = "+XMLtxt_logFileName+"\n")
p99 = subprocess.Popen('cp '+logFileName+' '+XMLtxt_logFileName,shell=True,stdout=PIPE,stdin=PIPE, stderr=PIPE)
#gcp1,gcpErr = p99.communicate()
print ("\n")
if fmissing == True:
 quit(1)
else:
 quit(0)
