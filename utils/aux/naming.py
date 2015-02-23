#!/usr/local/python/2.7.3/bin/python
import naming
import os
import re, sys, subprocess

#{p=GFDL-PPnodes, z=Zeus, g=gaea, s=Sooner, k=kd workstation, etc.}.
def constructExpname(lname_project,lname_target,series,method,kfold,basedir,dumb="yes"):
	expconfig = ""
	mapdir = basedir+"/utils/auxfiles/"
	proj_mappings = mapdir+"project_map"
	f = open(proj_mappings,'r')
	sname_project = ''
	if (dumb == "no"):
 		for line in f:
   			if  re.match('^longname:'+lname_project,line):
    				fields=line.split(',')
    				lname_project = fields[0].split(':')[1]
    				sname_project = fields[1].split(':')[1].strip()
      		if(sname_project == ''):
			print "project",lname_project,"not found in project_map."
			sname_project = lname_project
			print "Using ",lname_project," as default for sname_project(short name)"
		#print sname_project
		f.close()
        	target_mappings = mapdir+"target_map"
        	f = open(target_mappings,'r')
        	sname_target = ''
        	for line in f:
                	if  re.match('^longname:'+lname_target,line):
                        	fields=line.split(',')
                        	lname_target = fields[0].split(':')[1]
                        	sname_target = fields[1].split(':')[1].strip()
        	if(sname_target == ''):
                	print "target",lname_target,"not found in target_map."
                	sname_target = lname_target
                	print "Using ",lname_target," as default for sname_target(short name)"
       		# print sname_target
		f.close()
        if (dumb == "yes"):
	      sname_project = lname_project
              print "Dumb option -- Using ",sname_project," as project_ID"
	      sname_target = lname_target 
              print "Using ",sname_target," as target_ID"

	## get platform code
	system,node,release,version,machine = os.uname()
	if(node.startswith('pp')):
		print "Running on PP(PAN) node", node
		plat = "p1" 
	elif(node.startswith('an')):
                print "Running on AN(PAN) node", node
		plat = "p1"
	else:
                print "Running on a workstation", node
	        plat = node 
                print "\033[1;41mERROR code -5: Running on a workstation not tested for end-to-end runs yet. Please login to analysis nodes to run fudgeList. \033[1;m",plat
		sys.exit(-5)


	plat.strip()
        if(int(kfold) < 10):
	        #print "kfold < 10 ......................." 
       	 	k = "K0"+kfold
   	else:
		k = "K"+kfold
        expconfig = sname_project+sname_target+plat+"-"+method+"-"+series+k  #eg kd_PMtxp2-GFDL-ARRMv3-B00X01K02
	#print expconfig
	return expconfig,sname_project
  
 
