#!/usr/bin/python
import os,sys,re

v_print = None
verbose = 0 #default
def v_print(actualverbopt,expectedverbopt,verbmsg):
	if(actualverbopt == 0):
	        _v_print = lambda *a: None  # do-nothing function
	else:
	  if(expectedverbopt == 1 ):
	     print verbmsg
          else:
             _v_print = lambda *a: None  # do-nothing function

        global v_print
def checker(path,cnt=0,sev="error",varname="",verbose=0, spath="na"):
       #actual_cnt = len(os.walk(path).next()[2])
       listfile = []
       listbad = []
       append_ok = False
       othercheck = False
       print(spath)
       spath = re.split(":", spath)[1]
       mini_statdir = os.path.join(spath, 'status', 'minifile')
       if os.path.isdir(spath):
          out_contents = os.listdir(spath)
       else:
           out_contents = ["none"]
       if (any([re.search(".*fail", el) is not None for el in os.listdir(mini_statdir)])):
	  	  print "\033[1;31m .*fail found in output dir, postProc cannot be executed. \033[1;m"
	  	  return -13
       elif spath != 'na' and (any([re.search('.fail', el) is not None for el in os.listdir(mini_statdir)]) or os.path.exists(spath +"/status/.meta.fail") ):
	  	  print "\033[1;31m $status/minifile/.*.fail or .meta.fail found in script dir, postProc cannot be executed. \033[1;m"
	  	  return -13
       elif (any([re.search(".*success", el) is not None for el in os.listdir(mini_statdir)])):
	  	  othercheck = True
	  	  print "\033[1;42m *.mini.success found, other steps in checker to follow.\033[1;m"
       else:
	  v_print(1,verbose,"INFO:Waiting for status file to be generated "+mini_statdir)
          return 14
       if (othercheck == True):
         for ROOT,DIR,FILES in os.walk(path):
       		for file in FILES:
      			 if file.endswith('.nc'):
		            if (varname != ""):
			      if(file.startswith(varname)):
			        append_ok = True
			    else:
				append_ok = True
			    if(append_ok == True):
          		    	listfile.append(file)
			    	statfile = os.stat(path+"/"+file)
		    	    	#if(statfile.st_size <= 0):
				   #print "\033[1;31m ERROR Found file for zero byte. Breaking loop. \033[1;m"
				   #return -12
			 else:
				if (file.endswith('.success') == False) & (file.endswith('.fail') == False):
					listbad.append(file)
       		break
       actual_cnt = len(listfile)
       v_print(1,verbose,"INFO:Total number of .nc files in "+path+varname+"* is:"+(str)(len(listfile)))
       if (cnt > 0):
	 if (cnt != actual_cnt):
	        v_print(1,verbose,"Actual Count: "+(str)(actual_cnt))
		v_print(1,verbose,"Expected Count: "+(str)(cnt))
		if(sev == "error"):
                	print "\033[1;31m Error code -11: The expected number of files is not present. Please verify!\033[1;m"
		        #sys.exit(-11)
			return -11
		else:
	                v_print(1,verbose,"\033[1;43m Warning code -11: The expected number of files is not present. Please verify!\033[1;m")
			return -12
       if (len(listbad) > 0):
	       print "\033[1;31m Error code -10 OOPS: Total number of extraneous files in"+path+"is:\033[1;m",len(listbad)
	       print "\033[1;31mPlease verify this, clean-up directory and try again. CANNOT RUN postProc at this time.\033[1;m"
	       print "List of BAD files",listbad
	       return -10
       v_print(1,verbose,"\033[1;42m checker : complete: success\033[1;m")
       return 0
