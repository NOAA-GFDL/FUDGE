#!/usr/bin/python
import os,sys

v_print = None
verbose = 0 #default  
def v_print(sev,verbmsg):
	if(verbmsg == 0):
		#print nothing
	        _v_print = lambda *a: None  # do-nothing function
	else:
	  print verbmsg	
          
        global v_print
def checker(path,cnt=0,sev="error",varname="",verbose=0):
       #actual_cnt = len(os.walk(path).next()[2])
       listfile = []	 
       listbad = []
       append_ok = False
       othercheck = False	
       if (os.path.exists(path+"/mini.success")):
	  print "\033[1;42m mini.success found, other steps in checker to follow.\033[1;m"
	  othercheck = True
       elif (os.path.exists(path+"/mini.fail")):
	  print "\033[1;31m mini.fail found, postProc cannot be executed. \033[1;m" 
          return -13
       else:
	  print "Waiting for status file to be genertated"
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
		    	    	if(statfile.st_size <= 0):
				   print "\033[1;31m ERROR Found file for zero byte. Breaking loop. \033[1;m"
				   return -12		 
			 else:
				if (file.endswith('.success') == False) & (file.endswith('.fail') == False):
					listbad.append(file) 
       		break	
       actual_cnt = len(listfile) 	
       v_print(1,"Total number of .nc files in "+path+varname+"* is:"+(str)(len(listfile)))
       if (cnt > 0):
	 if (cnt != actual_cnt):
	        v_print(1,"Actual Count: "+(str)(actual_cnt))
		v_print(1,"Expected Count: "+(str)(cnt))
		if(sev == "error"):
                	print "\033[1;31m Error code -11: The expected number of files is not present. Please verify!\033[1;m"
		        #sys.exit(-11)
			return -11
		else:
	                v_print(1,"\033[1;43m Warning code -11: The expected number of files is not present. Please verify!\033[1;m")
			return -12
       if (len(listbad) > 0):
	       print "\033[1;31m Error code -10 OOPS: Total number of extraneous files in"+path+"is:\033[1;m",len(listbad)
	       print "\033[1;31mPlease verify this, clean-up directory and try again. CANNOT RUN postProc at this time.\033[1;m"
	       print "List of BAD files",listbad
	       return -10   
       v_print(1,"\033[1;42m checker : complete: success\033[1;m")
       return 0	 
