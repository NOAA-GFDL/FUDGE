#!/usr/bin/python
import os,sys
def checker(path,cnt=0):
       #actual_cnt = len(os.walk(path).next()[2])
			
       listfile = []	 
       listbad = []
       for ROOT,DIR,FILES in os.walk(path):
       		for file in FILES:
      			 if file.endswith('.nc'):
          			listfile.append(file)
				statfile = os.stat(path+"/"+file)
				if(statfile.st_size <= 0):
				   print "\033[1;31m ERROR Found file for zero byte. Breaking loop. \033[1;m"
				   sys.exit(-12)		 
			 else:
				listbad.append(file) 
       		break	
       actual_cnt = len(listfile) 	
       print "Total number of .nc files in "+path+"is:",len(listfile)
       if (cnt > 0):
	 if (cnt != actual_cnt):
	        print "Actual Count: ",actual_cnt
		print "Expected Count: ",cnt 
                print "\033[1;31m Error code -11: The expected number of files is not present. Please verify!\033[1;m"
		sys.exit(-11)
       if (len(listbad) > 0):
	       print "\033[1;31m Error code -10 OOPS: Total number of extraneous files in"+path+"is:\033[1;m",len(listbad)
	       print "\033[1;31mPlease verify this, clean-up directory and try again. CANNOT RUN postProc at this time.\033[1;m"
	       print "List of BAD files",listbad
	       sys.exit(-10)   	
       print "checker : complete: success"  				
