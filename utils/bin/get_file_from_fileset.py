#!/bin/python
#get_file_from_fileset.py

def get_file_from_fileset(file_dict, var='prime'):
   fkeys = list(file_dict.keys())
   num_entries = len(fkeys)
   try:
     retdict = file_dict[var]['file']
   except KeyError:
     if var == 'prime' and num_entries == 1: 
       retdict = file_dict[fkeys[0]]['file']
     else:
       print("Error in get_file_from_fileset: specified var " + var + 
             " not found in fileset keys " + ",".join(fkeys) + "!")
   return retdict
