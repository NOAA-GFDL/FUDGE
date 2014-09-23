import xml.etree.ElementTree as ET
dictParams = {}
hist_id = []
fut_id = []
esdgen_id = []
predictor_list = []
target = ''
target_id = []
class XMLHandler:
  def __init__(self):
        hist_id = []
  def getParams(self,xml):
	debug = 0
	#xml parser object
	tree = ET.parse(xml)
	
	# returns three major sub-section tags
	if(debug == 1):
     		print "returns three major sub-section tags:"
     		root = tree.getroot()
     		for child in root:
        		 print child.tag

	#returns entire tree with tags and attribs
	if(debug == 1):
     		print "returns entire tree with tags and attribs:"    
     		for node in tree.iter():
        		print node.tag, node.attrib
	# Scan through input tag
	############# Get generic input attribs ##############################
	for input_node in tree.iter('input'):
   		predictor_list = input_node.attrib.get('predictor_list')
                dictParams['predictor_list'] = predictor_list 
    		target = input_node.attrib.get('target')
		dictParams['target'] = target
    		spat_mask = input_node.attrib.get('spat_mask')
    		dictParams['spat_mask'] = spat_mask
    		maskvar = input_node.attrib.get('maskvar')
    		dictParams['maskvar'] = maskvar
	# Get grid information
	############## get grid info ######################################### 
	for grid_node in tree.iter('grid'):
    		output_grid =  grid_node.attrib.get('region')
    		dictParams['output_grid'] = output_grid
    		## dive deep to find lone, lone
    		for lons_node in grid_node.findall('.//lons'):
            		lons = lons_node.text
            		dictParams['lons'] = lons
    		for lone_node in grid_node.findall('.//lone'):
            		lone = lone_node.text
            		dictParams['lone'] = lone
   	## find lats, late (optional)
    		for lats_node in grid_node.findall('.//lats'):
            		lats = lats_node.text
            		dictParams['lats'] = lats
    		for late_node in grid_node.findall('.//late'):
            		late = late_node.text
            		dictParams['late'] = late
                for file_node in grid_node.findall('.//file_j_range'):
                        file_j_range = file_node.text
                        dictParams['file_j_range'] = file_j_range	
         ############### get historical predictor information ###############
         #-- TO DO all the gets in separate functions ##################
		for hist_node in tree.iter('historical_predictor'):
    			hist_file_start_time = hist_node.attrib['file_start_time']
	                dictParams['hist_file_start_time'] = hist_file_start_time
                        hist_file_end_time =  hist_node.attrib['file_end_time']
			dictParams['hist_file_end_time'] = hist_file_end_time
                        hist_train_start_time = hist_node.attrib['train_start_time']
			dictParams['hist_train_start_time'] = hist_train_start_time	
                        hist_train_end_time = hist_node.attrib['train_end_time']
			dictParams['hist_train_end_time'] = hist_train_end_time
		        hist_time_window = hist_node.attrib['time_window']
                        dictParams['hist_time_window'] = hist_time_window
    			for histid_node in hist_node.findall('.//dataset'):
#TODO id lists
        			hist_id.append(histid_node.text)
				dictParams['hist_id'] = hist_id 
	 ################# get historical target information ##################
                for target_node in tree.iter('historical_target'):
                        target_file_start_time = target_node.attrib['file_start_time']
                        dictParams['target_file_start_time'] = target_file_start_time
                        target_file_end_time =  target_node.attrib['file_end_time']
                        dictParams['target_file_end_time'] = target_file_end_time
                        target_train_start_time = target_node.attrib['train_start_time']
                        dictParams['target_train_start_time'] = target_train_start_time
                        target_train_end_time = target_node.attrib['train_end_time']
                        dictParams['target_train_end_time'] = target_train_end_time
                        target_time_window = target_node.attrib['time_window']
                        dictParams['target_time_window'] = target_time_window
                        for targetid_node in target_node.findall('.//dataset'):
                                target_id.append(targetid_node.text)
                                dictParams['target_id'] = target_id
	 ############## get future predictor information ######################
                for fut_node in tree.iter('future_predictor'):
                        fut_file_start_time = fut_node.attrib['file_start_time']
                        dictParams['fut_file_start_time'] = fut_file_start_time
                        fut_file_end_time =  fut_node.attrib['file_end_time']
                        dictParams['fut_file_end_time'] = fut_file_end_time
                        fut_train_start_time = fut_node.attrib['train_start_time']
                        dictParams['fut_train_start_time'] = fut_train_start_time
                        fut_train_end_time = fut_node.attrib['train_end_time']
                        dictParams['fut_train_end_time'] = fut_train_end_time
                        fut_time_window = fut_node.attrib['time_window']
                        dictParams['fut_time_window'] = fut_time_window
			if 'time_trim_mask' in fut_node.attrib:
                        	fut_time_trim_mask = fut_node.attrib['time_trim_mask']
                        	dictParams['fut_time_trim_mask'] = fut_time_trim_mask
                	for futid_node in fut_node.findall('.//dataset'):
                        	fut_id.append(futid_node.text)
                                dictParams['fut_id'] = fut_id
        ###### Get core method information  #####
        for core_node in tree.iter('core'):
	    for method_node in core_node.findall('.//method'):
		method = method_node.attrib.get('name')
	        dictParams['method'] = method	
            for exper_node in core_node.findall('.//experiment'):
	        dictParams['experiment'] = exper_node.text
            for proj_node in core_node.findall('.//project'):
                dictParams['project'] = proj_node.text
            for series_node in core_node.findall('.//exper_series'):
                dictParams['series'] = series_node.text

            for basedir_node in core_node.findall('.//basedir'):
                basedir = basedir_node.attrib.get('name')
                dictParams['basedir'] = basedir 
            for fold_node in core_node.findall('.//kfold'):
		kfold = fold_node.text.strip()
                dictParams['kfold'] = kfold 
            for output_node in core_node.findall('.//output'):
	       for rootdir_node in output_node.findall('.//root'):
                  oroot = rootdir_node.text
                  dictParams['oroot'] = oroot 
               for version_node in output_node.findall('.//version'):
                  dversion = version_node.text
                  dictParams['dversion'] = dversion 
	#### Get custom method params ##############
        for custom_node in tree.iter('custom'):
	    listParams = ''	
	    nparam=0 #total number of custom params
            for params_node in custom_node:
	        nparam = nparam + 1
		params = params_node.text	
	#	dictParams[params_node.tag]=params
		if(nparam > 1):
	           delimit = ","
		else:
		   delimit = ''	 
	        #listParams = listParams + delimit + ""+params_node.tag+"='"+params+"'"
		listParams = listParams + delimit + ""+params_node.tag+"="+params+""
	        dictParams['params']=listParams
	if(debug == 1):
       		 for x,v in dictParams.iteritems():
                         print x,v
		
	return dictParams



