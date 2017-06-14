get.MF.experiment.atts <- function(in.file){
  #Gets experiment attributes based off of the 
  #attributes present in the input file
  #Currently searches for: 
  #ds_scenario
  #ds_MF_source
  #is_PM <- grepl(strsplit(in.file, "/")[[1]][7], "^NCPP", perl=T)
  scenario = 'none'
  dataset = 'none'
  in.nc <- nc_open(in.file)
  exp_name <- ncatt_get(in.nc, 0, 'gfdl_experiment_name')
  is_PM <- exp_name$hasatt
  if(is_PM){
    #Get dataset name
    tmp.split <- strsplit(exp_name$value, "_")
    #If there are 5 elements, scenario is last 2; 
    #if there are 3 elements, scenario is last 1
    if (length(tmp.split[[1]])==5){
      scenario <- tmp.split[[1]][4]
      dataset <- paste(tmp.split[[1]][1:3], collapse="_")
    }else if (length(tmp.split[[1]])==3){
      scenario <- tmp.split[[1]][3]
      dataset <- paste(tmp.split[[1]][1:2], collapse="_")
    }else{
      stop(paste("Error in metadata parsing functions: please check att formatting!"))
    }
#     dataset <- ncatt_get(in.nc, 0, 'model_id')$value
#     #scenario needs forcing and model
#     scenario <- paste(ncatt_get(in.nc, 0, 'experiment_id')$value, 
#                        exp_name$value)
  }else{
    #Assume the 3^5 hogepoge of observational datasets
    #model_id, experiment_id
    scenario = ncatt_get(in.nc, 0, 'experiment_id')$value
    dataset <- ncatt_get(in.nc, 0, 'model_id')$value
  }
  nc_close(in.nc)
  return(list(ds_scenario = scenario, 
              ds_MF_source = dataset))
}

get.OH.experiment.atts <- function(in.file){
  #'Gets experiment attributes based off of the atts
  #'in the input file, assumed to be OH.
  #'Currently searches for: 
  #'ds_OH_source
  #is_PM <- grepl(strsplit(in.file, "/")[[1]][7], "^NCPP", perl=T)
  in.nc <- nc_open(in.file)
  exp_name <- ncatt_get(in.nc, 0, 'gfdl_experiment_name')
  is_PM <- exp_name$hasatt
  if(is_PM){
    #Get dataset name
    #dataset <- ncatt_get(in.nc, 0, 'model_id')$value
    tmp.split <- strsplit(exp_name$value, "_")
    #If there are 5 elements, scenario is last 2; 
    #if there are 3 elements, scenario is last 1
    if (length(tmp.split[[1]])==5){
      scenario <- tmp.split[[1]][4]
      dataset <- paste(tmp.split[[1]][1:3], collapse="_")
    }else if (length(tmp.split[[1]])==3){
      scenario <- tmp.split[[1]][3]
      dataset <- paste(tmp.split[[1]][1:2], collapse="_")
    }else{
      stop(paste("Error in metadata parsing functions: please check att formatting!"))
    }
  }else{
    #Assume the 3^5 hogepoge of observational datasets
    #daymet can be obtained from source attribute:
    #/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/daymet/historical/day/atmos/day/r0i0p1/v2p1/tasmax/SCCSC0p1/tasmax_day_daymet_historical_r0i0p1_SCCSC0p1_19800101-20051231.nc
    #livneh can be obtained from sources attribute:
    #/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/livneh/historical/day/atmos/day/r0i0p1/v1p2/tasmax/SCCSC0p1/tasmax_day_livneh_historical_r0i0p1_SCCSC0p1_19610101-20051231.nc
    #...and title, for that matter.
    #prism can get title, too.
    #/archive/esd/PROJECTS/DOWNSCALING/OBS_DATA/GRIDDED_OBS/prism/historical/day/atmos/day/r0i0p1/v1/tasmax/SCCSC0p1/tasmax_day_prism_historical_r0i0p1_SCCSC0p1_19810101-20051231.nc
    data_title <- ncatt_get(in.nc, 0, 'title')
    if (data_title$hasatt){
        dataset <- strsplit(data_title$value, " ")[[1]][2]
    	dataset <- sub(",", "", dataset)
    }else{
		dataset = 'none'
	}
  }
  nc_close(in.nc)
  return(list(ds_OH_source = dataset))
}

get.FUDGE.file.atts <- function(OH.files, MH.files, MF.files){
  datasets <- c(OH.files, MH.files, MF.files)
  var.names <- c('OH_filenames', 'MH_filenames', 'MF_filenames')
  for (e in 1:length(datasets)){
    el <- datasets[e]
    dset.filenames <- sapply(el, '[[', 'filename')
    dset.filenames <- paste(sapply(basename(dset.filenames), 'paste'), collapse="; ")
    assign(var.names[e], dset.filenames)
  }
  return(list(filenames_OH = OH_filenames, filenames_MH = MH_filenames, filenames_MF = MF_filenames))
}
