preserve_attributes <- function(vnames, struct_list, vout){

  out_attlist <- list()
  #if this att is in the input file, it should have the same value in the output file
  #examples: coordinates, standard_name (for SOME CASES)
  copy_atts <- c('coordinates', 'standard_name')
  for (ac in copy_atts){
    for (v in 1:length(vnames)){
      vname <- vnames[v]; struct <- struct_list[[vname]]
      if (ac %in% names(struct[['all_atts']])){
        print(ac)
        out_attlist[[ac]] <- struct[['all_atts']][[ac]]
        break
      }
    }
  }
  
  #if this att is in the input file, it should have instances of $var1 replaced with $var2
  mod_atts <- c('variable_id')
  for (a in mod_atts){
    for (v in 1:length(vnames)){
      is_present <- F
      vname <- vnames[v]; struct <- struct_list[[vname]]
      if (a %in% names(struct[['all_atts']])){
        is_present <- T
      }
      if (is_present && !(a %in% names(out_attlist))){
        modstr <- sub(vname, vout, struct_list[[vname]][['all_atts']][[a]], fixed=T)
        out_attlist[[a]] <- modstr
        is_present <- F
      }
    }
  }
  return(out_attlist)
}

