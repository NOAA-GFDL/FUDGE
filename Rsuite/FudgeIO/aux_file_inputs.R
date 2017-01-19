read_stencil_data <- function(stencil_file, delim="none"){
  if (stencil_file == "none"){
    IDIM=1
    JDIM=1
    stencil=1
  }else{
    if (delim=='none'){
      #First line is the IDIM,JDIM specification
      data <- trimws(readLines(stencil_file))
      dimspec <- sub(" ", "", data[1])
      dimstring <- unlist(strsplit(dimspec, ","))
      for (n in dimstring){
        n.split <- unlist(strsplit(n, "="))
        assign(n.split[[1]], as.numeric(n.split[[2]]))
      }
      stencil <- c()
      split_line <- function(x){
        substring(x, seq(1,nchar(x),1), seq(1,nchar(x),1))
      }
      for (n in 2:length(data)){
        if (data[n] != ""){
          stencil <- c(stencil, as.numeric(split_line(data[n])))
        }
      }
      try(dim(stencil) <- c(IDIM, JDIM), silent=F)
    }else{
      #First line is the IDIM,JDIM specification
      dimspec <- readLines(stencil_file, n=1)
      dimspec <- sub(" ", "", dimspec)
      dimstring <- unlist(strsplit(dimspec, ","))
      for (n in dimstring){
        n.split <- unlist(strsplit(n, "="))
        assign(n.split[[1]], as.numeric(n.split[[2]]))
      }
      #For each line after
      stencil <- read.delim(stencil_file, sep=" ", comment.char = "#", 
                            skip=1, header=F, strip.white=T)
      #Note to self: extra whitespace gets counted as NA values. 
      #" " might not be the best delimeter.
      #Add basic checks 
      test.dim <- c(IDIM, JDIM)
      if (any(dim(stencil) != test.dim)){
        stop(paste("Error reading in stencil", stencil_file, 
                   ": IDIM, JDIM did not agree with array dims!"))
      }
    }
  }
  #Check to make sure that center of stencil is present
  return(list(idim=IDIM, jdim=JDIM, stencil=stencil))
}

get_aux_params <- function(aux_file, copy_from_tmpdir=F){
  #Comment-stripping is a work in progress.
  #   aux_tmp <- readLines(aux_file)
  #   print(aux_tmp)
  #   aux_text <- c()
  #   print("Starting iteration")
  #   for (line in aux_tmp){
  #     print(line)
  #     if (! grepl("#", line)){
  #       append(aux_text, line)
  #     }
  #   }
  #   print(aux_text)
  #Read in as json
  aux_list <- fromJSON(aux_file)
  output <- list()
  #First get the global vars
  output$ds_handle_missing <- aux_list$ds_handle_missing
  dataset_names <- c("OH", "MH", "MF")
  for (n in dataset_names){
    el <- aux_list[[n]]
    out <- list()
    out$geomask <- el$geomask
    out <- c(out, read_stencil_data(el$stencil))
    output[[n]] <- out
  }
  return(output)
}