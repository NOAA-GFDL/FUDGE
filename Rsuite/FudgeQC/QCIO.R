#QCIO.R
#'Checks the input and output file destinations for existance;
#'halts the script and throws errors if they are not. 
#'@param output.dir: The output directory to which downscaled data will be written.
#'

QCIO <- function(output.dir, ...){
  if (! file.exists(output.dir)){
    stop(paste("Output directory error: Directory at", output.dir, "does not exist. Please check the path."))
  }
}