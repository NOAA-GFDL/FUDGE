# task1_transform_kdv6.R
# phase 1 of data transformation development for FUDGE
# author: Keith Dixon
# created:8/01/2014


# ----- Begin defining function TransformData that controls the processing
#       of multiple options for data transforms and back-transforms

# + + + begin defining function Transform + + +
# initally, the only option is to perform a square root transform, but
# we can add more transform methods later (e.g., gamma distrib fit + Box-Cox)
#
# Arg:
#   vector.in:  numerical vector containing the input data values.
#   masks.in:   logical vector of same length as vector.in containing masks
#               TRUE for wetdays and FALSE for non-wetdays
#   opt.value:  single element numerical vector holding the optoin value
#               that indicates whether a transform (opt.value > 0) or
#               a back-transform (opt.value <0) is to be performed, and
#               the specific transform-type that is desired.
#               current options include:
#               1 = square root transform; -1 = square root back-transform
#               2 = gamma distribution transform; -2 = gamma back-transform
#   vector.out: numerical vector containing the data values after processing.

TransformData <- function(vector.in, masks.in, opt.value) {

### NOTE: as currently written, this function does not make use of masks.in
  
# --- Begin square root transformation section 
# opt.value == 1 means to apply the square root transform 
  if (opt.value == 1) {vector.out <- sqrt(vector.in)
  }
# opt.val == -1 means to back-transform from the square root
  if (opt.value == -1) {vector.out <- vector.in^2
  }
  
# --- Begin gamma distribution transformation section 
# ...future work...
# opt.value == 2 means to apply the gamma transform 
  if (opt.value == 2) {stop("Gamma distribution transform is under development.")
  }
# opt.val == -2 means to back-transform from the square root
  if (opt.value == -2) {stop("Gamma distribution back-transform is under development.")
  }
  
  return (vector.out)}
# + + + end defining function transform + + +



