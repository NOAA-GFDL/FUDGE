# task1_wetdayid.R
# phase 1 of data transformation development for FUDGE
# author: Keith Dixon
# originally created:8/04/2014
# latest edits by Keith Dixon:8/05/2014

# ----- Begin defining function WetDayID that applies logic to classify
#       individual days as being a "wetday" or not according to a 
#       used-specified approach.

# Arg:
#   vector.in:  numerical vector containing the input data values.
#   opt.value:  single element numerical vector holding the option value
#               that indicates what method or criterion should be used
#               to make the binary determination of whether a daily precip
#               amount is categorized as a wetday or not.
#               0 means use a criterion of 0.0 precip units
#               1 means use criterion of 0.1 mm per day
#               2 means use criterion of 0.01 inch per day
#
#   is.wetday:        logical vector containing masks indentifying wetdays 
#                     as TRUE and non-wet days as FALSE
#   threshold.wetday: single element numerical vector storing the threshold
#                     (in MKS units) used to indentify wetdays

WetDayID <- function(vector.in, opt.value) {
  
# --- Begin section that uses opt.value to set the criterion to be used
#     to distinguish between wet days and non-wet days.

# note: Precip and other water fluxes are read in as MKS units of kg/m^2/s
#       Multiply by 86400 to convert from kg/m^2/s to mm per day 
#       Multiply by 3401.575 to convert from kg/m^2/s to inches per day 
#       Based on the following: 1 Kg of water = 1 L volume = 1000 cm^3 volume. 
#       If you spread 1000 cm^3 over an area of 1 square meter, the water 
#       would be 0.1 cm deep, or 1 mm.  Hence 1 Kg/m^2 is equivalent to 
#       1 mm of depth. 

# constants:
#   sec.per.day:  Number of seconds in a day = 86400
#   mm.per.inch:  Number of millimeters in one inch

# set some constants
  sec.per.day <- 86400.0
  mm.per.inch <- 25.4

# opt.value == 0 means apply a wetday criterion of 0.0 (within machine 
#                precision of zero)
  if (opt.value == 0) {
     threshold.wetday <- 0.0
  }

# opt.value == 1 means apply a wetday criterion equivalent to 0.1 mm per day 
  if (opt.value == 1) {
     threshold.wetday <- (0.1 / sec.per.day)
  }

# opt.value == 2 means apply a wetday criterion equivalent to 0.01 inch per day
  if (opt.value == 2) {
     threshold.wetday <- (0.01 * mm.per.inch / sec.per.day)
  }
  
# --- Load the logical output vector is.wetday to contain masks of
#     wetday=TRUE and non-wetday=FALSE
  is.wetday <- vector.in > threshold.wetday
  print(vector.in[1:15])
  print(is.wetday[1:15])
  print(threshold.wetday)

return (list("is.wetday" = is.wetday, "threshold.wetday" = threshold.wetday))
}
# + + + end defining function WetDayID + + +



