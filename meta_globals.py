######################################
#                                    #       
#  Byron C. Wallace                  #
#  Tufts Medical Center              # 
#  OpenMeta[analyst]                 # 
#                                    # 
#  Contains globals used             #
#   throughout.                      # 
#                                    # 
######################################

# number of digits to display
NUM_DIGITS = 3

# completely made up. need an actual versioning system.
VERSION = .005 

## For now we're going to hardcode which metrics are available.
# In the future, we may want to pull these out dynamically from 
# the R side. But then meta-analytic methods would have either to
# only operate over the effects and variances or else themselves 
# know how to compute arbitrary metrics.

# Binary metrics
BINARY_TWO_ARM_METRICS = ["OR", "RD", "RR", "AS", "PETO", "YUQ", "YUY"]
BINARY_ONE_ARM_METRICS = ["PR", "PLN", "PLO", "PAS", "PFT"]

# Continuous metrics
CONTINUOUS_TWO_ARM_METRICS = ["MD", "SMD"]
CONTINUOUS_ONE_ARM_METRICS = ["TX Mean"]