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

# Sometimes it's useful to know if we're dealing with a one-arm outcome,
# in general
ONE_ARM_METRICS = BINARY_ONE_ARM_METRICS + CONTINUOUS_ONE_ARM_METRICS 

# Diagnostic metrics
DIAGNOSTIC_METRICS = ["Sens", "Spec", "PPV", "NPV", "Acc", "PLR", "NLR", "DOR"]
DIAGNOSTIC_LOG_METRICS = ["PLR", "NLR", "DOR"]

# enumeration of data types and dictionaries mapping both ways
BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)

# we need two types for covariates; factor and continuous. we'll use the 
# above definition (enumerated as part of a general data type) for continuous
# and just define factor here.
FACTOR = 4

STR_TO_TYPE_DICT = {u"binary":BINARY, u"continuous":CONTINUOUS, 
                                    u"diagnostic":DIAGNOSTIC, u"OTHER":OTHER}
TYPE_TO_STR_DICT = {BINARY:u"binary", CONTINUOUS:u"continuous", 
                                    DIAGNOSTIC:u"diagnostic", OTHER:u"OTHER"}
                                    
# enumeration of meta-analytic types
VANILLA, NETWORK = range(2)

EMPTY_VALS = ("", None) # these indicate an empty row/cell 

# this is useful sometimes.
none_to_str = lambda x: "" if x is None else x