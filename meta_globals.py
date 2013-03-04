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

import os
#from PyQt4.Qt import QMessageBox

# number of digits to display
NUM_DIGITS = 3

# for calculating CIs (normal 95% CI)
MULT = 1.96

# completely made up. need an actual versioning system.
VERSION = .005 

## For now we're going to hardcode which metrics are available.
# In the future, we may want to pull these out dynamically from 
# the R side. But then meta-analytic methods would have either to
# only operate over the effects and variances or else themselves 
# know how to compute arbitrary metrics.

# Binary metrics
BINARY_TWO_ARM_METRICS = ["OR", "RD", "RR", "AS", "YUQ", "YUY"]
BINARY_ONE_ARM_METRICS = ["PR", "PLN", "PLO", "PAS", "PFT"]

# Continuous metrics
CONTINUOUS_TWO_ARM_METRICS = ["MD", "SMD"]
CONTINUOUS_ONE_ARM_METRICS = ["TX Mean"]

# Default metrics (for when making a new dataset)
DEFAULT_BINARY_ONE_ARM = "PR"
DEFAULT_BINARY_TWO_ARM = "OR"
DEFAULT_CONTINUOUS_ONE_ARM = "TX Mean"
DEFAULT_CONTINUOUS_TWO_ARM = "SMD"

# Sometimes it's useful to know if we're dealing with a one-arm outcome,
# in general
ONE_ARM_METRICS = BINARY_ONE_ARM_METRICS + CONTINUOUS_ONE_ARM_METRICS 
TWO_ARM_METRICS = BINARY_TWO_ARM_METRICS + CONTINUOUS_TWO_ARM_METRICS

# Diagnostic metrics
DIAGNOSTIC_METRICS = ["Sens", "Spec", "PLR", "NLR", "DOR"]
DIAGNOSTIC_LOG_METRICS = ["PLR", "NLR", "DOR"]

# enumeration of data types and dictionaries mapping both ways
BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)

# we need two types for covariates; factor and continuous. we'll use the 
# above definition (enumerated as part of a general data type) for continuous
# and just define factor here.
FACTOR = 4

# making life easier
COV_INTS_TO_STRS = {4:"Factor", 1:"Continuous"}

STR_TO_TYPE_DICT = {u"binary":BINARY, u"continuous":CONTINUOUS, 
                                    u"diagnostic":DIAGNOSTIC, u"OTHER":OTHER}
TYPE_TO_STR_DICT = {BINARY:u"binary", CONTINUOUS:u"continuous", DIAGNOSTIC:u"diagnostic", 
						OTHER:u"OTHER", FACTOR:u"factor"}
                                    
# enumeration of meta-analytic types
VANILLA, NETWORK = range(2)

EMPTY_VALS = ("", None) # these indicate an empty row/cell 

BASE_PATH = str(os.path.abspath(os.getcwd())) # where temporary R output should go

# this is the (local) path to a (pickled) dictionary containing
# user preferences
PREFS_PATH = "user_prefs.dict"

# this is a useful function sometimes.
none_to_str = lambda x: "" if x is None else x

# for diagnostic data -- this dictionary maps
# the mteric names as they appear in the UI/ure
# used here to the names used in the model.
# see get_diag_metrics_to_run.
DIAG_METRIC_NAMES_D = {
                        "sens":["Sens"], 
                        "spec":["Spec"],
                        "dor":["DOR"],
                        "lr":["PLR", "NLR"]
                      }

DIAG_FIELDS_TO_RAW_INDICES = {"TP":0, "FN":1, "FP":2, "TN":3}

PATH_TO_HELP = "http://tuftscaes.org/open_meta/help/openMA_help.html"#os.path.join("doc", "openMA_help.html")

# list of methods with no forest plot parameters
METHODS_WITH_NO_FOREST_PLOT = ["diagnostic.hsroc", "diagnostic.bivariate.ml"]




'''
some useful static methods
'''

def seems_sane(xticks):
    num_list = xticks.split(",")
    if len(num_list) == 1:
        return False
    try:
        num_list = [eval(x) for x in num_list]
    except:
        return False
    return True
    
def check_plot_bound(bound):
    try:
        # errrm... this might cause a problem if 
        # bound is 0... 
        return float(bound) 
    except:
        return False

def _is_a_float(s):
    try:
        float(s)
        return True
    except:
        return False

def _is_empty(s):
    return s is None or s == ""
    
def _is_an_int(s):
    try:
        int(s)
        return True
    except:
        return False
    
def is_NaN(x):
    return x != x







# These two functions started life in the diagnostic data form used for checking
#  that low < effect < high
def my_lt(a,b):
    if _is_a_float(a) and _is_a_float(b):
        return float(a) < float(b)
    else:
        return None
def between_bounds(est=None, 
                   low=None, 
                   high=None):
    good_result = my_lt(low,est)
    okay = True if not (good_result is None) else False
    if okay and not good_result:
        msg = "The lower CI must be less than the point estimate!"
        return False,msg
    
    good_result = my_lt(est,high)
    okay = True if not (good_result is None) else False
    if okay and not good_result:
        msg = "The higher CI must be greater than the point estimate!"
        return False,msg
    
    good_result = my_lt(low,high)
    okay = True if not (good_result is None) else False
    if okay and not good_result:
        msg = "The lower CI must be less than the higher CI!"
        return False,msg
    
    return True,None

def cast_to_int(value, name=None):
    '''Converts value to int if possible'''
    try:
        rounded = round(float(value))
        return int(rounded)
    except:
        if not name is None:
            print("Could not convert %s='%s' to int" % (name,str(value)))
        else:
            print("Could not convert '%s' to int" % (str(value)))
        return None