######################################
#                                    #       
#  Byron C. Wallace                  #
#  George Dietz                      #
#  CEBM @ Brown                      # 
#  OpenMeta[analyst]                 # 
#                                    # 
#  Contains globals used             #
#   throughout.                      # 
#                                    # 
######################################

# TODO: move functions out of here and just have this be constants w/o imports

import os
import meta_py_r

from PyQt4.Qt import QColor, QUndoCommand

# number of digits to display
NUM_DIGITS = 3

# number of digits to display in calculator
#   It is now a global here and in the data_table_view class. (However
#   here we show four digits; there it is 3. We want different
#   levels of granularity).
CALC_NUM_DIGITS = 4

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
BINARY_METRIC_NAMES = {"OR":"Odds Ratio",
                       "RD":"Risk Difference",
                       "RR":"Risk Ratio",
                       "AS":"Difference of arcsines transformed proportions",
                       "YUQ":"Yule's Q is equal to (oi-1)/(oi+1), where oi is the odds ratio.",
                       "YUY":"Yule's Y is equal to (sqrt(oi)-1)/(sqrt(oi)+1), where oi is the odds ratio.",
                       "PR":"Untransformed Proportion",
                       "PLN":"Natural Logarithm transformed Proportion",
                       "PLO":"Logit transformed Proportion",
                       "PAS":"Arcsine transformed Proportion",
                       "PFT":"Freeman-Tukey Transformed Proportion",
                       }

# Continuous metrics
CONTINUOUS_TWO_ARM_METRICS = ["MD", "SMD"]
CONTINUOUS_ONE_ARM_METRICS = ["TX Mean"]
CONTINUOUS_METRIC_NAMES = {"MD":"Mean Difference",
                           "SMD":"Standardized Mean Difference",
                           "TX Mean":"TX Mean",
                           }


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
DIAGNOSTIC_METRIC_NAMES = {"Sens":"Sensitivity",
                           "Spec":"Specificity",
                           "PLR":"Positive Likelihood Ratio",
                           "NLR":"Negative Likelihood Ratio",
                           "DOR":"Diagnostic Odds Ratio",
                           }

# Construct dictionary of all the metric names
ALL_METRIC_NAMES = {}
ALL_METRIC_NAMES.update(BINARY_METRIC_NAMES)
ALL_METRIC_NAMES.update(CONTINUOUS_METRIC_NAMES)
ALL_METRIC_NAMES.update(DIAGNOSTIC_METRIC_NAMES)

# enumeration of data types and dictionaries mapping both ways
BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)

# we need two types for covariates; factor and continuous. we'll use the 
# above definition (enumerated as part of a general data type) for continuous
# and just define factor here.
FACTOR = 4

# making life easier
COV_INTS_TO_STRS = {4:"factor", 1:"continuous"}

STR_TO_TYPE_DICT = {u"binary":BINARY,
                    u"continuous":CONTINUOUS, 
                    u"diagnostic":DIAGNOSTIC,
                    u"OTHER":OTHER
                    }

TYPE_TO_STR_DICT = {BINARY:u"binary",
                    CONTINUOUS:u"continuous",
                    DIAGNOSTIC:u"diagnostic",
					OTHER:u"OTHER",
                    FACTOR:u"factor",
                    }
                                    
# enumeration of meta-analytic types
VANILLA, NETWORK = range(2)

EMPTY_VALS = ("", None) # these indicate an empty row/cell 

BASE_PATH = str(os.path.abspath(os.getcwd()))

#def get_BASE_PATH():
#    BASE_PATH = str(os.path.abspath(os.getcwd())) # where temporary R output should go

# this is the (local) path to a (pickled) dictionary containing
# user preferences
PREFS_PATH = "user_prefs.dict"

# this is a useful function sometimes.
none_to_str = lambda x: "" if x is None else x

HELP_URL = "http://www.cebm.brown.edu/open_meta"

# for diagnostic data -- this dictionary maps
# the mteric names as they appear in the UI/ure
# used here to the names used in the model.
# see get_diag_metrics_to_run.
DIAG_METRIC_NAMES_D = {"sens":["Sens"], 
                       "spec":["Spec"],
                       "dor":["DOR"],
                       "lr":["PLR", "NLR"]
                      }

DIAG_FIELDS_TO_RAW_INDICES = {"TP":0, "FN":1, "FP":2, "TN":3}

PATH_TO_HELP = "http://tuftscaes.org/open_meta/help/openMA_help.html"#os.path.join("doc", "openMA_help.html")

# list of methods with no forest plot parameters
METHODS_WITH_NO_FOREST_PLOT = ["diagnostic.hsroc", "diagnostic.bivariate.ml"]

# this is the maximum size of a residual that we're willing to accept
# when computing 2x2 data
THRESHOLD = 1e-5

ERROR_COLOR = QColor("red")
OK_COLOR = QColor("black")

DEFAULT_GROUP_NAMES = ["Grp A", "Grp B"]  # old: DEFAULT_GROUP_NAMES = ["tx A", "tx B"]


def equal_close_enough(x,y):
    THRESHOLD = 1e-4
    if abs(x-y) < THRESHOLD:
        return True
    else:
        return False

### CONFIDENCE LEVEL STUFF #####
DEFAULT_CONF_LEVEL = 95.0    # (normal 95% CI)



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
    # there's no built-in for checking if a number is a NaN in
    # Python < 2.6. checking if a number is equal to itself
    # does the trick, though purportedly does not always work.
    return x != x

class CommandGenericDo(QUndoCommand):
    '''
   This is a generic undo/redo command that takes two unevaluated lambdas --
   thunks, if you will -- one for doing and one for undoing.
    '''
    def __init__(self, redo_f, undo_f, description=""):
        super(CommandGenericDo, self).__init__(description)
        self.redo_f = redo_f
        self.undo_f = undo_f
        
    def redo(self):
        self.redo_f()
        
    def undo(self):
        self.undo_f()
        

def matrix_as_table(matrix, col_width=None, spacing=2):
    ''' matrix is a list of rows, col_width is the fixed column width '''
    
    # get default col width, wide as widgest col
    if col_width is None:
        max_width = 0
        for row in matrix:
            widths = [len(x) for x in row]
            max_width = max(max_width, max(widths))
        col_width = max_width

    matrix_formatted = []
    for row in matrix:
        formatted_row = ["{0:{width}}".format(x, width=col_width) for x in row]
        spacer = " "*spacing
        formatted_row = spacer.join(formatted_row)
        matrix_formatted.append(formatted_row)
        
    matrix_formatted = "\n".join(matrix_formatted)
    return matrix_formatted

def tabulate(lists, sep=" | ", return_col_widths=False, align=[]):
    ''' Makes a pretty table from the lists in args'''
    ''' each arg is a list '''
    ''' if return_max_col_lenths is true, the return type is a tuple of (str, col_widths) '''
    ''' align is a list the same length as lists telling how the column should be aligned ('L','R') etc '''
    
    if len(align) != len(lists):
        align = ['L',]*len(lists) 
    print("Align is now %s: " % align)
    
    # covert lists in args to string lists
    string_lists = []
    for arg in lists:
        str_arg = [str(x) for x in arg]
        string_lists.append(str_arg)
    
    # get max length of each element in each column
    max_lengths = []
    for arg in string_lists:
        max_len = max([len(x) for x in arg])
        max_lengths.append(max_len)
        
    data = zip(*string_lists)
    out = []
    for row in data:
        row_str = ["{0:{align}{width}}".format(x, width=width,align='<' if row_alignment=='L' else '>') for x,width,row_alignment in zip(row,max_lengths,align)]
        row_str = sep.join(row_str)
        out.append(row_str)
    out_str =  "\n".join(out)
    
    if return_col_widths:
        return (out_str, max_lengths)
    return out_str