#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##
##  This is the bridge to R.
##  
#############################################################################

import math
import os

try:
    import rpy2
    from rpy2 import robjects as ro
except:
    raise Exception, "rpy2 not properly installed!"

try:
    ro.r("library(metafor)")
except:
    raise Exception, "metafor (R) package not installed.\nPlease install this package and then re-start OpenMeta."
    
try:
    # @TODO integrate into metafor (openmeta.R) package(s)
    # this is a bit hacky.
    ro.r("setwd(%s)" % os.getcwd())
    ro.r("source(data_transform.R)")
except:
    raise Exception, "whoops, data_transform.R script unavailable."
    
def effect_for_study(e1, n1, e2, n2, metric="OR"):
    '''
    Computes a point estimate, lower & upper bound for 
    the parametric 2x2 table data.

    @params
    ===
    e1 -- events in group 1
    n1 -- size of group 1
    e2 -- events in group 2
    n2 -- size of group 2
    '''
    r_str = "escalc(measure='%s', ai=c(%s), n1i=c(%s), ci=c(%s), n2i=c(%s))" %\
                    (metric, e1, n1, e2, n2)
    effect = ro.r(r_str)
    
    # the escalc function computes the log-odds ration and
    # the variance. for now let's just return the variance.
    return math.exp(effect[0][0])