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
    print "\nloading R libraries..."
    ro.r("library(metafor)")
except:
    raise Exception, "metafor (R) package not installed.\nPlease install this package and then re-start OpenMeta."
    
try:
    # @TODO integrate into metafor (openmeta.R) package(s)
    # this is extremely kludgey and will likely break on other platforms
    cur_dir =  os.getcwd().replace("\\", "//")
    print cur_dir
    ro.r("setwd('%s')" % cur_dir)
    ro.r("source('data_transform.R')")
except:
    raise Exception, "whoops, data_transform.R script unavailable."
    
    
def impute_two_by_two(bin_data_dict):
    print "imputing 2x2 table via R..."
    print bin_data_dict
    
    # rpy2 doesn't know how to handle None types.
    # we can just remove them from the dictionary.
    for param, val in bin_data_dict.items():
        if val is None:
            bin_data_dict.pop(param)
    
    dataf = ro.r['data.frame'](**bin_data_dict)
    two_by_two = ro.r('impute.bin.data(bin.data=%s)' % dataf.r_repr())
    print two_by_two
    
def none_to_null(x):
    if x is None:
        return ro.r['as.null']()
    return x
    
def effect_for_study(e1, n1, e2, n2, metric="OR", conf_level=.975):
    '''
    Computes a point estimate, lower & upper bound for 
    the parametric 2x2 table data.
    
    @TODO add support for non-normal (e.g., T) distributions
    
    @params
    ===
    e1 -- events in group 1
    n1 -- size of group 1
    e2 -- events in group 2
    n2 -- size of group 2
    '''
    print metric
    r_str = "escalc(measure='%s', ai=c(%s), n1i=c(%s), ci=c(%s), n2i=c(%s))" %\
                    (metric, e1, n1, e2, n2)
    
    effect = ro.r(r_str)
    lg_point_est = effect[0][0]
    v = math.sqrt(effect[1][0])
    
    # scalar for computing confidence interval
    r_str = "qnorm(%s)" % conf_level
    scalar = ro.r(r_str)[0]    
    
    lower, upper = (math.exp(lg_point_est-scalar*v), math.exp(lg_point_est+scalar*v))
    point_est, lower, upper = [math.exp(lg_point_est), lower, upper]
    print "%s, %s, %s" % (lower, point_est, upper)
    
    return (point_est, lower, upper)