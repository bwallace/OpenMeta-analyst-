#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##
##  This is the bridge to R.
##  
#############################################################################

try:
    import rpy2
except:
    raise Exception, "rpy2 not properly installed!"

def effect_for_study(e1, n1, e2, n2, metric="OR"):
    '''
    Computes a point estimate, lower & upper bound for 
    the parametric 2x2 table data.
    ==
    params
    '''