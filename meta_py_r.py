####################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#
#  This is a proxy module that is responsible for communicating with R.
#   **All calls to R (equivalently, all references to the rpy2 library)
#   are to be made via this module **
#
####################################################

import math
import os
import pdb
from pdb import set_trace
from PyQt4.QtCore import pyqtRemoveInputHook


try:
    import rpy2
    from rpy2 import robjects as ro
except:
    raise Exception, "rpy2 not properly installed!"

try:
    # ascertain that R has write privledges
    print "\nloading R libraries..."
    ro.r("library(metafor)")
    ro.r("library(openmetar)")
    print "openmetaR package succesfully loaded"
except:
    raise Exception, "Either the metafor or openmetar R package is not installed.\nPlease install these packages and then re-start OpenMeta."

try:
    ro.r("library(igraph)")
except:
    raise Exception, "igraph library not available!"

try:
    if not ro.r("file.exists('./.r_tmp')")[0]:
        print("creating tmp R directory...")
        ro.r("dir.create('./r_tmp')")
        print("success -- temporary results will be written to ./r_tmp")
except:
    raise Exception, "unable to create temporary directory for R results! make sure you have sufficient permissions."

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

def evaluate_in_r(r_str):
    res = ro.r(r_str)
    return str(res)

def get_params(method_name):
    param_list = ro.r("%s.parameters()" % method_name)
    # note that we're assuming that the last entry of param_list, as provided
    # by the corresponding R routine, is the order to display the variables
    param_d = {}
    for name, r_obj in zip(param_list.getnames(), param_list):    
        param_d[name] = r_obj

    order_vars = None
    if param_d.has_key("var_order"):
        order_vars = list(param_d["var_order"])
    
    return (_rlist_to_pydict(param_d['parameters']), _rlist_to_pydict(param_d['defaults']), order_vars)

def get_available_methods(for_data_type=None, data_obj_name=None):
    '''
    Returns a list of methods available in OpenMeta for the particular data_type
    (if one is given). Excludes "*.parameters" methods
    '''
    method_list = ro.r("lsf.str('package:openmetar')")
    # by convention, the methods available for a data type (e.g., binary)
    # start with the name of the data type. furthermore, the parameters
    # for those methods are returned by a method with a name
    # ending in ".parameters"
    special_endings = [".parameters", ".is.feasible"]
    is_special = lambda f: any([f.endswith(ending) for ending in special_endings])
    all_methods = [method for method in method_list if not is_special(method)]
    if for_data_type is not None:
        all_methods = [method for method in all_methods if method.startswith(for_data_type)]
    
    feasible_methods = all_methods
    # now, if a data object handle was provided, check which methods are feasible
    if data_obj_name is not None:
        feasible_methods = []
        for method in all_methods:
            is_feasible = True
            is_feas_f = "%s.is.feasible" % method
            if is_feas_f in method_list:
                is_feasible = ro.r("%s(%s)" % (is_feas_f, data_obj_name))[0]
            if is_feasible:
                feasible_methods.append(method)
    return feasible_methods


def ma_dataset_to_binary_robj(table_model, var_name):
    pass
    
    
def draw_network(edge_list, unconnected_vertices, network_path = '"./r_tmp/network.png"'):
    '''
    This draws the parametric network specified by edge_list.
    The latter is assumed to be in form:
        ["tx a", "tx b", "tx b", "tx c" .... "tx z']
    Where two adjacent entires in the list are connected.
    Note that we (lazily) make all calls to R here rather than
    implementing a method on the R side that takes a graph/
    edge list. We may want to change this eventually.
    '''
    if len(edge_list) > 0:
        edge_str = ", ".join([" '%s' " % x for x in edge_list])
        print "el <- matrix(c(%s), nc=2, byrow=TRUE)" % edge_str
        ro.r("el <- matrix(c(%s), nc=2, byrow=TRUE)" % edge_str)
        ro.r("g <- graph.edgelist(el, directed=FALSE)")
    else:
        ro.r("g <- graph.empty()") 
    
    if len(unconnected_vertices) > 0:
        print unconnected_vertices
        vertices_str = ", ".join([" '%s' " % x for x in unconnected_vertices])
        print "g <- add.vertices(g, %s, name=c(%s))" % (len(unconnected_vertices), vertices_str)
        ro.r("g <- add.vertices(g, %s, name=c(%s))" % (len(unconnected_vertices), vertices_str))
    ro.r("png(%s)" % network_path)
    ro.r("plot(g, vertex.label=V(g)$name, layout=layout.circle, vertex.size=25, asp=.3, margin=-.05)")
    ro.r("dev.off()")
    return "r_tmp/network.png"
    
def ma_dataset_to_simple_binary_robj(table_model, var_name="tmp_obj"):
    '''
    This converts a DatasetModel to an OpenMetaData (OMData) R object. We use type DatasetModel
    rather than a DataSet model directly to access the current variables. Furthermore, this allows
    us to check which studies (if any) were excluded by the user.
    
    By 'simple' we mean that this method returns a single outcome single follow-up (defined as the
    the currently selected, as indicated by the model object) data object.

     @TODO
        - implement methods for more advanced conversions, i.e., for multiple outcome
            datasets (althought this will be implemented in some other method)
    '''
    r_str = None
    
    # grab the study names. note: the list is pulled out in reverse order from the 
    # model, so we, er, reverse it.
    studies = table_model.get_studies()
    studies.reverse()
    study_names = ", ".join(["'" + study.name + "'" for study in studies])
        
    # first try and construct an object with raw data
    if table_model.included_studies_have_raw_data():
        print "ok; raw data has been entered for all included studies"
        
        # get the point estimates
        ests, SEs = table_model.get_cur_ests_and_SEs()
        ests_str = ", ".join(_to_strs(ests))
        SEs_str = ", ".join(_to_strs(SEs))
        
        # now figure out the raw data
        raw_data = table_model.get_cur_raw_data()
    
        g1_events = _get_col(raw_data, 0)
        g1_events.reverse()
        g1O1_str = ", ".join(_to_strs(g1_events))
        g1_totals = _get_col(raw_data, 1)
        g1_totals.reverse()
        g1O2 = [(total_i-event_i) for total_i, event_i in zip(g1_totals, g1_events)]
        g1O2_str = ", ".join(_to_strs(g1O2))
    
        # now, for group 2
        g2_events = _get_col(raw_data, 2)
        g2_events.reverse()
        g2O1_str = ", ".join(_to_strs(g2_events))
        g2_totals = _get_col(raw_data, 3)
        g2_totals.reverse()
        g2O2 = [(total_i-event_i) for total_i, event_i in zip(g2_totals, g2_events)]
        g2O2_str = ", ".join(_to_strs(g2O2))
    
        # actually creating a new object on the R side seems the path of least resistance here.
        # the alternative would be to try and create a representation of the R object on the 
        # python side, but this would require more work and I'm not sure what the benefits
        # would be
        r_str = "%s <- new('BinaryData', g1O1=c(%s), g1O2=c(%s), g2O1=c(%s), g2O2=c(%s), y=c(%s), SE=c(%s), studyNames=c(%s))" \
                        % (var_name, g1O1_str, g1O2_str, g2O1_str, g2O2_str, ests_str, SEs_str, study_names)
    
        
    elif table_model.included_studies_have_point_estimates():
        print "not sufficient raw data, but studies have point estimates..."
        ests, SEs = table_model.get_cur_ests_and_SEs()
        ests_str = ", ".join(_to_strs(ests))
        SEs_str = ", ".join(_to_strs(SEs))
        
        print "\n\n"
        print SEs_str
        
        r_str = "%s <- new('BinaryData', y=c(%s), SE=c(%s), studyNames=c(%s))" \
                            % (var_name, ests_str, SEs_str, study_names)
                            
    else:
        print "there is neither sufficient raw data nor entered effects/CIs. I cannot run an analysis."
        
    print "executing: %s" % r_str
    ro.r(r_str)
    print "ok."
    return r_str

def _all_studies_have_raw_data(studies):
    pass
    
def run_binary_ma(function_name, params, res_name = "result", bin_data_name="tmp_obj"):
    params_df = ro.r['data.frame'](**params)
    r_str = "%s<-%s(%s, %s)" % (res_name, function_name, bin_data_name, params_df.r_repr())
    print "\n\n(run_binary_ma): executing:\n %s\n" % r_str
    ro.r(r_str)
    result = ro.r("%s" % res_name)

    # parse out text field(s). note that "plot names" is 'reserved', i.e., it's
    # a special field which is assumed to contain the plot variable names
    # in R (for graphics manipulation).
    text_d = {}
    image_var_name_d = None
    for text_n, text in zip(list(result.getnames())[1:], list(result)[1:]):
        if not text_n == "plot_names":
           text_d[text_n]=text
        else:
           image_var_name_d = _rls_to_pyd(text)

    return {"images":_rls_to_pyd(result[0]), "image_var_names":image_var_name_d,
                                              "texts":text_d}


def _rls_to_pyd(r_ls):
    # base case is that the type is a native python type, rather
    # than an Rvector
    d = {}

    for name, val in zip(r_ls.getnames(), r_ls):
        try:
            # first check the key
            if str(name) != "NULL":
                print name
                print type(name)
                if "rpy2.robjects" in str(type(name)):
                    name = str(name[0])
                if not "rpy2.robjects" in str(type(val)):
                    # base case; not an rtype object
                    d[name] = val
                elif str(val)=="NULL":
                    d[name] = None
                elif str(val.getnames())=="NULL":
                    d[name] = val[0]
                else:
                    d[name] = _rls_to_pyd(val)
                if not isinstance(name, str):
                    raise Exception, "arg"
            else:
                # name is null
                return val

        except Exception,  inst:
            print inst
            pyqtRemoveInputHook()
            pdb.set_trace()

    return d


# TODO clean up
def _rlist_to_pydict(r_ls):
    # need to fix this; recursively build dictionary!!!!
    d = {}
    names = r_ls.getnames()
    for name, val in zip(names, r_ls):
        if isinstance(val, rpy2.robjects.RVector) and not str(val.getnames())=="NULL":
            d[name] = _rlist_to_pydict(val)
        cur_x = list(val)
        if len(cur_x) == 1:
            d[name] = cur_x[0]
        else:
            d[name] = cur_x
    return d

def _get_c_str_for_col(m, i):
    return ", ".join(self._get_col(m, i))

def _to_strs(v):
    return [str(x) for x in v]

def _get_col(m, i):
    col_vals = []
    for x in m:
        col_vals.append(x[i])
    return col_vals

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


