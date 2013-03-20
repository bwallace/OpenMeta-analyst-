#############################################################################
#                                                                           #
#  Byron C. Wallace                                                         #
#  Tufts Medical Center                                                     #
#  OpenMeta[analyst]                                                        #
#                                                                           #
#  This is a proxy module that is responsible for communicating with R.     #
#  An unholy mixture of R and Python                                        #            
#   **All calls to R (equivalently, all references to the rpy2 library)     #
#   are to be made via this module **                                       #
#                                                                           #
#############################################################################

import math
import os
import pdb
#import collections

from PyQt4.QtCore import pyqtRemoveInputHook
from meta_globals import (BASE_PATH,CONTINUOUS,ONE_ARM_METRICS,TWO_ARM_METRICS,
                          TYPE_TO_STR_DICT)

try:
    #import rpy2
    # this line throws a segfault
    from rpy2 import robjects as ro
except Exception, e:
    raise Exception, "rpy2 not properly installed!"
    print e
    
import rpy2.robjects
#from rpy2.rinterface import NALogicalType
#from rpy2.rinterface import NARealType

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
    ro.r("library(grid)")
except:
    raise Exception, "igraph library not available!"

try:
    if not ro.r("file.exists('./.r_tmp')")[0]:
        print("creating tmp R directory...")
        ro.r("dir.create('./r_tmp')")
        print("success -- temporary results will be written to ./r_tmp")
except:
    raise Exception, "unable to create temporary directory for R results! make sure you have sufficient permissions."

def reset_Rs_working_dir():
    print "resetting "
    ro.r("setwd('%s')" % BASE_PATH) 

#def impute_two_by_two(bin_data_dict):
#    print "computing 2x2 table via R..."
#    print bin_data_dict
#
#    # rpy2 doesn't know how to handle None types.
#    # we can just remove them from the dictionary.
#    for param, val in bin_data_dict.items():
#        if val is None:
#            bin_data_dict.pop(param)
#
#    dataf = ro.r['data.frame'](**bin_data_dict)
#    two_by_two = ro.r('impute.bin.data(bin.data=%s)' % dataf.r_repr())
#    print two_by_two

def OLDimpute_diag_data(diag_data_dict, metric):
    print "computing 2x2 table via R..."
    print diag_data_dict

    # rpy2 doesn't know how to handle None types.
    # we can just remove them from the dictionary.
    for param, val in diag_data_dict.items():
        if val is None:
            diag_data_dict.pop(param)

    dataf = ro.r['data.frame'](**diag_data_dict)
    two_by_two = ro.r("impute.diagnostic.data(%s, '%s')" % \
                        (dataf.r_repr(), metric))

    return _rls_to_pyd(two_by_two)

def impute_diag_data(diag_data_dict):
    print "computing 2x2 table via R..."
    print diag_data_dict

    # rpy2 doesn't know how to handle None types.
    # we can just remove them from the dictionary.
    for param, val in diag_data_dict.items():
        if val is None:
            diag_data_dict.pop(param)

    dataf = ro.r['data.frame'](**diag_data_dict)
    two_by_two = ro.r("gimpute.diagnostic.data(%s)" % (dataf.r_repr()))
    
    
    #print "Imputed 2by2:", _rls_to_pyd(two_by_two)
    print "Imputed 2by2:", _grlist_to_pydict(two_by_two)

    #return _rls_to_pyd(two_by_two)
    return _grlist_to_pydict(two_by_two)

def impute_bin_data(bin_data_dict):
    for param, val in bin_data_dict.items():
        if val is None:
            bin_data_dict.pop(param)

    dataf = ro.r['data.frame'](**bin_data_dict)
    two_by_two = ro.r("gimpute.bin.data(%s)" % (dataf.r_repr()))
    
    return _grlist_to_pydict(two_by_two)

def fillin_2x2(table_data_dict):
    #r_str = ["fillin.2x2.simple("]
    ro.r("source('sandbox.r')")
    r_str = ["fillin.2x2.simpler("]
    
    # construct argument list if argument is not None
    for param, val in table_data_dict.items():
        if val is not None:
            r_str.append("%s=%s," % (param, val))
        
    # drop the last comma, close the function call
    r_str = "".join(r_str)[:-1] if r_str[-1].endswith(",") else "".join(r_str)
    r_str += ")"
    res = ro.r(r_str)
    #if "NA" in str(res).split(" "):
    #    return None
    
    print "\n\n*****"
    print r_str
    #print res
    print "*****\n\n"
    
    
    # CONVERT res to python DICT using the fact that the r objects are iterable
    #for (name, index, value) in zip(enumerate(res.names)

    if len(res) == 1 and _gis_NA(res[0]):
        return None
    toreturn = _grlist_to_pydict(res,True)
    return toreturn

def _gis_NA(x):
    #print "Result of NA comparison:"
    #print "Old:", type(x) in [rpy2.rinterface.NALogicalType, rpy2.rinterface.NARealType]
    #print "New:", type(x) in [NALogicalType, NARealType]
    #return type(x) in [rpy2.rinterface.NALogicalType, rpy2.rinterface.NARealType]
    #print "TESTING NA"
    
    #return type(x) in [NALogicalType, NARealType]
    return str(x) == 'NA'

# NOTE: CUSTOM VERSION......
def _grlist_to_pydict(r_ls, recurse=True):
    '''
    parse rpy2 data structure into analogous Python
    dictionary. if the recursive flag is true, this is 
    done recursively, i.e., if a key points to an R
    list, that list will be converted, too.
    '''
    
    #pyqtRemoveInputHook()
    #pdb.set_trace() 
    
    def gis_a_list(x):
        # @TODO add additional vector types?
        #return type(x) in [rpy2.robjects.vectors.StrVector, rpy2.robjects.vectors.ListVector]
        return type(x) in [rpy2.robjects.vectors.StrVector, 
                           rpy2.robjects.vectors.ListVector, 
                           rpy2.robjects.vectors.FloatVector,
                           rpy2.robjects.vectors.Vector,]

    def convert_NA_to_None(x):
        return None if _gis_NA(x) else x
    
    def is_named_R_list(val):
        return gis_a_list(val) and not str(val.names)=="NULL"
    
    d = {}
    names = r_ls.names
    for name, val in zip(names, r_ls):
        #print "name {0}, val {1}".format(name, val)
        if recurse and is_named_R_list(val):
            print "recursing... \n"
            d[name] = _grlist_to_pydict(val)
        else: # not a named R list or we should not recurse
            try: #assume val is iterable.....
                cur_x = list(val)
                if len(cur_x) == 1:
                    # if it's a singleton, extract the
                    # the value and stick it in the dict.
                    # -- this is essentially the 'base case'
                    d[name] = cur_x[0]
                    d[name] = convert_NA_to_None(d[name])
                else: # val is a real list, not a singleton list
                    d[name] = val # not a singleton list
                    d[name] = [convert_NA_to_None(x) for x in d[name][:]]
            except: # val is not iterable
                d[name] = val

    return d

def impute_cont_data(cont_data_dict, alpha):
    print "computing continuous data via R..."
    
    # first check that we have some data;
    # if not, there's no sense in trying to
    # impute anything
    if len(cont_data_dict.items()) == 0:
        return {"succeeded":False}
    
    r_str = ["fillin.cont.1spell("]
    for param, val in cont_data_dict.items():
        r_str.append("%s=%s," % (param, val))
        
    r_str = "".join(r_str)

    # append alpha argument (for CI level); close function call (parens)
    r_str += "alpha=%s)" % alpha
    
    print "attempting to execute: %s" % r_str
    c_data = ro.r(r_str)
    #pyqtRemoveInputHook()
    #pdb.set_trace() 
    #return _rls_to_pyd(c_data)
    return _grlist_to_pydict(c_data,True)
    
def impute_pre_post_cont_data(cont_data_dict, correlation, alpha):
    if len(cont_data_dict.items()) == 0:
        return {"succeeded":False}
        
    r_str  = ["fillin.cont.AminusB("]
    for param, val in cont_data_dict.items():
        r_str.append("%s=%s," % (param, val))
    
    r_str = "".join(r_str)
    r_str += "correlation=%s, alpha=%s)" % (correlation, alpha)
    print "attempting to execute: %s" % r_str
    c_data = ro.r(r_str)
    return _rls_to_pyd(c_data)
    
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
    for name, r_obj in zip(param_list.names, param_list):    
        param_d[name] = r_obj

    order_vars = None
    if param_d.has_key("var_order"):
        order_vars = list(param_d["var_order"])

    pretty_names_and_descriptions = get_pretty_names_and_descriptions_for_params(\
                                        method_name, param_list)

    return (_rlist_to_pydict(param_d['parameters'], recurse=False), \
            _rlist_to_pydict(param_d['defaults']), \
            order_vars,\
            pretty_names_and_descriptions)
            

def get_pretty_names_and_descriptions_for_params(method_name, param_list):
    method_list = ro.r("lsf.str('package:openmetar')")
    pretty_names_f = "%s.pretty.names" % method_name
    params_d = {}
    if pretty_names_f in method_list:
        # try to match params to their pretty names and descriptions
        pretty_names_and_descriptions = ro.r("%s()" % pretty_names_f)
        # this dictionary is assumed to be as follows:
        #      params_d[param] --> {"pretty.name":XX, "description":XX}
        params_d = _rlist_to_pydict(pretty_names_and_descriptions) 

    # fill in entries for parameters for which pretty names/descriptions were
    # not provided-- these are just place-holders to make processing this
    # easier
    names_index = param_list.names.index("parameters") 
    param_names = param_list[names_index].names # pull out the list
    for param in param_names:
        if not param in params_d.keys():
            params_d[param] = {"pretty.name":param, "description":"None provided"}
    
    return params_d
    
def get_available_methods(for_data_type=None, data_obj_name=None, metric=None):
    '''
    Returns a list of methods available in OpenMeta for the particular data_type
    (if one is given). Excludes "*.parameters" methods
    '''
    method_list = ro.r("lsf.str('package:openmetar')")

    # the following constitute 'special' or 'reserved' function
    # names that are used by meta-analyst to parse out available
    # methods and their parameters. we exclude these from the list
    # of available meta-analytic routines.
    # 
    # by convention, the methods available for a data type (e.g., binary)
    # start with the name of the data type. furthermore, the parameters
    # for those methods are returned by a method with a name
    # ending in ".parameters"
    special_endings = [".parameters", ".is.feasible", ".overall", \
                            ".regression", "transform.f", ".pretty.names"]
    is_special = lambda f: any([f.endswith(ending) for ending in special_endings])
    all_methods = [method for method in method_list if not is_special(method)]
    if for_data_type is not None:
        all_methods = [method for method in all_methods if method.startswith(for_data_type)]


    # now, if a data object handle was provided, check which methods are feasible
    if data_obj_name is not None:
        # we will return a dictionary mapping pretty
        # names (optionally) to method names; if no pretty name exists,
        # then we just map the method name to itself.
        # note that if more than one method exists with the same pretty name
        # it will be overwritten!
        feasible_methods = {}
        for method in all_methods:
            is_feasible = True
            # we check if the author of this method has provided an is.feasible
            # routine; if so, we will call it. otherwise, we assume that we can
            # invoke the corresponding routine (i.e., we assume it's feasible)
            is_feas_f = "%s.is.feasible" % method
            if is_feas_f in method_list:
                # we need to pass along the metric along with the data 
                # object to assess if a given method is feasible (e.g,.
                # PETO for binary data only makes sense for 'OR')
                is_feasible = ro.r("%s(%s, '%s')" % (is_feas_f, data_obj_name, metric))[0]
 
            if is_feasible:
                # do we have a pretty name?
                pretty_names_f = "%s.pretty.names" % method
                if pretty_names_f in method_list:
                    pretty_name = ro.r("%s()$pretty.name" % pretty_names_f)[0]
                    feasible_methods[pretty_name] = method
                else:
                    # no? then just map to the function name
                    feasible_methods[method] = method
    return feasible_methods

def get_method_description(method_name):
    pretty_names_f = "%s.pretty.names" % method_name
    method_list = ro.r("lsf.str('package:openmetar')")
    description = "None provided."
    if pretty_names_f in method_list:
        try:
            description = ro.r("%s()$description" % pretty_names_f)[0]
        except:
            pass
    return description
    
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
        ro.r("el <- matrix(c(%s), nc=2, byrow=TRUE)" % edge_str)
        ro.r("g <- graph.edgelist(el, directed=FALSE)")
    else:
        ro.r("g <- graph.empty()") 
    
    if len(unconnected_vertices) > 0:
        print unconnected_vertices
        vertices_str = ", ".join([" '%s' " % x for x in unconnected_vertices])
        ro.r("g <- add.vertices(g, %s, name=c(%s))" % (len(unconnected_vertices), vertices_str))
    ro.r("png(%s)" % network_path)
    ro.r("plot(g, vertex.label=V(g)$name, layout=layout.circle, vertex.size=25, asp=.3, margin=-.05)")
    ro.r("dev.off()")
    return "r_tmp/network.png"
    

def ma_dataset_to_simple_continuous_robj(table_model, var_name="tmp_obj", \
                                                covs_to_include=None,
                                                studies=None):
    r_str = None
    
    if studies is None:
        # grab all studies. note: the list is pulled out in reverse order from the 
        # model, so we, er, reverse it.
        studies = table_model.get_studies()
    # the study_ids preserve the ordering
    study_ids = [study.id for study in studies]
    study_names = ", ".join(["'" + study.name + "'" for study in studies])
    
    # issue #139 -- also grab the years
    none_to_str = lambda n : str(n) if n is not None else "" # this will produce NA ints
    study_years = ", ".join(["as.integer(%s)" % none_to_str(study.year) for study in studies])

    ests, SEs = table_model.get_cur_ests_and_SEs(only_these_studies=study_ids)    
    ests_str = ", ".join(_to_strs(ests))
    SEs_str = ", ".join(_to_strs(SEs))
    
    cov_str = list_of_cov_value_objects_str(table_model.dataset,\
                                                study_ids,\
                                                cov_list=covs_to_include)


    # first try and construct an object with raw data -- note that if
    # we're using a one-armed metric for cont. data, we just use y/SE
    if (not table_model.current_effect in ONE_ARM_METRICS) and \
                         table_model.included_studies_have_raw_data():
        print "we have raw data... parsing, parsing, parsing"
            
        raw_data = table_model.get_cur_raw_data(only_these_studies=study_ids)
        Ns1_str = _get_str(raw_data, 0)
        means1_str = _get_str(raw_data, 1)
        SDs1_str = _get_str(raw_data, 2)
        Ns2_str = _get_str(raw_data, 3)
        means2_str = _get_str(raw_data, 4)
        SDs2_str = _get_str(raw_data, 5)

        r_str = "%s <- new('ContinuousData', \
                                     N1=c(%s), mean1=c(%s), sd1=c(%s), \
                                     N2=c(%s), mean2=c(%s), sd2=c(%s), \
                                     y=c(%s), SE=c(%s), study.names=c(%s),\
                                    years=c(%s), covariates=%s)" \
                        % (var_name, Ns1_str, means1_str, SDs1_str, \
                            Ns2_str, means2_str, SDs2_str, \
                            ests_str, SEs_str, study_names, study_years, cov_str)
         
    else:
        print "no raw data (or one-arm)... using effects"
        r_str = "%s <- new('ContinuousData', \
                            y=c(%s), SE=c(%s), study.names=c(%s),\
                            years=c(%s), covariates=%s)" \
                            % (var_name, ests_str, SEs_str, \
                                study_names, study_years, cov_str)
        
    # character encodings for R
    r_str = _sanitize_for_R(r_str)
    print "executing: %s" % r_str
    ro.r(r_str)
    print "ok."
    return r_str
    
    
def _get_str(M, col_index, reverse=True):
    x = _get_col(M, col_index)
    if reverse:
        x.reverse()
    return ", ".join(_to_strs(x))
    
    
def ma_dataset_to_simple_binary_robj(table_model, var_name="tmp_obj", 
                                        include_raw_data=True, covs_to_include=None,
                                        studies=None):
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
    
    if studies is None:
        # grab the study names. note: the list is pulled out in reverse order from the 
        # model, so we, er, reverse it.
        studies = table_model.get_studies(only_if_included=True)

    study_ids = [study.id for study in studies]

    # issue #139 -- also grab the years
    none_to_str = lambda n : str(n) if n is not None else "" # this will produce NA ints
    study_years = ", ".join(["as.integer(%s)" % none_to_str(study.year) for study in studies])
    study_names = ", ".join(["'" + study.name + "'" for study in studies])
    
    ests, SEs = table_model.get_cur_ests_and_SEs(only_if_included=True, only_these_studies=study_ids)
    ests_str = ", ".join(_to_strs(ests))
    SEs_str = ", ".join(_to_strs(SEs))

            
    # generate the covariate string
    cov_str = list_of_cov_value_objects_str(table_model.dataset,\
                                                study_ids,\
                                                cov_list=covs_to_include)
    

    # first try and construct an object with raw data
    if include_raw_data and table_model.included_studies_have_raw_data():
        print "ok; raw data has been entered for all included studies"
        
        # now figure out the raw data
        raw_data = table_model.get_cur_raw_data(only_these_studies=study_ids)
    
        g1_events = _get_col(raw_data, 0)
        
        g1O1_str = ", ".join(_to_strs(g1_events))
        g1_totals = _get_col(raw_data, 1)
        
        g1O2 = [(total_i-event_i) for total_i, event_i in zip(g1_totals, g1_events)]
        g1O2_str = ", ".join(_to_strs(g1O2))
    
        # now, for group 2; we only set up the string
        # for group two if we have a two-arm metric
        g2O1_str, g2O2_str = "0", "0" # the 0s are just to satisfy R; not used
        if table_model.current_effect in TWO_ARM_METRICS:  
            g2_events = _get_col(raw_data, 2)
            g2O1_str = ", ".join(_to_strs(g2_events))

            g2_totals = _get_col(raw_data, 3)
            g2O2 = [(total_i-event_i) for total_i, event_i in zip(g2_totals, g2_events)]
            g2O2_str = ", ".join(_to_strs(g2O2))
                    
        # actually creating a new object on the R side seems the path of least resistance here.
        # the alternative would be to try and create a representation of the R object on the 
        # python side, but this would require more work and I'm not sure what the benefits
        # would be
        r_str = "%s <- new('BinaryData', g1O1=c(%s), g1O2=c(%s), g2O1=c(%s), g2O2=c(%s), \
                            y=c(%s), SE=c(%s), study.names=c(%s), years=c(%s), covariates=%s)" % \
                            (var_name, g1O1_str, g1O2_str, g2O1_str, g2O2_str, \
                             ests_str, SEs_str, study_names, study_years, cov_str)

    elif table_model.included_studies_have_point_estimates():
        print "not sufficient raw data, but studies have point estimates..."

        r_str = "%s <- new('BinaryData', y=c(%s), SE=c(%s), study.names=c(%s), years=c(%s), covariates=%s)" \
                            % (var_name, ests_str, SEs_str, study_names, study_years, cov_str)
        
               
    else:
        print "there is neither sufficient raw data nor entered effects/CIs. I cannot run an analysis."
        # @TODO complain to the user here
    

    ###
    # ok, it seems R uses latin-1 for its unicode encodings,
    # whereas QT uses UTF8. this can cause situations where
    # rpy2 throws up on this call due to it not being able
    # to parse a character; so we sanitize. This isn't great,
    # because sometimes characters get garbled...
    r_str = _sanitize_for_R(r_str)
    print "executing: %s" % r_str
    ro.r(r_str)
    print "ok."
    return r_str

def _sanitize_for_R(a_str):
    # may want to do something fancier in the future...
    return a_str.encode('latin-1', 'ignore')

def ma_dataset_to_simple_diagnostic_robj(table_model, var_name="tmp_obj", \
                                            metric="Sens", covs_to_include=None,
                                            effects_on_disp_scale=False, 
                                            studies=None):
    '''
    This converts a DatasetModel to an OpenMetaData (OMData) R object. We use type DatasetModel
    rather than a DataSet model directly to access the current variables. Furthermore, this allows
    us to check which studies (if any) were excluded by the user.


    '''
    r_str = None
    
    # grab the study names. note: the list is pulled out in reverse order from the 
    # model, so we, er, reverse it.
    if studies is None:
        studies = table_model.get_studies(only_if_included=True)
    study_ids = [study.id for study in studies]

    study_names = ", ".join(["'" + study.name + "'" for study in studies])
    # issue #139 -- also grab the years
    study_years = ", ".join(["as.integer(%s)" % study.year for study in studies])

    y_ests, y_SEs = table_model.get_cur_ests_and_SEs(only_if_included=True, effect=metric)

    y_ests_str = ", ".join(_to_strs(y_ests))
    y_SEs_str = ", ".join(_to_strs(y_SEs))

    # generate the covariate string
    cov_str = list_of_cov_value_objects_str(table_model.dataset,\
                                            study_ids, \
                                            cov_list=covs_to_include)


    # first try and construct an object with raw data
    if table_model.included_studies_have_raw_data():
        print "ok; raw data has been entered for all included studies"
        
        # grab the raw data; the order is 
        # tp, fn, fp, tn
        raw_data = table_model.get_cur_raw_data()

        ### assembling TP, FP, TN and FN strings ...
        tps_str = ", ".join(_to_strs(_get_col(raw_data, 0)))
        fns_str = ", ".join(_to_strs(_get_col(raw_data, 1)))
        fps_str = ", ".join(_to_strs(_get_col(raw_data, 2)))
        tns_str = ", ".join(_to_strs(_get_col(raw_data, 3)))
        
        # actually creating a new object on the R side seems the path of least resistance here.
        # the alternative would be to try and create a representation of the R object on the 
        # python side, but this would require more work and I'm not sure what the benefits
        # would be
        r_str = "%s <- new('DiagnosticData', TP=c(%s), FN=c(%s), TN=c(%s), FP=c(%s), \
                            y=c(%s), SE=c(%s), study.names=c(%s), years=c(%s), covariates=%s)" % \
                            (var_name, tps_str, fns_str, tns_str, fps_str, \
                             y_ests_str, y_SEs_str, study_names, study_years, cov_str)
        
    elif table_model.included_studies_have_point_estimates(effect=metric):
        print "not sufficient raw data, but studies have point estimates..."

        r_str = "%s <- new('DiagnosticData', y=c(%s), SE=c(%s), study.names=c(%s), \
                                    years=c(%s), covariates=%s)" \
                            % (var_name, y_ests_str, y_SEs_str, study_names, study_years, cov_str)
                            
    else:
        print "there is neither sufficient raw data nor entered effects/CIs. I cannot run an analysis."
        # @TODO complain to the user here
    
    # character (unicode) encodings for R
    r_str = _sanitize_for_R(r_str)
    print "executing: %s" % r_str
    ro.r(r_str)
    print "ok."
    return r_str


def cov_to_str(cov, study_ids, dataset, \
                named_list=True, return_cov_vals=False):
    '''
    The string is constructured so that the covariate
    values are in the same order as the 'study_names'
    list.
    '''
    cov_str = None
    if named_list:
        cov_str = "%s=c(" % cov.name
    else:
        cov_str = "c("

    cov_value_d = dataset.get_values_for_cov(cov.name, ids_for_keys=True)
    
    # get the study ids in the same order as the names
    cov_values = []
   
    for study_id in study_ids:
        if cov.data_type == CONTINUOUS:
            if cov_value_d.has_key(study_id):
                cov_values.append("%s" % cov_value_d[study_id])
            else:
                cov_values.append("NA")
        else:
            if cov_value_d.has_key(study_id):
                # factor; note the string.
                cov_values.append("'%s'" % unicode(str(cov_value_d[study_id]).encode('latin1'), 'latin1'))
            else:
                cov_values.append("NA")
    cov_str += ",".join(cov_values) + ")"
    
    if return_cov_vals:
        return (cov_str, cov_values)
    return cov_str
        

def run_continuous_ma(function_name, params, res_name = "result", cont_data_name="tmp_obj"):
    params_df = ro.r['data.frame'](**params)
    r_str = "%s<-%s(%s, %s)" % (res_name, function_name, cont_data_name, params_df.r_repr())
    print "\n\n(run_continuous_ma): executing:\n %s\n" % r_str
    ro.r(r_str)
    result = ro.r("%s" % res_name)
    return parse_out_results(result)
    
def run_binary_ma(function_name, params, res_name="result", bin_data_name="tmp_obj"):
    params_df = ro.r['data.frame'](**params)
    r_str = "%s<-%s(%s, %s)" % (res_name, function_name, bin_data_name,\
                                    params_df.r_repr())
    print "\n\n(run_binary_ma): executing:\n %s\n" % r_str

    ro.r(r_str)
    result = ro.r("%s" % res_name)
    return parse_out_results(result)
       
def _to_R_param_str(param):
    ''' 
    Encodes Python parameters for consumption by R. Strings are single quoted,
    booleans cast to all-caps.
    '''
    if isinstance(param, str) or isinstance(param, unicode):
        return "'%s'"% param
    elif isinstance(param, bool):
        if param:
            return "TRUE"
        return "FALSE"
    return param
    
def _to_R_params(params):
    '''
    Given a Python dictionary of method arguments, this returns a string
    that represents a named list in R. 
    '''
    params_str = []
    for param in params.keys():
        params_str.append("'%s'=%s" % (param, _to_R_param_str(params[param])))
    
    params_str = "list("+ ",".join(params_str) + ")"
    return params_str

def run_diagnostic_multi(function_names, list_of_params, res_name="result", diag_data_name="tmp_obj"):
    r_params_str = "list(%s)" % ",".join([_to_R_params(p) for p in list_of_params])
    ro.r("list.of.params <- %s" % r_params_str)
    ro.r("f.names <- c(%s)" % ",".join(["'%s'" % f_name for f_name in function_names]))
    result = ro.r("multiple.diagnostic(f.names, list.of.params, %s)" % diag_data_name)

    return parse_out_results(result)

def run_diagnostic_ma(function_name, params, res_name="result", diag_data_name="tmp_obj"):
    params_str = _to_R_params(params)

    r_str = "%s<-%s(%s, %s)" % \
                        (res_name, function_name, diag_data_name, params_str) 
    
    print "\n\n(run_diagnostic_ma): executing:\n %s\n" % r_str
    ro.r(r_str)
    result = ro.r("%s" % res_name)
    return parse_out_results(result)
      
def load_vars_for_plot(params_path, return_params_dict=False):
    ''' 
    loads the three necessary (for plot generation) variables
    into R. we assume a naming convention in which params_path
    is the base, data is stored in *.data, params in *.params
    and result in *.res.
    '''
    for var in ("data", "params", "res"):
        cur_path = "%s.%s" % (params_path, var)
        if os.path.exists(cur_path):
            load_in_R(cur_path)
            print "loaded %s" % cur_path
        else:
            print "whoops -- couldn't load %s" % cur_path
            return False

    if return_params_dict:
        return _rls_to_pyd(ro.r("params"))
    return True

def write_out_plot_data(params_out_path, plot_data_name="plot.data"):
    ro.r("save.plot.data(%s, '%s')" % (plot_data_name, params_out_path))


def load_in_R(fpath):
    ''' loads what is presumed to be .Rdata into the R environment '''
    ro.r("load('%s')" % fpath)

def update_plot_params(plot_params, plot_params_name="params", \
                        write_them_out=False, outpath=None):
    # first cast the params to an R data frame to make it
    # R-palatable
    params_df = ro.r['data.frame'](**plot_params)
    ro.r("tmp.params <- %s" % params_df.r_repr())
   
    for param_name in plot_params:
        ro.r("%s$%s <- tmp.params$%s" % \
                (plot_params_name, param_name, param_name))

    if write_them_out:
        ro.r("save(tmp.params, file='%s')" % outpath)

def regenerate_plot_data(om_data_name="om.data", res_name="res",           
                            plot_params_name="params", plot_data_name="plot.data"):
    
    ####
    # this is crude, but works for now, and easier than making
    # the results_window keep track of why type of data it's
    # displaying. may need to re-think this ain any case for the
    # general case of plots (what 'type' is a mixed analysis, e.g.?)
    ####
    data_type = str(ro.r("class(%s)" % om_data_name))

    if "BinaryData" in data_type:
        ro.r("plot.data<-create.plot.data.binary(%s, %s, %s)" % \
                            (om_data_name, plot_params_name, res_name))
    elif "ContinuousData" in data_type:
        ro.r("plot.data<-create.plot.data.continuous(%s, %s, %s)" % \
                            (om_data_name, plot_params_name, res_name))
    else:
        ro.r("plot.data<-create.plot.data.diagnostic(%s, %s, %s)" % \
                            (om_data_name, plot_params_name, res_name))


def generate_reg_plot(file_path, params_name="plot.data"): 
    ro.r("meta.regression.plot(%s, '%s')" % (params_name, file_path))

def generate_forest_plot(file_path, side_by_side=False, params_name="plot.data"):
    if side_by_side:
        print "generating a side-by-side forest plot..."
        ro.r("two.forest.plots(%s, '%s')" % (params_name, file_path))
    else:
        ro.r("forest.plot(%s, '%s')" % (params_name, file_path))

def parse_out_results(result):
    # parse out text field(s). note that "plot names" is 'reserved', i.e., it's
    # a special field which is assumed to contain the plot variable names
    # in R (for graphics manipulation).
    text_d = {}
    image_var_name_d, image_params_paths_d, image_path_d  = {}, {}, {}
    image_order = None

    for text_n, text in zip(list(result.names), list(result)):
        # some special cases, notably the plot names and the path for a forest
        # plot. TODO in the case of diagnostic data, we're probably going to 
        # need to parse out multiple forest plot param objects...
        print text_n
        print "\n--------\n"
        if text_n == "images":
            image_path_d = _rls_to_pyd(text)
        elif text_n == "image_order":
            image_order = [x for x in text]
        elif text_n == "plot_names":
            if str(text) == "NULL":
                image_var_name_d = {}
            else:
                image_var_name_d = _rls_to_pyd(text)
        elif text_n == "plot_params_paths":
            if str(text) == "NULL":
                image_params_paths_d = {}
            else:
                image_params_paths_d = _rls_to_pyd(text)
        else:
            text_d[text_n]=text
            # Construct List of Weights for studies
            if text_n.rfind("Summary") != -1:
                summary_dict = _grlist_to_pydict(text)
                try:
                    if "study.names" in summary_dict['MAResults']: # this is a silly thing to look for but its something I explicitly set in the random methods so I know it's there
                        text_n_withoutSummary = text_n.replace("Summary","")
                        text_n_withoutSummary.strip()
                        key_name = text_n_withoutSummary + " Weights"
                        key_name.strip()
                        
                        study_names = summary_dict['MAResults']['study.names']
                        study_years = summary_dict['MAResults']['study.years']
                        study_weights = summary_dict['MAResults']['study.weights']
                        max_len = max([len(name) for name in study_names])
                        weights_txt = "studies" + " "*(max_len-1) + "weights\n"
                        
                        for name,year,weight in zip(study_names, study_years, study_weights):
                            weights_txt += "{0:{name_width}} {1} {2:4.1f}%\n".format(name, year, weight*100, name_width=max_len)
                        text_d[key_name] = weights_txt
                except:
                    print("In parse-out results:")
                    pyqtRemoveInputHook()
                    pdb.set_trace()
                    
            


    return {"images":image_path_d,
            "image_var_names":image_var_name_d,
            "texts":text_d,
            "image_params_paths":image_params_paths_d,
            "image_order":image_order}
                
                                       
def run_binary_fixed_meta_regression(selected_cov, bin_data_name="tmp_obj", \
                                        res_name="result"):
    method_str = "FE"                                        
    # equiavlent to params <- list(conf.level=95, digits=3)
    params = {"conf.level":95, "digits":3, "method":method_str}
    params_df = ro.r['data.frame'](**params)
    r_str = "%s<-binary.fixed.meta.regression(%s, %s, %s)" % \
            (res_name, bin_data_name, params_df.r_repr(), "'"+ selected_cov + "'")
    print "\n\n(run_binary_ma): executing:\n %s\n" % r_str
    ro.r(r_str)
    result = ro.r("%s" % res_name)
    return parse_out_results(result)
    
def _gen_cov_vals_obj_str(cov, study_ids, dataset): 
    values_str, cov_vals = cov_to_str(cov, study_ids, dataset, \
                            named_list=False, return_cov_vals=True)
    ref_var = cov_vals[0].replace("'", "") # arbitrary

    ## setting the reference variable to the first entry
    # for now -- this only matters for factors, obviously

    r_str = "new('CovariateValues', cov.name='%s', cov.vals=%s, \
                    cov.type='%s', ref.var='%s')" % \
                (cov.name, values_str, TYPE_TO_STR_DICT[cov.data_type], ref_var)
    return r_str


def list_of_cov_value_objects_str(dataset, study_ids, cov_list=None):
    r_cov_str = []
    if cov_list is None:
        # then use all covariates that belong to the dataset
        cov_list = dataset.covariates
    for cov in cov_list:
        r_cov_str.append(_gen_cov_vals_obj_str(cov, study_ids, dataset))
    r_cov_str = "list(" + ",".join(r_cov_str) + ")"

    return r_cov_str

def run_meta_regression(dataset, study_names, cov_list, metric_name,\
                        data_name="tmp_obj", results_name="results_obj",\
                        fixed_effects=False): 
                        
    method_str = "FE" if fixed_effects else "DL"    

    # @TODO conf.level, digits should be user-specified
    params = {"conf.level":95, "digits":3, "method":method_str, 
                "rm.method":"ML", "measure":metric_name}
    params_df = ro.r['data.frame'](**params)

    # create a lit of covariate objects on the R side
    r_str = "%s<- meta.regression(%s, %s)" % \
                            (results_name, data_name, params_df.r_repr())


    print "\n\n(run_meta_regression): executing:\n %s\n" % r_str

    ### TODO -- this is hacky

    ro.r(r_str)
    result = ro.r("%s" % results_name)

    if "try-error" in str(result):
        # uh-oh, there was an error (but the weird
        # RRunTimeError alluded to above; this is a 
        # legit error returned from an R routine)
        return str([msg for msg in result][0])
 

    parsed_results = parse_out_results(result)

    return parsed_results
  
def run_meta_method_diag(meta_function_name, function_names, list_of_params,\
                            res_name="result", diag_data_name="tmp_obj"):
    # list of parameter objects
    r_params_str = "list(%s)" % ",".join([_to_R_params(p) for p in list_of_params])
    ro.r("list.of.params <- %s" % r_params_str)
    # list of function names
    ro.r("f.names <- c(%s)" % ",".join(["'%s'" % f_name for f_name in function_names]))

    multi_meta_function_name = \
        {"loo.ma.diagnostic":"multiple.loo.diagnostic",\
         "subgroup.ma.diagnostic":"multiple.subgroup.diagnostic"}[meta_function_name]

    result = ro.r("%s(f.names, list.of.params, %s)" % (multi_meta_function_name, diag_data_name))
    
    return parse_out_results(result)
        

def run_meta_method(meta_function_name, function_name, params, \
                        res_name="result", data_name="tmp_obj"):
    '''
    Runs a binary `meta` method over the data in the bin_data_name argument
    (on the R side). The meta-method called is specified by the meta_function_name
    argument. 
    '''
    params_df = ro.r['data.frame'](**params)
    r_str = "%s<-%s('%s', %s, %s)" % \
            (res_name, meta_function_name, function_name, data_name, params_df.r_repr())

    print "\n\n(run_meta_method): executing:\n %s\n" % r_str

    ro.r(r_str)
    result = ro.r("%s" % res_name)
    
    # parse out text field(s). note that "plot names" is 'reserved', i.e., it's
    # a special field which is assumed to contain the plot variable names
    # in R (for graphics manipulation).
    return parse_out_results(result)  
          
                                                                                  
def _rls_to_pyd(r_ls):
    # base case is that the type is a native python type, rather
    # than an Rvector
    d = {}
    for name, val in zip(r_ls.names, r_ls):
        ###
        # I know we shouldn't wrap the whole thing in a (generic) try block,
        # but rpy2 can throw some funky exceptions...
        try:
            # first check the key
            if str(name) != "NULL":
                if "rpy2.robjects" in str(type(name)):
                    name = str(name[0])
                if not "rpy2.robjects" in str(type(val)):
                    # base case; not an rtype object
                    d[name] = val
                elif str(val)=="NULL":
                    d[name] = None
                elif str(val.names=="NULL"):
                    ###
                    # 11/28/11 -- swapping val[0] for the as.character
                    # version below to avoid parsing an ugly structure.
                    # this seems to be sane. did I mention we need
                    # unit tests???
                    #d[name] = val[0]
                    d[name] = ro.r("as.character(%s)" % val.r_repr())[0]
                else:
                    # recurse
                    d[name] = _rls_to_pyd(val)
                if not isinstance(name, str):
                    raise Exception, "arg"
            else:
                # name is null
                return val

        except Exception,  inst:
            print "error parsing R tuple.. here's the exception "
            print inst
            print "ignoring."

    return d


def _is_a_list(x):
    # @TODO add additional vector types?
    return type(x) in [rpy2.robjects.vectors.StrVector, 
                       rpy2.robjects.vectors.ListVector]

def _rlist_to_pydict(r_ls, recurse=True):
    '''
    parse rpy2 data structure into analogous Python
    dictionary. if the recursive flag is true, this is 
    done recursively, i.e., if a key points to an R
    list, that list will be converted, too. 
    '''
    d = {}
    names = r_ls.names

    for name, val in zip(names, r_ls):
        print "name {0}, val {1}".format(name, val)
        if recurse and _is_a_list(val) and not str(val.names)=="NULL":
            print "recursing... \n"
            d[name] = _rlist_to_pydict(val)

        cur_x = list(val)
        if len(cur_x) == 1:
            # if it's a singleton, extract the
            # the value and stick it in the dict.
            # -- this is essentially the 'base case'
            d[name] = cur_x[0]
        elif not recurse:
            d[name] = cur_x # not a singleton list

    return d

def _get_c_str_for_col(m, i):
    return ", ".join(_get_col(m, i))

def _to_strs(v):
    return [str(x) for x in v]

def _get_col(m, i):
    col_vals = []
    for x in m:
        col_vals.append(x[i])
    return col_vals

def diagnostic_effects_for_study(tp, fn, fp, tn, metrics=["Spec", "Sens"]):
    # first create a diagnostic data object
    r_str = "diag.tmp <- new('DiagnosticData', TP=c(%s), FN=c(%s), TN=c(%s), FP=c(%s))" % \
                            (tp, fn, tn, fp)
                            
    print "\n\n(diagnostic_effects_for_study): executing:\n %s\n" % r_str
    ro.r(r_str)
    
    # this will map metrics to est., lower, upper
    effects_dict = {}
    for metric in metrics:
        ###
        # Curiously (annoyingly), updating the params dictionary, then recasting it using the
        # ro.r['data.frame'](** params) call will not overwrite the existing
        # structure on the R side -- i.e., you will keep getting the same metric
        # here. Hence the somewhat ugly strategy of constructing the whole
        # named list on the R side anew on each iteration
        #####

        r_res = ro.r("get.res.for.one.diag.study(diag.tmp,\
                        list('to'='only0', 'measure'='%s', 'conf.level'=95, 'adjust'=.5))" % metric)   
        
        est, lower, upper = r_res[0][0], r_res[1][0], r_res[2][0]
        calc_estimates = (est, lower, upper)
        disp_estimates = [diagnostic_convert_scale(x, metric) for x in calc_estimates]
        effects_dict[metric] = {"calc_scale":calc_estimates, "display_scale":disp_estimates}
        

    return effects_dict
    
    
def continuous_effect_for_study(n1, m1, sd1, se1=None, n2=None, \
                                        m2=None, sd2=None, se2=None, \
                                        metric="MD", two_arm=True, conf_level=.975):
    
    point_est, se = None, None
    if two_arm:
        if not None in [se1, se2] and metric=="MD":
            # in this case, we have means & standard errors (but no sample size/ sds)
            # thus we compute the point estimate and se directly
            point_est = m1-m2
            se = math.sqrt(sum([x**2 for x in [se1, se2]]))
        else:
            r_str = "escalc('%s', n1i=c(%s), n2i=c(%s), m1i=c(%s), m2i=c(%s), sd1i=c(%s), sd2i=c(%s))" %\
                                (metric, n1, n2, m1, m2, sd1, sd2)
    
            
            effect = ro.r(r_str)
            # the first 0 indexes into the study; the second, into the point estimate
            # (the escalc method is general and thus expects an array of studies)
            point_est = effect[0][0]
            se = math.sqrt(effect[1][0])
    else:
        # only one-arm
        point_est = m1
        se = sd1/n1
    
    r_str =  "qnorm(%s)" % conf_level
    mult = ro.r(r_str)[0]
    lower, upper = (point_est-mult*se, point_est+mult*se)
    est_and_ci = (point_est, lower, upper)
    transformed_est_and_ci = continuous_convert_scale(est_and_ci, metric)
    return {"calc_scale":est_and_ci, "display_scale":transformed_est_and_ci}
    
def effect_for_study(e1, n1, e2=None, n2=None, two_arm=True, 
                metric="OR", conf_level=.975):
    '''
    Computes a point estimate, lower & upper bound for
    the parametric 2x2 *binary* table data.

    @TODO add support for non-normal (e.g., T) distributions

    @params
    ===
    e1 -- events in group 1
    n1 -- size of group 1
    e2 -- events in group 2
    n2 -- size of group 2
    --
    '''
    print metric
    r_str = None
    if two_arm:
        # notice that we're using WV's escalc routine here
        r_str = "escalc(measure='%s', ai=c(%s), n1i=c(%s), ci=c(%s), n2i=c(%s))" %\
                        (metric, e1, n1, e2, n2)
    else:
        r_str = "escalc(measure='%s', xi=c(%s), ni=c(%s))" % (metric, e1, n1)        
             
    #print "calling out to R: %s" % r_str
    effect = ro.r(r_str)

    #print "result: %s" % effect
    point_est = effect[0][0]
    se = math.sqrt(effect[1][0])
    
    #print "point_est: ", point_est
    #print "var:", effect[1][0]

    # scalar for computing confidence interval
    r_str = "qnorm(%s)" % conf_level
    mult = ro.r(r_str)[0]

    # note that the point estimate, lower & upper are all computed
    # and returned on the calculation scale (e.g., log in the case of
    # ratios)
    lower, upper = (point_est-mult*se, point_est+mult*se)
    
    print "%s, %s, %s" % (lower, point_est, upper)

    # we return both the transformed and untransformed scales here
    est_and_ci = (point_est, lower, upper)
    transformed_est_and_ci = binary_convert_scale(est_and_ci, metric)
    return {"calc_scale":est_and_ci, "display_scale":transformed_est_and_ci}

def binary_convert_scale(x, metric_name, convert_to="display.scale"):
    # convert_to is either 'display.scale' or 'calc.scale'
    return generic_convert_scale(x, metric_name, "binary", convert_to)
    
def continuous_convert_scale(x, metric_name, convert_to="display.scale"):
    return generic_convert_scale(x, metric_name, "continuous", convert_to)
    
def diagnostic_convert_scale(x, metric_name, convert_to="display.scale"):
    return generic_convert_scale(x, metric_name, "diagnostic", convert_to)

    
def generic_convert_scale(x, metric_name, data_type, convert_to="display.scale"):
    ro.r("trans.f <- %s.transform.f('%s')" % (data_type, metric_name))

    islist = isinstance(x, list) or isinstance(x, tuple) # being loose with what qualifies as a 'list' here.
    if islist:
        ro.r("x <- c%s" % str(x))
    else:
        ro.r("x <- %s" % str(x))
    transformed = ro.r("trans.f$%s(x)" % convert_to)
    transformed_ls = [x_i for x_i in transformed]
    if not islist:
        # scalar
        return transformed_ls[0]
    return transformed_ls
    
