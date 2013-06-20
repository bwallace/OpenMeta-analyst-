####################################
#                                  #
# unit tests for meta_py_r module  #
#                                  #
# Author: George Dietz             #
#         CEBM@Brown               #
#                                  #
####################################

''' Desired tests:
Regular meta-analysis:
[X] binary data * analysis methods
[X] continuous data * analysis methods
[ ] diagnostic data * sens/spec * method (4)
                    * dor/lor   * method (3)

Cumulative meta-analysis:
[X] binary data * analysis methods
[X] continuous data * analysis methods
[ ] diagnostic data * analysis methods

Subgroup meta-analysis: (factor covariate)
[ ] binary data * analysis method (4)
[ ] continuous data * analysis method (2)
[ ] diagnostic data * sens * method
                      spec * method
                      dor  * method
                      lor  * method


leave-one-out meta-analysis:
[X] binary data * analysis methods
[X] continuous data * analysis methods
[ ] diagnostic data * sens/spec * method (4)
                    * dor/lor   * method (3)

meta-regression:
[ ] binary data
    [ ] continuous covariate
    [ ] factor covariate
[ ] continuous data
    [ ] continuous covariate
    [ ] factor covariate
[ ] diagnostic data
    [ ] continuous covariate
    [ ] factor covariate
'''

import nose
from nose import with_setup, tools
import os, sys

from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *


#import meta_py_r # this needs to come first or else weird stuff happens w/ nose
#import meta_globals

print("Importing meta_globals")
#import meta_globals
print("Importing meta_form")
import meta_form
print("Importing meta_py_r")
import meta_py_r


from types import (NoneType, BooleanType, IntType, LongType, FloatType,
                   StringType, UnicodeType, ListType, DictType, TupleType)

simple_types = [NoneType, BooleanType, IntType, LongType, FloatType,
                StringType, UnicodeType]

def _compare_dicts(dA, dB):
    '''dA and dB are two dictionaries who have some elements which are dictionaries, etc. Otherwise the elements are just non-iterable types'''
    
    if set(dA.keys()) != set(dB.keys()):
        return False

    # keys are verified the same at this point
    for k,val_A in dA.items():
        val_B = dB[k]
        
        # must be the same type
        if type(val_A) != type(val_B):
            return False
        data_type = type(val_A)
        
        if data_type == DictType:
            equal_vals = _compare_dicts(val_A, val_B)
        else:
            equal_vals = _compare_values(val_A, val_B)
            
        if not equal_vals:
            return False
    
    return True # if we managed to get here despite all the checks we are golden
        
def _compare_values(valA, valB):
    ''' Returns true if the values of valA and valB are equal. Deals with
    lists, tuples, dictionaries as well as the normal basic types  '''
    
    if type(valA) != type(valB):
        return False
    
    data_type = type(valA)
    
    if data_type in [ListType, TupleType]:
        if len(valA) != len(valB):
            return False
        for x,y in zip(valA, valB):
            return _compare_values(x,y)
    elif data_type == DictType:
        return _compare_dicts(valA, valB)
    elif data_type in simple_types:
        return valA == valB
    else:
        print("Type %s not recognized, cannot compare" % data_type)
        raise TypeError("Type %s not recognized, cannot compare" % data_type)


app,meta = None, None

def setup_module(module): # runs before any method in the class
    print("")
    print("ENTERING SETUP MODULE ------------------------------------------------")
    
    # load R libraries
    rloader = meta_py_r.RlibLoader()
    rloader.load_all()
    
    global app, meta
    app = QtGui.QApplication(sys.argv)
    meta = meta_form.MetaForm()
    #meta.tableView.setSelectionMode(QTableView.ContiguousSelection)
    meta.show()
    app.processEvents()
    
    
def teardown_module(module):
    QApplication.quit()

################### BINARY META ANALYSIS TESTS ################################

#def test_dummy():
#    print("Running test dummy")
#    assert True
        
def test_binary_meta_analysis():
    
    fullpath = os.path.join(os.getcwd(),"../sample_data", "amino.oma")

    meta.open(fullpath)
    meta_py_r.ma_dataset_to_simple_binary_robj(meta.model)
    
    method_and_params = []
    method_and_params.append({'method':"binary.random",
                              'parameters':{'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results':{'images': {'Forest Plot': './r_tmp/forest.png'}, 'texts': {'Weights': 'studies             weights\nGonzalez       1993  7.3%\nPrins          1993  6.2%\nGiamarellou    1991  2.1%\nMaller         1993 10.7%\nSturm          1989  2.0%\nMarik          1991 12.2%\nMuijsken       1988  7.5%\nVigano         1992  1.8%\nHansen         1988  5.3%\nDe Vries       1990  6.1%\nMauracher      1989  2.2%\nNordstrom      1990  5.3%\nRozdzinski     1993 10.3%\nTer Braak      1990  8.7%\nTulkens        1988  1.2%\nVan der Auwera 1991  2.0%\nKlastersky     1977  6.0%\nVanhaeverbeek  1993  1.2%\nHollender      1989  1.8%\n', 'Summary': 'Binary Random-Effects Model\n\nMetric: Odds Ratio\n\n Model Results\n\n Estimate  Lower bound   Upper bound   p-Value  \n\n 0.770        0.485         1.222       0.267   \n\n\n Heterogeneity\n\n tau^2  Q(df=18)   Het. p-Value   I^2  \n\n 0.378   33.360        0.015      46%  \n\n\n Results (log scale)\n\n Estimate  Lower bound   Upper bound   Std. error  \n\n -0.262      -0.724         0.200         0.236    \n\n\n'}, 'image_var_names': {'forest plot': 'forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371560122.39001'}, 'image_order': None},
                              })
    method_and_params.append({'method':"binary.fixed.peto",
                              'parameters':{'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results':{'images': {'Forest Plot': './r_tmp/forest.png'}, 'texts': {'References': '[1] "Yusuf, S., Peto, R., Lewis, J., Collins, R., & Sleight, P. (1985). Beta blockade during and after myocardial infarction: An overview of the randomized trials. Progress in Cardiovascular Disease, 27, 335-371."\n', 'Weights': 'studies             weights\nGonzalez       1993  5.1%\nPrins          1993  4.3%\nGiamarellou    1991  2.5%\nMaller         1993 13.3%\nSturm          1989  1.3%\nMarik          1991 26.0%\nMuijsken       1988  5.2%\nVigano         1992  0.9%\nHansen         1988  4.7%\nDe Vries       1990  3.9%\nMauracher      1989  3.1%\nNordstrom      1990  2.9%\nRozdzinski     1993 11.5%\nTer Braak      1990  7.3%\nTulkens        1988  0.5%\nVan der Auwera 1991  1.3%\nKlastersky     1977  4.7%\nVanhaeverbeek  1993  0.5%\nHollender      1989  0.9%\n', 'Summary': 'Binary Fixed-Effect Model - Peto\n\nMetric: Odds Ratio\n\n Model Results\n\n Estimate  Lower bound   Upper bound   p-Value  \n\n 0.718        0.550         0.937       0.015   \n\n\n Heterogeneity\n\n Q(df=18)  Het. p-Value   I^2  \n\n 37.229        0.005      52%  \n\n\n Results (log scale)\n\n Estimate  Lower bound   Upper bound   Std. error  \n\n -0.331      -0.597        -0.065         0.136    \n\n\n'}, 'image_var_names': {'forest plot': 'forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371560197.03328'}, 'image_order': None},
                              })
    method_and_params.append({'method':"binary.fixed.mh",
                              'parameters':{'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results':{'images': {'Forest Plot': './r_tmp/forest.png'}, 'texts': {'References': '[1] "Mantel, N., & Haenszel, W. (1959) Statistical aspects of the analysis of data from retrospective studies of disease. Journal of the National Cancer Institute, 22, 719-748."\n', 'Weights': 'studies             weights\nGonzalez       1993  5.6%\nPrins          1993  5.1%\nGiamarellou    1991  4.0%\nMaller         1993  9.6%\nSturm          1989  1.6%\nMarik          1991 33.8%\nMuijsken       1988  3.4%\nVigano         1992  0.0%\nHansen         1988  7.8%\nDe Vries       1990  2.3%\nMauracher      1989  4.8%\nNordstrom      1990  3.0%\nRozdzinski     1993  9.9%\nTer Braak      1990  5.4%\nTulkens        1988  0.0%\nVan der Auwera 1991  1.6%\nKlastersky     1977  1.3%\nVanhaeverbeek  1993  0.0%\nHollender      1989  0.8%\n', 'Summary': 'Binary Fixed-Effect Model - Mantel Haenszel\n\nMetric: Odds Ratio\n\n Model Results\n\n Estimate  Lower bound   Upper bound   p-Value  \n\n 0.712        0.543         0.934       0.014   \n\n\n Heterogeneity\n\n Q(df=18)  Het. p-Value   I^2  \n\n 33.557        0.014      46%  \n\n\n Results (log scale)\n\n Estimate  Lower bound   Upper bound   Std. error  \n\n -0.340      -0.611        -0.069         0.138    \n\n\n'}, 'image_var_names': {'forest plot': 'forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371560599.03599'}, 'image_order': None},
                              })
    method_and_params.append({'method':"binary.fixed.inv.var",
                              'parameters':{'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results':{'images': {'Forest Plot': './r_tmp/forest.png'}, 'texts': {'Weights': 'studies             weights\nGonzalez       1993  5.6%\nPrins          1993  4.2%\nGiamarellou    1991  0.9%\nMaller         1993 14.8%\nSturm          1989  0.9%\nMarik          1991 27.6%\nMuijsken       1988  5.8%\nVigano         1992  0.8%\nHansen         1988  3.1%\nDe Vries       1990  4.0%\nMauracher      1989  1.0%\nNordstrom      1990  3.2%\nRozdzinski     1993 13.2%\nTer Braak      1990  8.3%\nTulkens        1988  0.5%\nVan der Auwera 1991  0.9%\nKlastersky     1977  3.9%\nVanhaeverbeek  1993  0.5%\nHollender      1989  0.8%\n', 'Summary': 'Binary Fixed-Effect Model - Inverse Variance\n\nMetric: Odds Ratio\n\n Model Results\n\n Estimate  Lower bound   Upper bound   p-Value  \n\n 0.760        0.571         1.011       0.059   \n\n\n Heterogeneity\n\n Q(df=18)  Het. p-Value   I^2  \n\n 33.360        0.015      46%  \n\n\n Results (log scale)\n\n Estimate  Lower bound   Upper bound   Std. error  \n\n -0.275      -0.561         0.011         0.146    \n\n\n'}, 'image_var_names': {'forest plot': 'forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371560784.58866'}, 'image_order': None},
                              })
    
    # for now just check that images and texts match
    for t in method_and_params:
        check_binary_meta_analysis.description = "Testing Binary Meta Analysis %s" % t['method']
        yield check_binary_meta_analysis, t
        
def test_binary_meta_analysis_meta_methods():
    fullpath = os.path.join(os.getcwd(),"../sample_data", "amino.oma")

    meta.open(fullpath)
    meta_py_r.ma_dataset_to_simple_binary_robj(meta.model)
    
    method_and_params = []
    #### CUMULATIVE MA BINARY #####
    method_and_params.append({'meta_f_str': "cum.ma.binary",
                              'method'    : "binary.random",
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Cumulative Forest Plot': './r_tmp/forest.png'}, 'texts': {'Cumulative Summary': 'Binary Random-Effects Model\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Gonzalez            0.571       0.170         1.916         0.617       NA   \n\n + Prins             0.509       0.204         1.273         0.467     0.149  \n\n + Giamarellou       0.430       0.180         1.032         0.446     0.059  \n\n + Maller            0.657       0.268         1.609         0.457     0.358  \n\n + Sturm             0.614       0.265         1.422         0.428     0.255  \n\n + Marik             0.540       0.275         1.060         0.344     0.073  \n\n + Muijsken          0.634       0.334         1.205         0.328     0.164  \n\n + Vigano            0.668       0.358         1.246         0.318     0.205  \n\n + Hansen            0.572       0.305         1.073         0.321     0.082  \n\n + De Vries          0.646       0.352         1.188         0.310     0.160  \n\n + Mauracher         0.594       0.323         1.095         0.312     0.095  \n\n + Nordstrom         0.606       0.345         1.062         0.287     0.080  \n\n + Rozdzinski        0.655       0.398         1.078         0.255     0.096  \n\n + Ter Braak         0.706       0.442         1.127         0.239     0.145  \n\n + Tulkens           0.712       0.453         1.121         0.231     0.143  \n\n + Van der Auwera    0.696       0.446         1.086         0.227     0.110  \n\n + Klastersky        0.775       0.477         1.258         0.247     0.302  \n\n + Vanhaeverbeek     0.780       0.486         1.253         0.241     0.304  \n\n + Hollender         0.770       0.485         1.222         0.236     0.267  \n\n\n'}, 'image_var_names': {'cumulative forest plot': 'cumulative_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371575868.36483'}, 'image_order': None},
                              })
    method_and_params.append({'meta_f_str': "cum.ma.binary",
                              'method'    : "binary.fixed.peto",
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Cumulative Forest Plot': './r_tmp/forest.png'}, 'texts': {'Cumulative Summary': 'Binary Fixed-effect Model - Peto\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error    p-Val   \n\n Gonzalez            0.571       0.170         1.916         0.617        NA    \n\n + Prins             0.522       0.219         1.246         0.444      0.143   \n\n + Giamarellou       0.407       0.188         0.881         0.394      0.022   \n\n + Maller            0.779       0.458         1.325         0.271      0.357   \n\n + Sturm             0.736       0.439         1.234         0.264      0.245   \n\n + Marik             0.547       0.379         0.790         0.187      0.001   \n\n + Muijsken          0.603       0.424         0.856         0.179      0.005   \n\n + Vigano            0.617       0.435         0.873         0.177      0.006   \n\n + Hansen            0.566       0.405         0.791         0.171     < 0.001  \n\n + De Vries          0.608       0.439         0.841         0.166      0.003   \n\n + Mauracher         0.575       0.419         0.789         0.162     < 0.001  \n\n + Nordstrom         0.578       0.424         0.789         0.159     < 0.001  \n\n + Rozdzinski        0.625       0.468         0.834         0.147      0.001   \n\n + Ter Braak         0.664       0.503         0.876         0.142      0.004   \n\n + Tulkens           0.665       0.504         0.877         0.141      0.004   \n\n + Van der Auwera    0.656       0.499         0.863         0.140      0.003   \n\n + Klastersky        0.722       0.552         0.943         0.137      0.017   \n\n + Vanhaeverbeek     0.723       0.553         0.944         0.136      0.017   \n\n + Hollender         0.718       0.550         0.937         0.136      0.015   \n\n\n'}, 'image_var_names': {'cumulative forest plot': 'cumulative_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371576749.90466'}, 'image_order': None},
                          })
    method_and_params.append({'meta_f_str': "cum.ma.binary",
                              'method'    : "binary.fixed.mh",
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Cumulative Forest Plot': './r_tmp/forest.png'}, 'texts': {'Cumulative Summary': 'Binary Fixed-effect Model - Mantel Haenszel\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error    p-Val   \n\n Gonzalez            0.571       0.170         1.916         0.617        NA    \n\n + Prins             0.507       0.203         1.263         0.466      0.145   \n\n + Giamarellou       0.369       0.156         0.875         0.440      0.024   \n\n + Maller            0.775       0.453         1.327         0.274      0.353   \n\n + Sturm             0.727       0.428         1.235         0.270      0.239   \n\n + Marik             0.539       0.370         0.786         0.193      0.001   \n\n + Muijsken          0.598       0.418         0.854         0.182      0.005   \n\n + Vigano            0.610       0.428         0.870         0.181      0.006   \n\n + Hansen            0.558       0.396         0.787         0.175     < 0.001  \n\n + De Vries          0.603       0.433         0.839         0.169      0.003   \n\n + Mauracher         0.566       0.408         0.784         0.167     < 0.001  \n\n + Nordstrom         0.569       0.413         0.784         0.163     < 0.001  \n\n + Rozdzinski        0.618       0.460         0.830         0.151      0.001   \n\n + Ter Braak         0.659       0.497         0.874         0.144      0.004   \n\n + Tulkens           0.659       0.497         0.874         0.144      0.004   \n\n + Van der Auwera    0.648       0.490         0.859         0.143      0.003   \n\n + Klastersky        0.718       0.547         0.942         0.139      0.017   \n\n + Vanhaeverbeek     0.718       0.547         0.942         0.139      0.017   \n\n + Hollender         0.712       0.543         0.934         0.138      0.014   \n\n\n'}, 'image_var_names': {'cumulative forest plot': 'cumulative_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371576826.62302'}, 'image_order': None},
                          })
    method_and_params.append({'meta_f_str': "cum.ma.binary",
                              'method'    : "binary.fixed.inv.var",
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Cumulative Forest Plot': './r_tmp/forest.png'}, 'texts': {'Cumulative Summary': 'Binary Fixed-effect Model - Inverse Variance\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Gonzalez            0.571       0.170         1.916         0.617       NA   \n\n + Prins             0.509       0.204         1.273         0.467     0.149  \n\n + Giamarellou       0.430       0.180         1.032         0.446     0.059  \n\n + Maller            0.854       0.485         1.505         0.289     0.586  \n\n + Sturm             0.813       0.466         1.419         0.284     0.466  \n\n + Marik             0.562       0.381         0.829         0.199     0.004  \n\n + Muijsken          0.622       0.430         0.901         0.189     0.012  \n\n + Vigano            0.635       0.440         0.917         0.187     0.015  \n\n + Hansen            0.589       0.412         0.843         0.183     0.004  \n\n + De Vries          0.633       0.447         0.896         0.177     0.010  \n\n + Mauracher         0.614       0.435         0.867         0.176     0.006  \n\n + Nordstrom         0.616       0.440         0.863         0.172     0.005  \n\n + Rozdzinski        0.666       0.489         0.908         0.158     0.010  \n\n + Ter Braak         0.709       0.528         0.954         0.151     0.023  \n\n + Tulkens           0.711       0.529         0.955         0.151     0.023  \n\n + Van der Auwera    0.702       0.523         0.942         0.150     0.018  \n\n + Klastersky        0.764       0.573         1.018         0.147     0.066  \n\n + Vanhaeverbeek     0.765       0.574         1.019         0.146     0.067  \n\n + Hollender         0.760       0.571         1.011         0.146     0.059  \n\n\n'}, 'image_var_names': {'cumulative forest plot': 'cumulative_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371576934.50447'}, 'image_order': None},
                          })
    
    #### LEAVE ONE-OUT BINARY ######
    method_and_params.append({'meta_f_str': u'loo.ma.binary',
                              'method'    : 'binary.random',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Leave-one-out Forest plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Binary Random-Effects Model\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Overall             0.770       0.485         1.222         0.236     0.267  \n\n - Gonzalez          0.783       0.477         1.285         0.253     0.332  \n\n - Prins             0.796       0.489         1.296         0.249     0.359  \n\n - Giamarellou       0.811       0.513         1.281         0.233     0.369  \n\n - Maller            0.715       0.437         1.172         0.252     0.184  \n\n - Sturm             0.791       0.494         1.265         0.240     0.327  \n\n - Marik             0.864       0.543         1.375         0.237     0.538  \n\n - Muijsken          0.724       0.446         1.175         0.247     0.191  \n\n - Vigano            0.750       0.468         1.200         0.240     0.230  \n\n - Hansen            0.851       0.542         1.336         0.230     0.484  \n\n - De Vries          0.724       0.449         1.166         0.243     0.184  \n\n - Mauracher         0.813       0.515         1.283         0.233     0.375  \n\n - Nordstrom         0.772       0.474         1.257         0.249     0.298  \n\n - Rozdzinski        0.738       0.442         1.231         0.261     0.244  \n\n - Ter Braak         0.726       0.443         1.189         0.252     0.204  \n\n - Tulkens           0.764       0.476         1.227         0.242     0.266  \n\n - Van der Auwera    0.791       0.494         1.265         0.240     0.327  \n\n - Klastersky        0.696       0.458         1.059         0.214     0.091  \n\n - Vanhaeverbeek     0.764       0.476         1.226         0.242     0.265  \n\n - Hollender         0.780       0.486         1.253         0.241     0.304  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371578321.65222'}, 'image_order': None},
                      })
    method_and_params.append({'meta_f_str': u'loo.ma.binary',
                              'method'    : 'binary.fixed.peto',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Leave-one-out Forest plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Binary Fixed-effect Model - Peto\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Overall             0.718       0.550         0.937         0.136     0.015  \n\n - Gonzalez          0.726       0.553         0.954         0.139     0.022  \n\n - Prins             0.732       0.558         0.961         0.139     0.025  \n\n - Giamarellou       0.746       0.570         0.977         0.138     0.033  \n\n - Maller            0.649       0.487         0.863         0.146     0.003  \n\n - Sturm             0.728       0.557         0.952         0.137     0.020  \n\n - Marik             0.878       0.645         1.197         0.158     0.411  \n\n - Muijsken          0.687       0.523         0.903         0.139     0.007  \n\n - Vigano            0.710       0.543         0.927         0.136     0.012  \n\n - Hansen            0.766       0.583         1.006         0.139     0.055  \n\n - De Vries          0.690       0.526         0.905         0.139     0.007  \n\n - Mauracher         0.752       0.574         0.985         0.138     0.039  \n\n - Nordstrom         0.720       0.549         0.943         0.138     0.017  \n\n - Rozdzinski        0.686       0.517         0.911         0.144     0.009  \n\n - Ter Braak         0.683       0.518         0.901         0.141     0.007  \n\n - Tulkens           0.717       0.549         0.936         0.136     0.014  \n\n - Van der Auwera    0.728       0.557         0.952         0.137     0.020  \n\n - Klastersky        0.654       0.498         0.859         0.139     0.002  \n\n - Vanhaeverbeek     0.717       0.549         0.936         0.136     0.014  \n\n - Hollender         0.723       0.553         0.944         0.136     0.017  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371578645.56023'}, 'image_order': None},
                      })
    method_and_params.append({'meta_f_str': u'loo.ma.binary',
                              'method'    : 'binary.fixed.mh',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Leave-one-out Forest plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Binary Fixed-effect Model - Mantel Haenszel\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Overall             0.712       0.543         0.934         0.138     0.014  \n\n - Gonzalez          0.720       0.545         0.951         0.142     0.021  \n\n - Prins             0.727       0.551         0.959         0.141     0.024  \n\n - Giamarellou       0.742       0.564         0.975         0.140     0.032  \n\n - Maller            0.639       0.476         0.857         0.150     0.003  \n\n - Sturm             0.724       0.551         0.950         0.139     0.020  \n\n - Marik             0.874       0.637         1.199         0.161     0.403  \n\n - Muijsken          0.680       0.514         0.899         0.143     0.007  \n\n - Vigano            0.704       0.536         0.924         0.139     0.012  \n\n - Hansen            0.760       0.576         1.003         0.142     0.053  \n\n - De Vries          0.683       0.517         0.901         0.142     0.007  \n\n - Mauracher         0.748       0.568         0.984         0.140     0.038  \n\n - Nordstrom         0.713       0.542         0.939         0.140     0.016  \n\n - Rozdzinski        0.678       0.508         0.906         0.148     0.009  \n\n - Ter Braak         0.675       0.509         0.896         0.144     0.007  \n\n - Tulkens           0.712       0.543         0.934         0.138     0.014  \n\n - Van der Auwera    0.724       0.551         0.950         0.139     0.020  \n\n - Klastersky        0.643       0.486         0.851         0.143     0.002  \n\n - Vanhaeverbeek     0.712       0.543         0.934         0.138     0.014  \n\n - Hollender         0.718       0.547         0.942         0.139     0.017  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371578738.91666'}, 'image_order': None},
                      })
    method_and_params.append({'meta_f_str': u'loo.ma.binary',
                              'method'    : 'binary.fixed.inv.var',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
                              'results'   : {'images': {'Leave-one-out Forest plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Binary Fixed-effect Model - Inverse Variance\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Overall             0.760       0.571         1.011         0.146     0.059  \n\n - Gonzalez          0.772       0.576         1.037         0.150     0.085  \n\n - Prins             0.778       0.581         1.042         0.149     0.092  \n\n - Giamarellou       0.776       0.583         1.035         0.147     0.084  \n\n - Maller            0.683       0.501         0.931         0.158     0.016  \n\n - Sturm             0.769       0.577         1.025         0.147     0.073  \n\n - Marik             0.975       0.697         1.364         0.171     0.882  \n\n - Muijsken          0.725       0.540         0.974         0.150     0.033  \n\n - Vigano            0.752       0.564         1.001         0.146     0.051  \n\n - Hansen            0.802       0.600         1.073         0.148     0.137  \n\n - De Vries          0.730       0.545         0.977         0.149     0.034  \n\n - Mauracher         0.777       0.583         1.036         0.147     0.086  \n\n - Nordstrom         0.763       0.571         1.020         0.148     0.068  \n\n - Rozdzinski        0.726       0.534         0.987         0.157     0.041  \n\n - Ter Braak         0.721       0.535         0.972         0.152     0.032  \n\n - Tulkens           0.758       0.569         1.010         0.146     0.059  \n\n - Van der Auwera    0.769       0.577         1.025         0.147     0.073  \n\n - Klastersky        0.699       0.522         0.936         0.149     0.016  \n\n - Vanhaeverbeek     0.758       0.569         1.010         0.146     0.058  \n\n - Hollender         0.765       0.574         1.019         0.146     0.067  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371578803.22466'}, 'image_order': None},
                      })
    
    
    # for now just check that images and texts match
    for t in method_and_params:
        check_binary_meta_method_analysis.description = "Testing Binary Meta Method Analysis %s:%s" % (t['meta_f_str'], t['method'])
        yield check_binary_meta_method_analysis, t
    
####################################################
#    method_and_params.append({'meta_f_str': "",
#                              'method'    : "",
#                              'parameters': ,
#                              'results'   : ,
#                      })
####################################################
    

        
def check_binary_meta_analysis(test_data):
    test_result = meta_py_r.run_binary_ma(test_data['method'], test_data['parameters'])
    _results_match(test_result, test_data['results'], ['images','texts'])
    
def check_binary_meta_method_analysis(test_data):
    test_result = meta_py_r.run_meta_method(test_data['meta_f_str'], test_data['method'], test_data['parameters'])
    _results_match(test_result, test_data['results'], ['images','texts'])
    
def _results_match(test_res, ref_res, keys_to_compare): # test results and reference results
    for key in keys_to_compare:
        assert ((key in test_res) and (key in ref_res)), "key %s not found in test_result dictionary" % key
        val_ref, val_test = ref_res[key], test_res[key]
        
        equal_values = _compare_values(val_ref, val_test)
        
        assert equal_values, "test value: %s does not match reference value: %s for key: %s" % (val_test, val_ref, key)
        
        
################# CONTINUOUS META ANALYSIS TESTS #############################

def test_continuous_meta_analysis():
    fullpath = os.path.join(os.getcwd(),"../sample_data", "continuous.oma")
    meta.open(fullpath)
    meta_py_r.ma_dataset_to_simple_continuous_robj(meta.model)
    
    method_and_params = []
    method_and_params.append({
                              'meta_f_str': 'None',
                              'method': 'continuous.random',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': False, 'fp_xlabel': u'[default]', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': False, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'SMD', 'fp_show_summary_line': True},
                              'results': {'images': {'Forest Plot': './r_tmp/forest.png'}, 'texts': {'Weights': 'studies      weights\nCarroll 1997 15.8%\nGrant   1981 16.3%\nPeck    1987 12.6%\nDonat   2003 23.3%\nStewart 1990 13.8%\nYoung   1995 18.3%\n', 'Summary': 'Continuous Random-Effects Model\n\nMetric: Standardized Mean Difference\n\n Model Results\n\n Estimate  Lower bound   Upper bound   Std. error   p-Value  \n\n 0.358        0.152         0.565         0.105     < 0.001  \n\n\n Heterogeneity\n\n tau^2  Q(df=5)   Het. p-Value   I^2  \n\n 0.037   11.914       0.036      58%  \n\n\n'}, 'image_var_names': {'forest plot': 'forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371651937.75495'}, 'image_order': None},
                             })
    
    method_and_params.append({
                              'meta_f_str': 'None',
                              'method': 'continuous.fixed',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': False, 'fp_xlabel': u'[default]', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': False, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'SMD', 'fp_show_summary_line': True},
                              'results': {'images': {'Forest Plot': './r_tmp/forest.png'}, 'texts': {'Weights': 'studies      weights\nCarroll 1997 12.4%\nGrant   1981 13.3%\nPeck    1987  8.1%\nDonat   2003 39.2%\nStewart 1990  9.5%\nYoung   1995 17.5%\n', 'Summary': 'Continuous Fixed-Effect Model\n\nMetric: Standardized Mean Difference\n\n Model Results\n\n Estimate  Lower bound   Upper bound   Std. error   p-Value  \n\n 0.415        0.289         0.541         0.064     < 0.001  \n\n\n Heterogeneity\n\n Q(df=5)  Het. p-Value   I^2  \n\n 11.914       0.036      58%  \n\n\n'}, 'image_var_names': {'forest plot': 'forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371651942.7068'}, 'image_order': None},
                             })
    
    # for now just check that images and texts match
    for t in method_and_params:
        check_continuous_meta_analysis.description = "Testing Continuous Meta Analysis %s" % t['method']
        yield check_continuous_meta_analysis, t

def test_continuous_meta_analysis_meta_methods():
    fullpath = os.path.join(os.getcwd(),"../sample_data", "continuous.oma")
    meta.open(fullpath)
    meta_py_r.ma_dataset_to_simple_continuous_robj(meta.model)
    
    method_and_params = []
    
    # Cumulative meta analysis
    method_and_params.append({
                              'meta_f_str': 'cum.ma.continuous',
                              'method': 'continuous.random',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': False, 'fp_xlabel': u'[default]', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': False, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'SMD', 'fp_show_summary_line': True},
                              'results': {'images': {'Cumulative Forest Plot': './r_tmp/forest.png'}, 'texts': {'Cumulative Summary': 'Continuous Random-Effects Model\n\nMetric: Standardized Mean Difference\n\n Model Results\n\n Studies    Estimate   Lower bound   Upper bound   Std. error    p-Val   \n\n Carroll      0.095      -0.264         0.453         0.183        NA    \n\n + Grant      0.189      -0.059         0.438         0.127      0.136   \n\n + Peck       0.232       0.015         0.449         0.111      0.036   \n\n + Donat      0.376       0.091         0.660         0.145      0.010   \n\n + Stewart    0.397       0.169         0.626         0.116     < 0.001  \n\n + Young      0.358       0.152         0.565         0.105     < 0.001  \n\n\n'}, 'image_var_names': {'cumulative forest plot': 'cumulative forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371655521.74068'}, 'image_order': None},
                             })
    
    method_and_params.append({
                              'meta_f_str': 'cum.ma.continuous',
                              'method': 'continuous.fixed',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': False, 'fp_xlabel': u'[default]', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': False, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'SMD', 'fp_show_summary_line': True},
                              'results': {'images': {'Cumulative Forest Plot': './r_tmp/forest.png'}, 'texts': {'Cumulative Summary': 'Continuous Fixed-effect Model - Inverse Variance\n\nMetric: Standardized Mean Difference\n\n Model Results\n\n Studies    Estimate   Lower bound   Upper bound   Std. error    p-Val   \n\n Carroll      0.095      -0.264         0.453         0.183        NA    \n\n + Grant      0.189      -0.059         0.438         0.127      0.136   \n\n + Peck       0.232       0.015         0.449         0.111      0.036   \n\n + Donat      0.464       0.316         0.611         0.075     < 0.001  \n\n + Stewart    0.464       0.325         0.602         0.071     < 0.001  \n\n + Young      0.415       0.289         0.541         0.064     < 0.001  \n\n\n'}, 'image_var_names': {'cumulative forest plot': 'cumulative forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371655532.42571'}, 'image_order': None},
                             })
    
    # Leave one out meta analysis
    method_and_params.append({
                              'meta_f_str': 'loo.ma.continuous',
                              'method': 'continuous.random',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': False, 'fp_xlabel': u'[default]', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': False, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'SMD', 'fp_show_summary_line': True},
                              'results': {'images': {'Leave-one-out Forest Plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Continuous Random-Effects Model\n\nMetric: Standardized Mean Difference\n\n Model Results\n\n Studies    Estimate   Lower bound   Upper bound   Std. error    p-Val   \n\n Overall      0.358       0.152         0.565         0.105     < 0.001  \n\n - Carroll    0.411       0.202         0.621         0.107     < 0.001  \n\n - Grant      0.370       0.126         0.615         0.125      0.003   \n\n - Peck       0.353       0.113         0.593         0.122      0.004   \n\n - Donat      0.254       0.093         0.416         0.082      0.002   \n\n - Stewart    0.337       0.094         0.580         0.124      0.007   \n\n - Young      0.397       0.169         0.626         0.116     < 0.001  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371655544.35331'}, 'image_order': None},
                             })
    
    method_and_params.append({
                              'meta_f_str': 'loo.ma.continuous',
                              'method': 'continuous.fixed',
                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': False, 'fp_xlabel': u'[default]', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': False, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'SMD', 'fp_show_summary_line': True},
                              'results': {'images': {'Leave-one-out Forest Plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Continuous Fixed-effect Model - Inverse Variance\n\nMetric: Standardized Mean Difference\n\n Model Results\n\n Studies    Estimate   Lower bound   Upper bound   Std. error    p-Val   \n\n Overall      0.415       0.289         0.541         0.064     < 0.001  \n\n - Carroll    0.460       0.326         0.595         0.069     < 0.001  \n\n - Grant      0.436       0.301         0.571         0.069     < 0.001  \n\n - Peck       0.419       0.288         0.551         0.067     < 0.001  \n\n - Donat      0.254       0.093         0.416         0.082      0.002   \n\n - Stewart    0.410       0.278         0.543         0.068     < 0.001  \n\n - Young      0.464       0.325         0.602         0.071     < 0.001  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371655554.24533'}, 'image_order': None},
                             })
    
    # for now just check that images and texts match
    for t in method_and_params:
        check_continuous_meta_method_analysis.description = "Testing Continuous Meta Method Analysis %s:%s" % (t['meta_f_str'], t['method'])
        yield check_continuous_meta_method_analysis, t


def check_continuous_meta_analysis(test_data):
    test_result = meta_py_r.run_continuous_ma(test_data['method'], test_data['parameters'])
    _results_match(test_result, test_data['results'], ['images','texts'])

# same as check_binary_meta_method_analysis--> should be refactored together
def check_continuous_meta_method_analysis(test_data):
    test_result = meta_py_r.run_meta_method(test_data['meta_f_str'], test_data['method'], test_data['parameters'])
    _results_match(test_result, test_data['results'], ['images','texts'])





        
####### Don't delete this. Its good to use as a template ####
#def setup_module(module):
#    print ("") # this is to get a newline after the dots
#    print ("setup_module before anything in this file")
#
#def teardown_module(module):
#    print ("teardown_module after everything in this file")
#
#def my_setup_function():
#    print ("my_setup_function")
#
#def my_teardown_function():
#    print ("my_teardown_function")
#
#@with_setup(my_setup_function, my_teardown_function)
#def test_numbers_3_4():
#    print 'test_numbers_3_4  <============================ actual test code'
#    assert multiply(3,4) == 12
#
#@with_setup(my_setup_function, my_teardown_function)
#def test_strings_a_3():
#    print 'test_strings_a_3  <============================ actual test code'
#    assert multiply('a',3) == 'aaa'
    



