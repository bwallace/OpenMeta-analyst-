#######################################
#                                                                                   #
#                           Byron C. Wallace                              #
#  Tufts Medical Center                                                 #
#  OpenMeta[analyst]                                                    #
#                                                                                   #
#  This is the code for the ui dialog that handles          #
#  the method selection and algorithm specifications  #
#                                                                                   #
#######################################


from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *

import ui_ma_specs
import meta_py_r

'''
Note that we can do this! See: http://bcbio.wordpress.com/

hmmm note the go_data object which is created and then used directly...

08	params = {"ontology" : go_term_type,
09	          "annot" : robjects.r["annFUN.gene2GO"],
10	          "geneSelectionFun" : robjects.r["topDiffGenes"],
11	          "allGenes" : _dict_to_namedvector(gene_vals),
12	          "gene2GO" : _dict_to_namedvector(gene_to_go)
13	          }
14	go_data = robjects.r.new("topGOdata", **params)
15	results = robjects.r.runTest(go_data, algorithm=topgo_method,
16	        statistic="fisher")
17	scores = robjects.r.score(results)
'''
# we're going to need to pass in the data type here -- perhaps the
# datatable model?
class MA_Specs(QDialog, ui_ma_specs.Ui_Dialog):
    
    def __init__(self, parent=None):
        super(MA_Specs, self).__init__(parent)
        self.setupUi(self)
        self.data_type = "binary"
        self.current_method = None
        self.current_params = None
        self.populate_cbo_box()
        self.ui_for_params()
    
    def populate_cbo_box(self):
        available_methods = meta_py_r.get_available_methods(self.data_type)
        print "\n\navailable binary methods: %s" % ", ".join(available_methods)
        for method in available_methods:
            self.method_cbo_box.addItem(method)
        self.current_method = self.method_cbo_box.currentText()
        self.setup_params()
        self.parameter_grp_box.setTitle(self.current_method)
        

    def ui_for_params(self):
        layout = QGridLayout()
        
        lbl = QLabel(self.current_params.keys()[0], self.parameter_grp_box)
        layout.addWidget(lbl)
        lbl2 = QLabel(self.current_params.keys()[1], self.parameter_grp_box)
        layout.addWidget(lbl2)
        self.parameter_grp_box.setLayout(layout)
        
        
    def setup_params(self):
        self.current_params = meta_py_r.get_params(self.current_method)
        