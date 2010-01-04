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
    
    def __init__(self, model, parent=None):
        super(MA_Specs, self).__init__(parent)
        self.setupUi(self)
        self.model = model
        
        QObject.connect(self.buttonBox, SIGNAL("accepted()"), self.run_ma)
        QObject.connect(self.buttonBox, SIGNAL("rejected()"), self.cancel)
        
        self.data_type = self.model.get_current_outcome_type()
        print "data type: %s" % self.data_type
        self.current_method = None
        self.current_params = None
        self.current_defaults = None
        self.current_param_vals = {}
        self.populate_cbo_box()
        self.ui_for_params()
        
    
    def cancel(self):
        print "cancel"
        self.reject()
        
    def run_ma(self):
        # note that this call creates a tmp object in R called
        # tmp_obj
        meta_py_r.ma_dataset_to_simple_binary_robj(self.model)
        result = meta_py_r.run_binary_ma(self.current_param_vals)
        #print result
        self.parent().analysis(result)
        self.accept()
        
    def populate_cbo_box(self):
        available_methods = meta_py_r.get_available_methods(self.data_type)
        print "\n\navailable %s methods: %s" % (self.data_type, ", ".join(available_methods))
        for method in available_methods:
            self.method_cbo_box.addItem(method)
        self.current_method = self.method_cbo_box.currentText()
        self.setup_params()
        self.parameter_grp_box.setTitle(self.current_method)
        

    def ui_for_params(self):
        layout = QGridLayout()
        cur_grid_row = 0
        
        # we want to add the parameters in groups, for example,
        # we add combo boxes (which will be lists of values) together, 
        # followed by numerical inputs. thus we create an ordered list
        # of functions to check if the argument is the corresponding 
        # type (float, list); if it is, we add it otherwise we pass. this isn't
        # the most efficient way to do things, but the number of parameters
        # is going to be relatively tiny anyway
        ordered_types = [lambda x: isinstance(x, list), \
                                    lambda x: isinstance(x, str) and x.lower()=="float"]
                                    
        for is_right_type in ordered_types:
            for key, val in self.current_params.items():
                if is_right_type(val):
                    self.add_param(layout, cur_grid_row, key, val)
                    cur_grid_row+=1
        
        self.parameter_grp_box.setLayout(layout)
        
    def add_param(self, layout, cur_grid_row, name, value):
        print "adding param. name: %s, value: %s" % (name, value)
        if isinstance(value, list):
            # then it's an enumeration of values
            self.add_enum(layout, cur_grid_row, name, value)
        elif value.lower() == "float":
            self.add_float_box(layout, cur_grid_row, name)
        else:
            print "uknown type! throwing up. bleccch."
            print "name:%s. value: %s" % (name, value)
            # throw exception here

    def add_enum(self, layout, cur_grid_row, name, values):
        self.add_label(layout, cur_grid_row, name)
        cbo_box = QComboBox()
        for value in values:
            cbo_box.addItem(value)
        
        if self.current_defaults.has_key(name):
            cbo_box.setCurrentIndex(cbo_box.findText(self.current_defaults[name]))
            self.current_param_vals[name] = self.current_defaults[name]
            
        layout.addWidget(cbo_box, cur_grid_row, 1)
    
    def add_float_box(self, layout, cur_grid_row, name):
        self.add_label(layout, cur_grid_row, name)
        # now add the float input line edit
        finput = QLineEdit()
        
        # if a default value has been specified, use it
        if self.current_defaults.has_key(name):
            finput.setText(str(self.current_defaults[name]))
            self.current_param_vals[name] = self.current_defaults[name]
        
        finput.setMaximumWidth(50)
        layout.addWidget(finput, cur_grid_row, 1)
    
    def add_label(self, layout, cur_grid_row, name):
        lbl = QLabel(name, self.parameter_grp_box)
        layout.addWidget(lbl, cur_grid_row, 0)
       
    def setup_params(self):
        self.current_params, self.current_defaults = meta_py_r.get_params(self.current_method)
        #for param in self.current_params.keys():
        #    if self.current_defaults.has_key(param):
                
        print self.current_defaults
        