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

# we're going to need to pass in the data type here -- perhaps the
# datatable model?
class MA_Specs(QDialog, ui_ma_specs.Ui_Dialog):
    
    def __init__(self, parent=None):
        super(MA_Specs, self).__init__(parent)
        self.setupUi(self)
        self.populate_cbo_box()
    
    def populate_cbo_box(self):
        available_methods = meta_py_r.get_available_methods("binary")
        print "\n\navailable binary methods: %s" % ", ".join(available_methods)
        for method in available_methods:
            self.method_cbo_box.addItem(method)