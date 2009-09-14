from PyQt4.Qt import *
from PyQt4 import QtGui

import ui_binary_data_form
from ui_binary_data_form import Ui_BinaryDataForm

class BinaryDataForm2(QDialog, ui_binary_data_form.Ui_BinaryDataForm):
    
    def __init__(self, parent=None):
        #QtGui.QFormLayout.addLayout = add_layout_fix
        super(BinaryDataForm2, self).__init__(parent)
        self.setupUi(self)
        
    

