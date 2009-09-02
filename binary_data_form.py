from PyQt4.Qt import *
from PyQt4 import QtGui

import ui_binary_data_form
from ui_binary_data_form import Ui_BinaryDataForm

def add_layout_fix(self, layout, row, column, rowSpan, columnSpan, alignment=0):
    if column == 0:
        role = QtGui.QFormLayout.LabelRole
    else:
        role = QtGui.QFormLayout.FieldRole
    if rowSpan != 1 or columnSpan != 1:
        raise ValueError('rowSpan and columnSpan must be 1')
    self.setLayout(row, role, layout)
    
QtGui.QFormLayout.addLayout = add_layout_fix
print "ok."
 
class BinaryDataForm2(QDialog, ui_binary_data_form.Ui_BinaryDataForm):
    
    def __init__(self, parent=None):
        QtGui.QFormLayout.addLayout = add_layout_fix
        super(BinaryDataForm2, self).__init__(parent)
        self.setupUi(self)
        

