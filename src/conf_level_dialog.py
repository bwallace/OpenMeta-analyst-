'''
Created on Apr 29, 2013

@author: George Dietz
'''

from PyQt4.QtCore import *
from PyQt4.QtGui import *

from meta_globals import DEFAULT_CONF_LEVEL

class ChangeConfLevelDlg(QDialog):
    ''' Dialog for changing confidence level '''
    
    def __init__(self, previous_value=DEFAULT_CONF_LEVEL, parent=None):
        super(ChangeConfLevelDlg, self).__init__(parent)
        
        cl_label = QLabel("Global Confidence Level:")
        
        self.conf_level_spinbox = QDoubleSpinBox()
        self.conf_level_spinbox.setRange(50, 99.999 )
        self.conf_level_spinbox.setSingleStep(0.1)
        self.conf_level_spinbox.setSuffix(QString("%"))
        self.conf_level_spinbox.setValue(previous_value)
        self.conf_level_spinbox.setDecimals(1)
        
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        
        hlayout = QHBoxLayout()
        hlayout.addWidget(cl_label)
        hlayout.addWidget(self.conf_level_spinbox)
        vlayout = QVBoxLayout()
        vlayout.addLayout(hlayout)
        vlayout.addWidget(buttonBox)
        self.setLayout(vlayout)
        
        self.connect(buttonBox, SIGNAL("accepted()"), self, SLOT("accept()"))
        self.connect(buttonBox, SIGNAL("rejected()"), self, SLOT("reject()"))
        self.setWindowTitle("Change Confidence Level")
        
    def get_value(self):
        return self.conf_level_spinbox.value()