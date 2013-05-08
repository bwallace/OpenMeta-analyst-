''''''''''''''''''''''''''''''
' admittedly, kind of silly. '
''''''''''''''''''''''''''''''

from PyQt4.Qt import *
import pdb
import forms.ui_running


class MetaProgress(QDialog, forms.ui_running.Ui_running):
    
    def __init__(self, parent=None):
        super(MetaProgress, self).__init__(parent)
        self.setupUi(self)