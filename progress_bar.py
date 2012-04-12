''''''''''''''''''''''''''''''
' admittedly, kind of silly. '
''''''''''''''''''''''''''''''

from PyQt4.Qt import *
import pdb
import ui_running


class MetaProgress(QDialog, ui_running.Ui_running):
    
    def __init__(self, parent=None):
        super(MetaProgress, self).__init__(parent)
        self.setupUi(self)