from PyQt4.Qt import *
import ui_start_up

class StartUp(QDialog, ui_start_up.Ui_WelcomeDialog):
    
    def __init__(self, parent=None):

        super(StartUp, self).__init__(parent)
        self.setupUi(self)
        