from PyQt4.Qt import *
import ui_new_group

# TODO should probalby just merge into a single module along with
# the AddNewOutcome dialog...
class AddNewGroupForm(QDialog, ui_new_group.Ui_new_group_dialog):
    
    def __init__(self, parent=None):
        super(AddNewGroupForm, self).__init__(parent)
        self.setupUi(self)