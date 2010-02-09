from PyQt4.Qt import *
import ui_edit_dialog

class EditDialog(QDialog, ui_edit_dialog.Ui_edit_dialog):

    def __init__(self, results, parent=None):
        super(EditDialog, self).__init__(parent)
        self.setupUi(self)