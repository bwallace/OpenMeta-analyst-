from PyQt4.Qt import *
import ui_edit_dialog
import edit_list_models
import meta_py_r
import pdb

class EditDialog(QDialog, ui_edit_dialog.Ui_edit_dialog):

    def __init__(self, dataset, parent=None):
        super(EditDialog, self).__init__(parent)
        self.setupUi(self)
        self.groups_model = edit_list_models.TXGroupModel(dataset = dataset)
        self.group_list.setModel(self.groups_model)
        
        self.dataset = dataset



        