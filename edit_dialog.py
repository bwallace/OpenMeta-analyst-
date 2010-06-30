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
        self._setup_connections()
        self.dataset = dataset
        self.selected_group = None

    def _setup_connections(self):
        QObject.connect(self.add_group_btn, SIGNAL("pressed()"),
                                    self.add_group)
        QObject.connect(self.remove_group_btn, SIGNAL("pressed()"),
                                    self.remove_group)
        QObject.connect(self.group_list, SIGNAL("clicked(QModelIndex)"),
                                    self.group_selected)

                                    
    def add_group(self):
        print "hello?"
        
    def remove_group(self):
        self.group_list.model().dataset.delete_group(self.selected_group)
        self.group_list.model().reset()
        
    def group_selected(self, index):
        self.selected_group = self.group_list.model().group_list[index.row()]
        self.remove_group_btn.setEnabled(True)