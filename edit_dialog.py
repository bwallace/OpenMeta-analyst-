##################################
#                                           
#  Byron C. Wallace                   
#  Tufts Medical Center               
#  OpenMeta[analyst]                  
#                                     
#  This form is for 'batch' editing a dataset. Note that any 
#  edits apply to *all* MetaAnalyticUnit objects known. So
#  e.g., if a group name is changed, it will be changed 
# *everywhere*.
#
#  Note also that this form doesn't itself provide any 
#  undo/redo functionality. Rather, the strategy is to
#  treat *all* editing done via this form as one
#  undoable action.
#                                     
##################################


from PyQt4.Qt import *
import ui_edit_dialog
import edit_list_models
import meta_py_r
import add_new_dialogs
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
        form = add_new_dialogs.AddNewGroupForm(self)
        form.group_name_le.setFocus()        
        if form.exec_():
            new_group_name = unicode(form.group_name_le.text().toUtf8(), "utf-8")
            self.group_list.model().dataset.add_group(new_group_name)
            self.group_list.model().refresh_group_list()
            
    def remove_group(self):
        self.group_list.model().dataset.delete_group(self.selected_group)
        self.group_list.model().reset()
        
    def group_selected(self, index):
        self.selected_group = self.group_list.model().group_list[index.row()]
        self.remove_group_btn.setEnabled(True)