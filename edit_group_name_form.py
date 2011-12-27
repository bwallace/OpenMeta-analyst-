from PyQt4.Qt import *
import ui_edit_group_name

class EditGroupName(QDialog, ui_edit_group_name.Ui_group_name_dialog):
    
    def __init__(self, cur_group_name, parent=None):
        super(EditGroupName, self).__init__(parent)
        self.setupUi(self)
        self.group_name_le.setText(cur_group_name)
        
    