from PyQt4.Qt import *
import pdb

import ui_meta_reg

class MetaRegForm(QDialog, ui_meta_reg.Ui_cov_reg_dialog):
    
    def __init__(self, model, parent=None):
        super(MetaRegForm, self).__init__(parent)
        self.model = model
        self.setupUi(self)
        self._populate_combo_box()
        
        
    def _populate_combo_box(self):
        pyqtRemoveInputHook()
        pdb.set_trace()
        pass