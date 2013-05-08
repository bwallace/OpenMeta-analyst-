from PyQt4.Qt import *
import forms.ui_new_outcome

class AddNewOutcomeForm(QDialog, forms.ui_new_outcome.Ui_Dialog):
    
    def __init__(self, parent=None):
        super(AddNewOutcomeForm, self).__init__(parent)
        self.setupUi(self)
        self._populate_combo_box()

        
    def _populate_combo_box(self):
        for name, item_id in zip([QString(s) for s in ["Binary", "Continuous", "Diagnostic", "Other"]],
                                     [QVariant(i) for i in range(4)]):
            self.datatype_cbo_box.addItem(name, item_id)
        