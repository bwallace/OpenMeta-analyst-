from PyQt4.Qt import *
import forms.ui_diagnostic_explain_dlg

from settings import update_setting

class DiagnosticExplain(QDialog, forms.ui_diagnostic_explain_dlg.Ui_diag_explain_window):
    
    def __init__(self, parent=None):
        super(DiagnosticExplain, self).__init__(parent)
        self.setupUi(self)

        QObject.connect(self.dont_show_again_chk_box, SIGNAL("stateChanged(int)"), self.update_explain_diag_setting)

    def update_explain_diag_setting(self, state):
        field = "explain_diag"
        
        if state == Qt.Checked:
            update_setting(field, True)
        elif state == Qt.Unchecked:
            update_setting(field, False)