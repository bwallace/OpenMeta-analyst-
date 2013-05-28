from PyQt4.Qt import *
import pdb
import forms.ui_diagnostic_explain_dlg

class DiagnosticExplain(QDialog, forms.ui_diagnostic_explain_dlg.Ui_diag_explain_window):
    
    def __init__(self, parent=None):
        super(DiagnosticExplain, self).__init__(parent)
        self.setupUi(self)

        QObject.connect(self.dont_show_again_chk_box,  SIGNAL("stateChanged(int)"),
                        lambda: self.parent().parent().update_user_prefs("explain_diag", \
                                    not self.dont_show_again_chk_box.isChecked()))
