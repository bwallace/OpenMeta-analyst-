from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
import pdb


import ui_diagnostic_metrics
import ma_specs
import meta_globals
from meta_globals import *

class Diag_Metrics(QDialog, ui_diagnostic_metrics.Ui_diag_metric):

    def __init__(self, model, parent=None, meta_f_str=None, external_params=None):
        super(Diag_Metrics, self).__init__(parent)
        self.setupUi(self)
        self.model = model

        QObject.connect(self.btn_ok, SIGNAL("pressed()"), self.ok)

    def ok(self):
        form =  ma_specs.MA_Specs(self.model, parent=self, diag_metrics=self.get_selected_metrics())
        form.show()

    def get_selected_metrics(self):
        selected_metrics = []
        # just loop through all the check
        # boxes on the form and see if they're checked. 
        for metric in DIAGNOSTIC_METRICS:
            if eval("self.chk_box_%s.isChecked()" % metric.lower()):
                selected_metrics.append(metric)

        pyqtRemoveInputHook()
        pdb.set_trace()
        return selected_metrics


