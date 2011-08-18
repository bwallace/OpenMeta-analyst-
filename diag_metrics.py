from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
import pdb


import ui_diagnostic_metrics
import ma_specs
import meta_globals
from meta_globals import *

class Diag_Metrics(QDialog, ui_diagnostic_metrics.Ui_diag_metric):

    SELECTABLE_METRICS = ["sens", "spec", "dor", "lr"]

    def __init__(self, model, parent=None, meta_f_str=None, external_params=None):
        super(Diag_Metrics, self).__init__(parent)
        self.setupUi(self)
        self.model = model
        self.parent = parent
        QObject.connect(self.btn_ok, SIGNAL("pressed()"), self.ok)

    def ok(self):
        form =  ma_specs.MA_Specs(self.model, parent=self.parent,\
                     diag_metrics=self.get_selected_metrics())
        form.show()
        self.hide()

    def get_selected_metrics(self):
        selected_metrics = []
        # just loop through all the check
        # boxes on the form and see if they're checked. 

        for metric in self.SELECTABLE_METRICS:
            if eval("self.chk_box_%s.isChecked()" % metric):
                print metric
                selected_metrics.append(metric)

  
        return selected_metrics


