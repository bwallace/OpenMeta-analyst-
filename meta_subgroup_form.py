from PyQt4.Qt import *
import pdb

import ui_cov_subgroup_dlg
import meta_py_r
import ma_specs

class MetaSubgroupForm(QDialog, ui_cov_subgroup_dlg.Ui_cov_subgroup_dialog):
    
    def __init__(self, model, parent=None):
        super(MetaSubgroupForm, self).__init__(parent)
        self.model = model
        self.setupUi(self)
        self._populate_combo_box()
        QObject.connect(self.buttonBox, SIGNAL("rejected()"), self.cancel)
        QObject.connect(self.buttonBox, SIGNAL("accepted()"), self.get_selected_cov)
        
    def cancel(self):
        print "(cancel)"
        self.reject()
        
    def get_selected_cov(self):
        selected_cov = unicode(self.cov_subgroup_cbo_box.currentText().toUtf8(), "utf-8") 
        self.parent().meta_subgroup(selected_cov)
        self.accept()
        
    def _populate_combo_box(self):
        studies = self.model.get_studies(only_if_included=True)
        
        for cov in self.model.dataset.covariates:
            cov_vals = [study.covariate_dict[cov.name] for study in studies]
            if not None in cov_vals:
                self.cov_subgroup_cbo_box.addItem(cov.name)

        