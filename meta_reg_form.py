from PyQt4.Qt import *
import pdb

import ui_meta_reg
import meta_py_r

class MetaRegForm(QDialog, ui_meta_reg.Ui_cov_reg_dialog):
    
    def __init__(self, model, parent=None):
        super(MetaRegForm, self).__init__(parent)
        self.model = model
        self.setupUi(self)
        self._populate_combo_box()
        QObject.connect(self.buttonBox, SIGNAL("rejected()"), self.cancel)
        QObject.connect(self.buttonBox, SIGNAL("accepted()"), self.run_meta_reg)
        
    def cancel(self):
        print "(cancel)"
        self.reject()
        
    def run_meta_reg(self):
        selected_cov = self.cov_cbo_box.currentText()
        print selected_cov
        meta_py_r.ma_dataset_to_simple_binary_robj(self.model)
        result = meta_py_r.run_binary_meta_regression(selected_cov)
        self.parent().analysis(result)
        self.accept()
        
    def _populate_combo_box(self):
        studies = self.model.get_studies(only_if_included=True)
        
        for cov in self.model.dataset.covariates:
            cov_vals = [study.covariate_dict[cov.name] for study in studies]
            if not None in cov_vals:
                self.cov_cbo_box.addItem(cov.name)

        