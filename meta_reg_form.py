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
        studies = self.model.get_studies(only_if_included=True)
        
        for cov in self.model.dataset.covariates:
            cov_vals = [study.covariate_dict[cov.name] for study in studies]
            if not None in cov_vals:
                self.cov_cbo_box.addItem(cov.name)

        