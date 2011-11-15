from PyQt4.Qt import *
import pdb

import ui_meta_reg
import meta_py_r

class MetaRegForm(QDialog, ui_meta_reg.Ui_cov_reg_dialog):
    
    def __init__(self, model, parent=None):
        super(MetaRegForm, self).__init__(parent)
        self.model = model
        self.setupUi(self)
        self.covs_and_check_boxes = None
        self._populate_chk_boxes()
        
        QObject.connect(self.buttonBox, SIGNAL("rejected()"), self.cancel)
        QObject.connect(self.buttonBox, SIGNAL("accepted()"), self.run_meta_reg)
        
    def cancel(self):
        print "(cancel)"
        self.reject()
        
    def run_meta_reg(self):
        #selected_cov = self.cov_cbo_box.currentText()
        #print selected_cov

        meta_py_r.ma_dataset_to_simple_binary_robj(self.model, include_raw_data=False)
        selected_covariates = []
        for cov, chk_box in self.covs_and_check_boxes:
            if chk_box.isChecked():
                selected_covariates.append(cov)

            # here we have to exclud studies that do not have values
            # for all of the selected covariates
            cov_d = self.model.dataset.get_values_for_cov(cov.name)

        studies = []
        for study in [study.name for study in self.model.dataset.studies]:
            if study != '' and cov_d[study] is not None and cov_d[study] != '':
                studies.append(study)

        #result = meta_py_r.run_binary_fixed_meta_regression(selected_cov)
        result = meta_py_r.run_meta_regression(self.model.dataset, studies,\
                                                 selected_covariates)
        self.parent().analysis(result)
        self.accept()
        
    def _populate_combo_box(self):
        studies = self.model.get_studies(only_if_included=True)
        
        for cov in self.model.dataset.covariates:
            cov_vals = [study.covariate_dict[cov.name] for study in studies]
            if not None in cov_vals:
                self.cov_cbo_box.addItem(cov.name)

    def _populate_chk_boxes(self):
        self.covs_and_check_boxes = []
        studies = self.model.get_studies(only_if_included=True)
        
        chk_box_layout = QGridLayout()
        for cov in self.model.dataset.covariates:
            cov_vals = [study.covariate_dict[cov.name] for study in studies]
            if not None in cov_vals:
                chk_box = QCheckBox(cov.name)
                chk_box_layout.addWidget(chk_box)
                self.covs_and_check_boxes.append((cov, chk_box))
                
            self.cov_grp_box.setLayout(chk_box_layout)
            