import pdb
import string
import math

from PyQt4.Qt import *
from PyQt4 import QtGui

import meta_py_r
from meta_globals import *
import ui_change_cov_type
from ui_change_cov_type import Ui_ChangeCovTypeForm
from ma_dataset import Covariate

class ChangeCovTypeForm(QDialog, Ui_ChangeCovTypeForm):
    def __init__(self, dataset, cov, parent=None):
        super(ChangeCovTypeForm, self).__init__(parent)
        self.setupUi(self)
        self.dataset = dataset
        self.cov_model = CovModel(dataset, cov)
        self.cov_prev_table.setModel(self.cov_model)
        self.cov_prev_table.resizeColumnsToContents()


class CovModel(QAbstractTableModel):
    '''
    This module mediates between the dataset class and 
    the TableView used in the ui.
    '''
    def __init__(self, dataset, covariate, filename=QString()):
        super(CovModel, self).__init__()
        self.dataset = dataset
        studies = self.dataset.studies

        self.covariate = covariate
        
        # now we add a covariate with the new type
        self.new_data_type = CONTINUOUS if covariate.data_type==FACTOR else FACTOR

        # first sort the studies by the cov. of interest
        self.dataset.studies.sort(\
            cmp=self.dataset.cmp_studies(compare_by=self.covariate.name))
        
        self.update_included_studies()
        self.add_cov_with_new_type()

        self.refresh_cov_values()
        
        self.STUDY_COL, self.ORIG_VAL, self.NEW_VAL = range(3)

    def add_cov_with_new_type(self):
        new_name = self.covariate.name
        if self.new_data_type == CONTINUOUS:
            new_name += " (continuous)"
        else:
            new_name += " (factor)"

        guessed_vals = self.guess_at_values() # try and infer sensible values
        self.new_covariate = \
            Covariate(new_name, COV_INTS_TO_STRS[self.new_data_type])

        self.dataset.add_covariate(self.new_covariate, cov_values=guessed_vals)
        self.reset()
       
    def guess_at_values(self):
        cov_d = self.dataset.get_values_for_cov(self.covariate) # original values
        guessed_vals_d = self.vals_to_new_vals(cov_d)

        studies_to_guessed_vals = {}
        for study in self.included_studies:
            if cov_d.has_key(study.name):
                orig_val = cov_d[study.name]
                studies_to_guessed_vals[study.name] = guessed_vals_d[orig_val]
            else:
                studies_to_guessed_vals[study.name] = None

        return studies_to_guessed_vals


    def vals_to_new_vals(self, cov_d):
        unique_values = list(set(cov_d.values()))
        # fix for issue #155
        unique_values.sort()
        mapping = {}
        for i,val in enumerate(unique_values):
            if self.new_data_type == FACTOR:
                mapping[val] = self._to_alphabet_str(i)
            else:
                mapping[val] = i
        
        print mapping
        return mapping

    def _is_a_num(self, x):
        try:
            y = float(x)
            return y
        except:
            # nope.
            return False

    def _to_alphabet_str(self, x):
        # base conversion.
        alphabet = string.ascii_lowercase
        alpha_str = ""
        x_left = x 
        while x_left >= 0:
            if x_left > 25:
                alpha_str += "a"
                x_left -= 26
            else:
                alpha_str += alphabet[x_left]
                x_left = -1

        return alpha_str 

    def refresh_cov_values(self):
        self.dataset.studies.sort(\
            cmp=self.dataset.cmp_studies(compare_by=self.covariate.name))
        
        self.update_included_studies()
        cov_d = self.dataset.get_values_for_cov(self.covariate)
        new_cov_d = self.dataset.get_values_for_cov(self.new_covariate)

        self.orig_cov_list, self.new_cov_list = [], []
        for study in self.included_studies:
            if cov_d.has_key(study.name):
                self.orig_cov_list.append(cov_d[study.name])            
                self.new_cov_list.append(new_cov_d[study.name])
            else:
                self.orig_cov_list.append(None)
                self.new_cov_list.append(None)
        self.orig_cov_list.append("")

        self.reset()
        
    def update_included_studies(self):
        study_list = []
        for study in self.dataset.studies:
            if study.include:
                study_list.append(study)
        self.included_studies = study_list

    def data(self, index, role=Qt.DisplayRole):
        
        if not index.isValid() or not (0 <= index.row() < len(self.included_studies)):
            return QVariant()

        orig_cov_val = self.orig_cov_list[index.row()]
        if role == Qt.DisplayRole:
            row, column = index.row(), index.column()
            if column == self.STUDY_COL:
                return QVariant(self.included_studies[row].name)
            elif column == self.ORIG_VAL:
                return QVariant(self.orig_cov_list[row])
            elif column == self.NEW_VAL:
                return QVariant(self.new_cov_list[row])
        elif role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        return QVariant()
   


    def rowCount(self, index=QModelIndex()):
        return len(self.included_studies) # don't show blank study!
        
    def columnCount(self, index=QModelIndex()):
        return 3 # study, orig_val, new_val
        
    def setData(self, index, value, role=Qt.EditRole):
        # don't allow users to mess with the original
        # covariate.
        if index.isValid() and 0 <= index.row() < len(self.dataset):
            column = index.column()

            if column == self.NEW_VAL:
                # then a (new) covariate value has been edited.
                #pyqtRemoveInputHook()
                #pdb.set_trace()
                study = self.included_studies[index.row()] # associated study
                cov_name = self.new_covariate.name
                new_value = None
                if self.new_covariate.data_type == FACTOR:
                    new_value = value.toString()
                else:
                    # continuous
                    new_value, converted_ok = value.toDouble()
                    if not converted_ok: 
                        print "whoops! can't convert %s to a number." % value
                        new_value = None
                study.covariate_dict[cov_name] = new_value
                self.refresh_cov_values()
                return True
        return False
        
    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)


    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        if role != Qt.DisplayRole:
            return QVariant()
        if orientation == Qt.Horizontal:
            if section == self.STUDY_COL:
                return QVariant("study")
            elif section == self.ORIG_VAL:
                return QVariant(self.covariate.name)
            elif section == self.NEW_VAL:
                return QVariant(self.new_covariate.name)