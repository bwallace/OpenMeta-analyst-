##################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Diagnostic data form module; for flexible entry of diagnostic
#  outcome data.
#
##################################################


import pdb

from PyQt4.Qt import *
from PyQt4 import QtGui

import meta_py_r
from meta_globals import *
import ui_continuous_data_form
from ui_diagnostic_data_form import Ui_DiagnosticDataForm

class DiagnosticDataForm(QDialog, Ui_DiagnosticDataForm):
    def __init__(self, ma_unit, cur_txs, cur_group_str, parent=None):
        super(DiagnosticDataForm, self).__init__(parent)
        self.setupUi(self)
        self.setup_signals_and_slots()
        self.ma_unit = ma_unit
        self.raw_data_dict = {}
        for group in cur_txs:
            raw_data = self.ma_unit.get_raw_data_for_group(group)
            self.raw_data_dict[group] = raw_data
        self.cur_groups = cur_txs
        self.cur_effect = "Sens" # arbitrary
        self.group_str = cur_group_str
        self.alpha = .05
        
        entry_widgets = [self.two_by_two_table, self.alpha_edit,\
            self.low_txt_box, self.high_txt_box, self.effect_txt_box]
        
        # block all the widgets for a moment
        for widget in entry_widgets:
            widget.blockSignals(True)
        self._update_raw_data()
        self._populate_effect_data()
        self._update_data_table()
        self.set_current_effect()

        # unblock
        for widget in entry_widgets:
            widget.blockSignals(False)
    
    def setup_signals_and_slots(self):
        QObject.connect(self.two_by_two_table, SIGNAL("cellChanged (int, int)"), 
                                            self.value_changed)
        QObject.connect(self.alpha_edit, SIGNAL("textChanged (QString)"), 
                                            self.update_alpha)                            
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                             self.effect_changed) 
                                                                                
        QObject.connect(self.effect_txt_box, SIGNAL("textChanged(QString)"), lambda new_text : self.val_edit("est", new_text))
        QObject.connect(self.low_txt_box, SIGNAL("textChanged(QString)"), lambda new_text : self.val_edit("lower", new_text))
        QObject.connect(self.high_txt_box, SIGNAL("textChanged(QString)"), lambda new_text : self.val_edit("upper", new_text))



    def _get_int(self, i, j):
        if not self._is_empty(i,j):
            int_val = int(float(self.two_by_two_table.item(i, j).text()))
            return int_val

    def _is_empty(self, i, j):
        val = self.two_by_two_table.item(i,j)
        return val is None or val.text() == ""


    def _is_txt_box_empty(self, txt_box):
        val = txt_box.text()
        val is None or val == ""

    def value_changed(self, i, j):
        new_val = self._get_int(i, j)
        if new_val is not None:
            self.impute_data()

    
    def impute_data(self):
        diag_data_dict = self.build_dict()

        if diag_data_dict is not None:
            imputed = meta_py_r.impute_diag_data(diag_data_dict, self.cur_effect)
            print "imputed data: %s" % imputed
            self.update_2x2_table(imputed)



    def _get_row_col(self, field):
        row = 0 if field in ("FP", "TP") else 1
        col = 1 if field in ("FP", "TN") else 0
        return (row, col)

    def update_2x2_table(self, imputed_dict):
        self.two_by_two_table.blockSignals(True) 
        for field in ["FP", "TP", "TN", "FN"]:
            if field in imputed_dict:
                row, col = self._get_row_col(field)
                self._set_table_item(row, col, imputed_dict[field])
                # here we update the MA unit
                raw_data_index = DIAG_FIELDS_TO_RAW_INDICES[field]
                self.ma_unit.tx_groups[self.group_str].raw_data[raw_data_index] =\
                         float(imputed_dict[field])
        self.two_by_two_table.blockSignals(False) 
      
    def _set_table_item(self, i, j, val):
        item = QTableWidgetItem(val)
        self.two_by_two_table.setItem(i, j, item)

    def update_alpha(self):
        pass

    def build_dict(self):
        d = {}
        metric_str = self.cur_effect.lower()
       

        if not self._is_txt_box_empty(self.effect_txt_box):
            try:
                d[metric_str] = float(self.effect_txt_box.text())
            except:
                pass

        if not self._is_txt_box_empty(self.low_txt_box):
            try:
                d["%s.lb" % metric_str] = float(self.low_txt_box.text())
            except:
                pass

        if not self._is_txt_box_empty(self.high_txt_box):
            try:
                d["%s.ub" % metric_str] = float(self.high_txt_box.text())
            except:
                pass
        
        if not self._is_txt_box_empty(self.alpha_edit):
            try:
                d["conf.level"] = (1.0-float(self.alpha_edit.text()))*100
            except:
                pass


        # now grab the raw data, if available
        if not self._is_empty(0,0):
            d["TP"] = float(self._get_int(0,0))
        
        if not self._is_empty(1,0):
            d["FN"] = float(self._get_int(1,0))
        
        if not self._is_empty(0,1):
            d["FP"] = float(self._get_int(0,1))
        
        if not self._is_empty(1,1):
            d["TN"] = float(self._get_int(1,1))

        return d

    def val_edit(self, val_str, new_val_text):
        print "imputing data!"
        self.impute_data()  

    def effect_changed(self):
        self.cur_effect = str(self.effect_cbo_box.currentText()) 
        self.set_current_effect()

    def _update_raw_data(self):
        ''' populates the 2x2 table with whatever parametric data was provided '''
        self.two_by_two_table.blockSignals(True) 
        field_index = 0
        for col in (0,1):
            for row in (0,1):
                val = self.raw_data_dict[self.group_str][field_index]
                if val is not None:
                    try:
                        val = str(int(val))
                    except:
                        val = str(val)
                    item = QTableWidgetItem(val)
                    self.two_by_two_table.setItem(row, col, item)
                field_index+=1
        self.two_by_two_table.blockSignals(False)

    def _populate_effect_data(self):
        # for now we only back-calculate from sens/spec
        effects = ["Sens", "Spec"] # TODO add more metrics
        self.effect_cbo_box.blockSignals(True)
        self.effect_cbo_box.addItems(effects)
        self.effect_cbo_box.blockSignals(False)
        self.effect_cbo_box.setCurrentIndex(0)

        # populate fields with current effect data
        self.set_current_effect()
    
    def set_current_effect(self):
        effect_dict = self.ma_unit.effects_dict[self.cur_effect][self.group_str]
        for s, txt_box in zip(['display_est', 'display_lower', 'display_upper'], \
                              [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            txt_box.blockSignals(True)
            if effect_dict[s] is not None:
                txt_box.setText(QString("%s" % round(effect_dict[s], NUM_DIGITS)))
            else:
                txt_box.setText(QString(""))
            txt_box.blockSignals(False)

    def _update_data_table(self):
        pass
