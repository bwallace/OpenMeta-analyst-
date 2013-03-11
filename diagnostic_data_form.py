##################################################
#
#  Byron C. Wallace                  
#  George Dietz                      
#  CEBM @ Brown                       
#  OpenMeta[analyst] 
#
#  ---
#  Diagnostic data form module; for flexible entry of diagnostic
#  outcome data.
#
##################################################


import pdb

from PyQt4.Qt import *
#from PyQt4 import QtGui

import meta_py_r
import meta_globals
from meta_globals import (_is_a_float, _is_empty, NUM_DIGITS,
                          DIAGNOSTIC_METRICS, DIAG_FIELDS_TO_RAW_INDICES,EMPTY_VALS)
#import ui_continuous_data_form
from ui_diagnostic_data_form import Ui_DiagnosticDataForm

BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS = ["Sens", "Spec"]

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
        self.group_str = cur_group_str
        self.cur_effect = "Sens" # arbitrary
        self.alpha = .05
        
        # Stores form effect info as text
        self.form_effects_dict = {"Sens":{"est":"","lower":"","upper":"","se":""},
                                  "Spec":{"est":"","lower":"","upper":"","se":""},
                                  "alpha":"","prevalence":""}
        # Stores table items as text
        self.table_backup = [[None,None,None],[None,None,None],[None,None,None]]
        
        entry_widgets = [self.two_by_two_table, self.alpha_edit,\
            self.low_txt_box, self.high_txt_box, self.effect_txt_box, self.se_txt_box]
        
        # block all the widgets for a moment
        for widget in entry_widgets:
            widget.blockSignals(True)
        
        self.alpha_edit.setText(".05")
        
        self._setup_inconsistency_checking()
        self._update_raw_data()           # ma_unit --> table
        self._populate_effect_data()      # make combo boxes for effects
        self._impute_2by2_from_effects()  # back-calculate 2x2
        self._update_data_table()         # fill in the rest of the data table
        self.set_current_effect()         # fill in current effect data in line edits
        self._setup_table_effect_dict()   # stores effect info locally
        self._save_table_data()
        self._save_displayed_effects_data() # for validation on text boxes

        # unblock
        for widget in entry_widgets:
            widget.blockSignals(False)
        
        (self.candidate_est,self.candidate_lower,self.candidate_upper,) = (None,None,None)
        self.candidate_alpha = None
        self.candidate_prevalence = None
        #self.candidate_se = None
    
        # SE MAKES NO SENSE IN THIS CONTEXT........
        self.se_txt_box.setVisible(False)
        self.se_lbl.setVisible(False)
    
    def setup_signals_and_slots(self):
        QObject.connect(self.two_by_two_table, SIGNAL("cellChanged (int, int)"), 
                                            self._cell_changed)                          
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                             self.effect_changed) 
        QObject.connect(self.startover_Btn, SIGNAL("clicked()"), self.start_over)
        
        QObject.connect(self.effect_txt_box, SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("est", new_text))
        QObject.connect(self.low_txt_box,    SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("lower", new_text))
        QObject.connect(self.high_txt_box,   SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("upper", new_text))
        QObject.connect(self.alpha_edit,     SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("alpha", new_text))
        QObject.connect(self.prevalence_txt_box, SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("prevalence", new_text))
        QObject.connect(self.se_txt_box, SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("se", new_text))
        
        QObject.connect(self.effect_txt_box, SIGNAL("editingFinished()"), lambda: self.val_changed("est")   )
        QObject.connect(self.low_txt_box,    SIGNAL("editingFinished()"), lambda: self.val_changed("lower") )
        QObject.connect(self.high_txt_box,   SIGNAL("editingFinished()"), lambda: self.val_changed("upper") )
        QObject.connect(self.alpha_edit,     SIGNAL("editingFinished()"), lambda: self.val_changed("alpha") )
        QObject.connect(self.prevalence_txt_box, SIGNAL("editingFinished()"), lambda: self.val_changed("prevalence") )
        QObject.connect(self.se_txt_box,     SIGNAL("editingFinished()"), lambda: self.val_changed("se") )

######################### INCONSISTENCY CHECKING STUFF #########################
    def _setup_inconsistency_checking(self):
        # set-up inconsistency label
        inconsistency_palette = QPalette()
        inconsistency_palette.setColor(QPalette.WindowText,Qt.red)
        self.inconsistencyLabel.setPalette(inconsistency_palette)
        self.inconsistencyLabel.setVisible(False)
        
        self.check_table_consistency = meta_globals.ConsistencyChecker(
                            fn_consistent=self.action_consistent_table,
                            fn_inconsistent=self.action_inconsistent_table,
                            table_2x2 = self.two_by_two_table)

    def action_consistent_table(self):    
        self.inconsistencyLabel.setVisible(False)
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(True)
    def action_inconsistent_table(self):
        #show label, disable OK buttonbox button
        self.inconsistencyLabel.setVisible(True)
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(False)
####################### END INCONSISTENCY CHECKING STUFF #######################


#    @pyqtSignature("int, int, int, int")
#    def on_raw_data_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
#        #self.current_item_data = self._get_int(currentRow,currentColumn)
#        #print "Current Item Data:",self.current_item_data
#        pass

    def _get_int(self, i, j):
        try:
            if not self._is_empty(i,j):
                int_val = int(float(self.two_by_two_table.item(i, j).text()))
                return int_val
        except:
            print "Could not convert %s to integer" % self.two_by_two_table.item(i, j).text()
            return None
    
    def _cell_data_not_valid(self, celldata_string):
        # ignore blank entries
        if celldata_string.trimmed() == "" or celldata_string is None:
            return None

        if not meta_globals._is_a_float(celldata_string):
            return "Raw data needs to be numeric."

        if not meta_globals._is_an_int(celldata_string):
            return "Expecting count data -- you provided a float (?)"

        if int(celldata_string) < 0:
            return "Counts cannot be negative."
        return None

    def _is_empty(self, i, j):
        val = self.two_by_two_table.item(i,j)
        return val is None or val.text() == ""
    def _is_invalid(self, i, j):
        val = self.two_by_two_table.item(i,j)
        try:
            int(val.text())
        except:
            return True
        return False
    
    def _is_txt_box_empty(self, txt_box):
        val = txt_box.text()
        return val is None or val == ""
    def _is_txt_box_invalid(self, txt_box):
        val = txt_box.text()
        return meta_globals.is_NaN(val) or self._is_txt_box_empty(txt_box) or (not _is_a_float(val))
    
    def _set_val(self, row, col, val):
        is_NaN = lambda x: x != x
        
        # get out quick
        if is_NaN(val):
            return
        
        # need this to reset empty cells
        if val in EMPTY_VALS:
            self.two_by_two_table.setItem(row, col, QTableWidgetItem(""))
            return

        try:
            val = str(int(val))
        except:
            print("Got to except in _set_val")
        self.two_by_two_table.setItem(row, col, QTableWidgetItem(QString(val))) 
    
    def _set_vals(self, computed_d):
        '''Sets values in table widget'''
        self.two_by_two_table.blockSignals(True)
        self._set_val(0, 0, computed_d["c11"])
        self._set_val(0, 1, computed_d["c12"])
        self._set_val(1, 0, computed_d["c21"])
        self._set_val(1, 1, computed_d["c22"])  
        self._set_val(0, 2, computed_d["r1sum"])
        self._set_val(1, 2, computed_d["r2sum"])
        self._set_val(2, 0, computed_d["c1sum"])
        self._set_val(2, 1, computed_d["c2sum"])  
        self._set_val(2, 2, computed_d["total"])  
        self.two_by_two_table.blockSignals(False)

    def _cell_changed(self, row, col):
        new_num_not_valid = self._cell_data_not_valid(self.two_by_two_table.item(row, col).text())
        # Test if entered data is valid (a number)
        if new_num_not_valid:
            # popup warning message
            QMessageBox.warning(self.parent(), "whoops", new_num_not_valid)
            # set value back to original and leave, doing nothing
            self._restore_table()
            return
    
        self._update_data_table() # calculate rest of data table
        
        #check consistency of table (rows sum, etc)
        (check, msg) = self.check_table_consistency.run()
        if msg:
            QMessageBox.warning(self.parent(), "whoops", msg)
            #pyqtRemoveInputHook()
            #pdb.set_trace()
            self._restore_table()
            return
        
        self._save_table_data()
        self._update_ma_unit()           # 2x2 table --> ma_unit
        self.impute_effects_in_ma_unit() # effects   --> ma_unit
        self.set_current_effect()        # ma_unit   --> effects
        
    def _save_displayed_effects_data(self):
        print "Saving Displayed Effects data...."
        
        self.form_effects_dict[self.cur_effect]["est"]   = self.effect_txt_box.text() 
        self.form_effects_dict[self.cur_effect]["lower"] = self.low_txt_box.text()    
        self.form_effects_dict[self.cur_effect]["upper"] = self.high_txt_box.text()    
        #self.form_effects_dict[self.cur_effect]["se"]    = self.se_txt_box.text()
        self.form_effects_dict["alpha"]                  = self.alpha_edit.text() 
        self.form_effects_dict["prevalence"]             = self.prevalence_txt_box.text()
        
    def _restore_displayed_effects_data(self):
        print "Restoring displayed effects data..."
        
        self.block_box_signals(True)
        self.effect_txt_box.setText(    self.form_effects_dict[self.cur_effect]["est"]  )    
        self.low_txt_box.setText(       self.form_effects_dict[self.cur_effect]["lower"])       
        self.high_txt_box.setText(      self.form_effects_dict[self.cur_effect]["upper"])      
        #self.se_txt_box.setText(        self.form_effects_dict[self.cur_effect]["se"]   )        
        self.alpha_edit.setText(        self.form_effects_dict["alpha"]                 )        
        self.prevalence_txt_box.setText(self.form_effects_dict["prevalence"]            )
        
        self.candidate_est        = self.effect_txt_box.text()
        self.candidate_lower      = self.low_txt_box.text()
        self.candidate_upper      = self.high_txt_box.text()
        self.candidate_alpha      = self.alpha_edit.text()
        self.candidate_prevalence = self.prevalence_txt_box.text()
        self.block_box_signals(False)
                                    
    def _save_table_data(self):
        for row in range(3):
            for col in range(3):
                contents = self.two_by_two_table.item(row, col).text()
                self.table_backup[row][col]=contents
        self.prevalence_backup = self.prevalence_txt_box.text()
        
        #print("Backed-up table:")
        #self._print_backup_table()
    def _restore_table(self):
        #print "Table to restore:"
        #self._print_backup_table()
        
        self.two_by_two_table.blockSignals(True)
        for row in range(3):
            for col in range(3):
                self.two_by_two_table.item(row, col).setText(self.table_backup[row][col])
        self.check_table_consistency.run()
        
        #prevalence
        self.prevalence_txt_box.blockSignals(True)
        self.prevalence_txt_box.setText(self.prevalence_backup)
        self.prevalence_txt_box.blockSignals(False)
        
        self.two_by_two_table.blockSignals(False)
        
    def _print_backup_table(self):
        for row in range(3):
            line = ""
            for col in range(3):
                line += self.table_backup[row][col] + ", "
            print line
    
    def _get_table_vals(self):
        ''' Package table from 2x2 table in to a dictionary'''
        
        vals_d = {}
        vals_d["c11"] = self._get_int(0, 0)
        vals_d["c12"] = self._get_int(0, 1)
        vals_d["c21"] = self._get_int(1, 0)
        vals_d["c22"] = self._get_int(1, 1)
        vals_d["r1sum"] = self._get_int(0, 2)
        vals_d["r2sum"] = self._get_int(1, 2)
        vals_d["c1sum"] = self._get_int(2, 0)
        vals_d["c2sum"] = self._get_int(2, 1)
        vals_d["total"] = self._get_int(2, 2)
        return vals_d

    def impute_data(self):
        diag_data_dict = self.build_dict()

        if diag_data_dict is not None:
            print "arguments to imputed data: ", diag_data_dict, self.cur_effect
            imputed = meta_py_r.impute_diag_data(diag_data_dict, self.cur_effect)
            print "imputed data: %s" % imputed
            self.update_2x2_table(imputed)
    
    def _impute_2by2_from_effects(self):
        original_effect = self.cur_effect
        
        for effect in BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS:
            # stupid way to do this but whatever
            #    should be done by looking at ma_unit, not playing with gui
            self.cur_effect = effect
            self.set_current_effect()
            
            diag_data_dict = self.build_dict()
            if diag_data_dict is not None:
                print "arguments to imputed data: ", diag_data_dict, effect
                imputed = meta_py_r.impute_diag_data(diag_data_dict, effect)
                print "imputed data: %s" % imputed
                self.update_2x2_table(imputed)
                
        # restore things to how they were
        self.cur_effect = original_effect
        self.set_current_effect()

    def _get_row_col(self, field):
        row = 0 if field in ("FP", "TP") else 1
        col = 1 if field in ("FP", "TN") else 0
        return (row, col)

    def update_2x2_table(self, imputed_dict):
        ''' Fill in entries in 2x2 table and add data to ma_unit'''
        
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
    
    def _update_ma_unit(self):
        '''Copy data from data table to the MA_unit'''
        raw_dict = self._get_raw_data() # values are floats or None
        for field,value in raw_dict.iteritems():
            i = DIAG_FIELDS_TO_RAW_INDICES[field]
            self.ma_unit.tx_groups[self.group_str].raw_data[i] = raw_dict[field]
        
    
    #### MOSTLY DUPLICATED FROM ma_data_table_model.update_outcome_if_possible()
    def impute_effects_in_ma_unit(self):
        '''Calculate and store values for effects in ma_unit based on values in 2x2 table'''
        
        # diagnostic data
        counts = self._get_raw_data()
        tp, fn, fp, tn = counts['TP'], counts['FN'], counts['FP'], counts['TN']
        
        if None in [tp,fn,fp,tn]:
            return  # do nothing if we don't have all the counts
        
        # sensitivity and specificity
        ests_and_cis = meta_py_r.diagnostic_effects_for_study(\
                                tp, fn, fp, tn, metrics=DIAGNOSTIC_METRICS)
        
        # now we're going to set the effect estimate/CI on the MA object.
        for metric in DIAGNOSTIC_METRICS:
            est, lower, upper = ests_and_cis[metric]["calc_scale"]
            self.ma_unit.set_effect_and_ci(metric, self.group_str, est, lower, upper)
            
            disp_est, disp_lower, disp_upper = ests_and_cis[metric]["display_scale"]
            self.ma_unit.set_display_effect_and_ci(metric, self.group_str, disp_est, disp_lower, disp_upper)
        
      
    def _set_table_item(self, i, j, val):
        if meta_globals.is_NaN(val):
            print "%s is not a number" % val
            return
        
        item = QTableWidgetItem(val)
        self.two_by_two_table.setItem(i, j, item)

    def update_alpha(self):
        pass



    def build_dict(self):
        d = {}
        metric_str = self.cur_effect.lower()
       

        if not self._is_txt_box_invalid(self.effect_txt_box):
            try:
                d[metric_str] = float(self.effect_txt_box.text())
            except:
                pass

        if not self._is_txt_box_invalid(self.low_txt_box):
            try:
                d["%s.lb" % metric_str] = float(self.low_txt_box.text())
            except:
                pass

        if not self._is_txt_box_invalid(self.high_txt_box):
            try:
                d["%s.ub" % metric_str] = float(self.high_txt_box.text())
            except:
                pass
        
        if not self._is_txt_box_invalid(self.alpha_edit):
            try:
                d["conf.level"] = (1.0-float(self.alpha_edit.text()))*100
            except:
                pass
        
        if not self._is_txt_box_invalid(self.prevalence_txt_box):
            try:
                d["prev"] = float(self.prevalence_txt_box.text())
            except:
                pass
        
        if not self._is_txt_box_invalid(self.se_txt_box):
            try:
                d["%s.se" % metric_str] = float(self.se_txt_box.text())
            except:
                pass

        # now grab the raw data, if available
        d.update(self._get_raw_data())
        
        return d
    
    def _get_raw_data(self,convert_None_to_NA_string=False):
        '''Returns a dictionary of the raw data in the table, None for empty cell'''
        
        NoneValue = "NA" if convert_None_to_NA_string else None
        
        d={}
        d["TP"] = float(self._get_int(0,0)) if not self._is_empty(0,0) else NoneValue
        d["FN"] = float(self._get_int(1,0)) if not self._is_empty(1,0) else NoneValue
        d["FP"] = float(self._get_int(0,1)) if not self._is_empty(0,1) else NoneValue
        d["TN"] = float(self._get_int(1,1)) if not self._is_empty(1,1) else NoneValue
        return d

    def block_box_signals(self,state):
        self.effect_txt_box.blockSignals(state)
        self.low_txt_box.blockSignals(state)
        self.high_txt_box.blockSignals(state)
        self.alpha_edit.blockSignals(state)
        self.prevalence_txt_box.blockSignals(state)
        #self.se_txt_box.blockSignals(state)

    def val_changed(self, val_str):
        #print "--------------\nEntering val_changed...."
        
        def is_between_bounds(est=self.form_effects_dict[self.cur_effect]["est"], 
                              low=self.form_effects_dict[self.cur_effect]["lower"], 
                              high=self.form_effects_dict[self.cur_effect]["upper"]):
            return meta_globals.between_bounds(est=est, low=low, high=high)

        ###### ERROR CHECKING CODE#####
        # Make sure entered value is numeric and between the appropriate bounds
        self.block_box_signals(True)
        float_msg = "Must be numeric!"
        try:
            if val_str == "est" and not _is_empty(self.candidate_est):
                # Check type
                if not _is_a_float(self.candidate_est) :
                    QMessageBox.warning(self.parent(), "whoops", float_msg)
                    raise Exception("error")
                (good_result,msg) = is_between_bounds(est=self.candidate_est)
                if not good_result:
                    QMessageBox.warning(self.parent(), "whoops", msg)
                    raise Exception("error")
                display_scale_val = float(self.candidate_est)
            elif val_str == "lower" and not _is_empty(self.candidate_lower):
                if not _is_a_float(self.candidate_lower) :
                    QMessageBox.warning(self.parent(), "whoops", float_msg)
                    raise Exception("error")
                (good_result,msg) = is_between_bounds(low=self.candidate_lower)
                if not good_result:
                    QMessageBox.warning(self.parent(), "whoops", msg)
                    raise Exception("error")
                display_scale_val = float(self.candidate_lower)
            elif val_str == "upper" and not _is_empty(self.candidate_upper): 
                if not _is_a_float(self.candidate_upper) :
                    QMessageBox.warning(self.parent(), "whoops", float_msg)
                    raise Exception("error")
                (good_result,msg) = is_between_bounds(high=self.candidate_upper)
                if not good_result:
                    QMessageBox.warning(self.parent(), "whoops", msg)
                    raise Exception("error")
                display_scale_val = float(self.candidate_upper)
            elif val_str == "alpha" and not _is_empty(self.candidate_alpha):
                if not _is_a_float(self.candidate_alpha):
                    QMessageBox.warning(self.parent(), "whoops", float_msg)
                    raise Exception("error")
                if _is_a_float(self.candidate_alpha) and not 0 < float(self.candidate_alpha) < 1:
                    QMessageBox.warning(self.parent(), "whoops", "Alpha must be between 0 and 1 (closer to zero please!")
                    raise Exception("error")
            elif val_str == "prevalence" and not _is_empty(self.candidate_prevalence):
                if not _is_a_float(self.candidate_prevalence):
                    QMessageBox.warning(self.parent(), "whoops", float_msg)
                    raise Exception("error")
                if _is_a_float(self.candidate_prevalence) and not 0 < float(self.candidate_prevalence) < 1:
                    QMessageBox.warning(self.parent(), "whoops", "Prevalence must be between 0 and 1.")
                    raise Exception("error")
    #        elif val_str == "se" and not _is_empty(self.candidate_se):
    #            if not _is_a_float(self.candidate_se):
    #                QMessageBox.warning(self.parent(), "whoops", float_msg)
    #                errorflag = True
    #            if _is_a_float(self.candidate_se) and not 0 < float(self.candidate_se):
    #                QMessageBox.warning(self.parent(), "whoops", "Standard Error must be greater than 0.")
    #                errorflag = True
    #            if errorflag:
    #                self.se_txt_box.setText(self.curr_se_tbox_text)
    #                self.candidate_se = self.curr_se_tbox_text
    #                self.se_txt_box.setFocus()
    #                block_box_signals(False)
    #                return
        except:
            print "Error flag is true"
            self._restore_displayed_effects_data()
            self.block_box_signals(True)
            if val_str == "est":
                self.effect_txt_box.setFocus()
            elif val_str == "lower":
                self.low_txt_box.setFocus()
            elif val_str == "upper":
                self.high_txt_box.setFocus()
            elif val_str == "alpha":
                self.alpha_edit.setFocus()
            elif val_str == "prevalence":
                self.prevalence_txt_box.setFocus()
            self.block_box_signals(False)
            return
                
        self.block_box_signals(False)
        
        # If we got to this point it means everything is ok so far
        self._save_displayed_effects_data()
        
        try:
            display_scale_val = float(display_scale_val)
        except:
            # a number wasn't entered; ignore
            # should probably clear out the box here, too.
            print "fail."
            return None
            
        calc_scale_val = meta_py_r.binary_convert_scale(display_scale_val, \
                                        self.cur_effect, convert_to="calc.scale")
        
        if val_str == "est":
            self.ma_unit.set_effect(self.cur_effect, self.group_str, calc_scale_val)
            self.ma_unit.set_display_effect(self.cur_effect, self.group_str, display_scale_val)
        elif val_str == "lower":
            self.ma_unit.set_lower(self.cur_effect, self.group_str, calc_scale_val)
            self.ma_unit.set_display_lower(self.cur_effect, self.group_str, display_scale_val)
        else:
            self.ma_unit.set_upper(self.cur_effect, self.group_str, calc_scale_val)
            self.ma_unit.set_display_upper(self.cur_effect, self.group_str, display_scale_val)
        
        # update effect quantity in local dictionary of effects
        if val_str in ["est","lower","upper"]: #["est","lower","upper","se"]:
            self.form_effects_dict[self.cur_effect][val_str] = str(display_scale_val)

        # Impute 2x2 from here
        print "imputing data!"
        self.impute_data()
        self._update_data_table()
        self._save_table_data()
    
    def val_edit(self, val_str, display_scale_val):
        print "Editing %s with value: %s" % (val_str,display_scale_val)
        if val_str == "est":
            self.candidate_est = display_scale_val
        if val_str == "lower":
            self.candidate_lower = display_scale_val
        if val_str == "upper":
            self.candidate_upper = display_scale_val
        if val_str == "alpha":
            self.candidate_alpha = display_scale_val
        if val_str == "prevalence":
            self.candidate_prevalence = display_scale_val
        #if val_str == "se":
        #    self.candidate_se = display_scale_val

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
        effects = BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS # TODO add more metrics
        self.effect_cbo_box.blockSignals(True)
        self.effect_cbo_box.addItems(effects)
        self.effect_cbo_box.blockSignals(False)
        self.effect_cbo_box.setCurrentIndex(0)
    
    def set_current_effect(self):
        '''Fill in effect text boxes with data from ma_unit'''
        effect_dict = self.ma_unit.effects_dict[self.cur_effect][self.group_str]
        for s, txt_box in zip(['display_est', 'display_lower', 'display_upper'], \
                              [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            txt_box.blockSignals(True)
            if effect_dict[s] is not None:
                txt_box.setText(QString("%s" % round(effect_dict[s], NUM_DIGITS)))
            else:
                txt_box.setText(QString(""))
            txt_box.blockSignals(False)
            
    def _setup_table_effect_dict(self):
        '''Fill in local copy of table-effects dict w/ data from ma_unit'''
        for effect in BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS:
            effects_dict = self.ma_unit.effects_dict[self.cur_effect][self.group_str]
            for keyA,keyB in zip(['display_est', 'display_lower', 'display_upper'],["est","lower","upper"]):
                self.form_effects_dict[effect][keyB] = str(effects_dict[keyA])
        print "Form effects dict:",self.form_effects_dict

    def _update_data_table(self):
        '''Try to calculate rest of 2x2 table from existing cells'''
        
        self.two_by_two_table.blockSignals(True)
        
        params = self._get_table_vals()
        computed_params = meta_globals.compute_2x2_table(params)
        print "Computed Params", computed_params
        if computed_params:
            self._set_vals(computed_params) # computed --> table widget
        
        # Compute prevalence if possible
        self.prevalence_txt_box.blockSignals(True)
        if (not computed_params['c1sum'] in EMPTY_VALS) and (not computed_params['total'] in EMPTY_VALS):
            prevalence = float(computed_params['c1sum'])/float(computed_params['total'])
            prev_str = str(prevalence)[:7]
            self.prevalence_txt_box.setText("%s" % prev_str)
            self.curr_prevalence_tbox_text = self.prevalence_txt_box.text()
        self.prevalence_txt_box.blockSignals(False)
        
        
        self.two_by_two_table.blockSignals(False)
        
    def start_over(self):
        blank_vals = {}
        blank_vals["c11"]   = ""        blank_vals["c12"]   = ""        blank_vals["c21"]   = ""        blank_vals["c22"]   = ""        blank_vals["r1sum"] = ""        blank_vals["r2sum"] = ""        blank_vals["c1sum"] = ""        blank_vals["c2sum"] = ""        blank_vals["total"] = ""
        self._set_vals(blank_vals)
        self._update_ma_unit()
        
        # clear out effects stuff
        for metric in DIAGNOSTIC_METRICS:
            self.ma_unit.set_effect_and_ci(metric, self.group_str, None, None, None)
            self.ma_unit.set_display_effect_and_ci(metric, self.group_str, None, None, None)
        self.set_current_effect()
        
        self.prevalence_txt_box.blockSignals(True)
        self.prevalence_backup = ""
        self.prevalence_txt_box.setText("")
        self.prevalence_txt_box.blockSignals(False)
            