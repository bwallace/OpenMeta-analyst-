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
        
        entry_widgets = [self.two_by_two_table, self.alpha_edit,\
            self.low_txt_box, self.high_txt_box, self.effect_txt_box]
        
        # block all the widgets for a moment
        for widget in entry_widgets:
            widget.blockSignals(True)
        
        self._setup_inconsistency_checking()
        self._update_raw_data()            # ma_unit --> table
        self._populate_effect_data()       # make combo boxes for effects
        self._impute_2by2_from_effects()   # back-calculate 2x2
        self._update_data_table() # does nothing....
        self.set_current_effect() # fill in current effect data in line edits

        # unblock
        for widget in entry_widgets:
            widget.blockSignals(False)
        
        # used for checking the data we attempt to set is good
        self.current_item_data = self._get_int(self.two_by_two_table.currentRow(),self.two_by_two_table.currentColumn())
        
        # for validation on text boxes
        self.curr_effect_tbox_text = self.effect_txt_box.text()
        self.curr_low_tbox_text    = self.low_txt_box.text()
        self.curr_high_tbox_text  = self.high_txt_box.text()
        (self.candidate_est,self.candidate_lower,self.candidate_upper) = (None,None,None)
    
    def setup_signals_and_slots(self):
        QObject.connect(self.two_by_two_table, SIGNAL("cellChanged (int, int)"), 
                                            self._cell_changed)
        QObject.connect(self.alpha_edit, SIGNAL("textChanged (QString)"), 
                                            self.update_alpha)                            
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                             self.effect_changed) 
        
        QObject.connect(self.effect_txt_box, SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("est", new_text))
        QObject.connect(self.low_txt_box,    SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("lower", new_text))
        QObject.connect(self.high_txt_box,   SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("upper", new_text))
        
        QObject.connect(self.effect_txt_box, SIGNAL("editingFinished()"), lambda: self.val_changed("est")   )
        QObject.connect(self.low_txt_box,    SIGNAL("editingFinished()"), lambda: self.val_changed("lower") )
        QObject.connect(self.high_txt_box,   SIGNAL("editingFinished()"), lambda: self.val_changed("upper") )


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


    @pyqtSignature("int, int, int, int")
    def on_raw_data_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
        self.current_item_data = self._get_int(currentRow,currentColumn)
        print "Current Item Data:",self.current_item_data

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
        ##print "previous cell data:",self.current_item_data
        ##print "new cell data:", self.two_by_two_table.item(row, col).text()
        print "------------------------------------"
        print "CELL CHANGED"
        
        new_num_not_valid = self._cell_data_not_valid(self.two_by_two_table.item(row, col).text())
        # Test if entered data is valid (a number)
        if new_num_not_valid:
            # popup warning message
            QMessageBox.warning(self.parent(), "whoops", new_num_not_valid)
            # set value back to original and leave, doing nothing
            self.two_by_two_table.blockSignals(True)
            self._set_val(row, col, self.current_item_data)
            self.two_by_two_table.blockSignals(False)
            return
        
        # Try to calculate the rest of the table values
        table_values = self._get_table_vals()
        print "Table Values: ", table_values
        computed_parameters = meta_globals.compute_2x2_table(table_values)
        if computed_parameters:
            print("Computed Parameters:",computed_parameters)
            self._set_vals(computed_parameters) # computed --> table widget
        self.current_item_data = self._get_int(row,col) # For verification
        
        #check consistency of table
        self.check_table_consistency.run()
        
        self._update_ma_unit()           # 2x2 table --> ma_unit
        self.impute_effects_in_ma_unit() # effects   --> ma_unit
        self.set_current_effect()        # ma_unit   --> effects
    
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


        # now grab the raw data, if available
        if not self._is_invalid(0,0):
            d["TP"] = float(self._get_int(0,0))
        
        if not self._is_invalid(1,0):
            d["FN"] = float(self._get_int(1,0))
        
        if not self._is_invalid(0,1):
            d["FP"] = float(self._get_int(0,1))
        
        if not self._is_invalid(1,1):
            d["TN"] = float(self._get_int(1,1))

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
            

    #def val_edit(self, val_str, new_val_text):
    #    print "imputing data!"
    #    self.impute_data()
        
        
    def val_changed(self, val_str):
        def is_between_bounds(est=self.curr_effect_tbox_text, 
                              low=self.curr_low_tbox_text, 
                              high=self.curr_high_tbox_text):
            return meta_globals.between_bounds(est=est, low=low, high=high)
        def block_box_signals(state):
            self.effect_txt_box.blockSignals(state)
            self.low_txt_box.blockSignals(state)
            self.high_txt_box.blockSignals(state)
        ###### ERROR CHECKING CODE#####
        # Make sure entered value is numeric and between the appropriate bounds
        block_box_signals(True)
        float_msg = "Must be numeric!"
        errorflag = False
        if val_str == "est" and not _is_empty(self.candidate_est):
            # Check type
            if not _is_a_float(self.candidate_est) :
                QMessageBox.warning(self.parent(), "whoops", float_msg)
                errorflag = True
            (good_result,msg) = is_between_bounds(est=self.candidate_est)
            if not good_result:
                QMessageBox.warning(self.parent(), "whoops", msg)
                errorflag = True
            if errorflag:
                self.effect_txt_box.setText(self.curr_effect_tbox_text)
                self.candidate_est = self.curr_effect_tbox_text
                self.effect_txt_box.setFocus()
                block_box_signals(False)
                return
            display_scale_val = float(self.candidate_est)
        elif val_str == "lower" and not _is_empty(self.candidate_lower):
            if not _is_a_float(self.candidate_lower) :
                QMessageBox.warning(self.parent(), "whoops", float_msg)
                errorflag=True
            (good_result,msg) = is_between_bounds(low=self.candidate_lower)
            if not good_result:
                QMessageBox.warning(self.parent(), "whoops", msg)
                errorflag=True
            if errorflag:
                self.low_txt_box.setText(self.curr_low_tbox_text)
                self.candidate_lower = self.curr_low_tbox_text
                self.low_txt_box.setFocus()
                block_box_signals(False)
                return
            display_scale_val = float(self.candidate_lower)
        elif val_str == "upper" and not _is_empty(self.candidate_upper): 
            if not _is_a_float(self.candidate_upper) :
                QMessageBox.warning(self.parent(), "whoops", float_msg)
                errorflag=True
            (good_result,msg) = is_between_bounds(high=self.candidate_upper)
            if not good_result:
                QMessageBox.warning(self.parent(), "whoops", msg)
                errorflag=True
            if errorflag:
                self.high_txt_box.setText(self.curr_high_tbox_text)
                self.candidate_upper = self.curr_high_tbox_text
                self.high_txt_box.setFocus()
                block_box_signals(False)
                return
            display_scale_val = float(self.candidate_upper)
            
        block_box_signals(False)
        # If we got to this point it means everything is ok so far
    
        self.curr_effect_tbox_text = self.effect_txt_box.text()
        self.curr_low_tbox_text    = self.low_txt_box.text()
        self.curr_high_tbox_text   = self.high_txt_box.text()
        
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

        # Impute 2x2 from here
        print "imputing data!"
        self.impute_data()
    
    def val_edit(self, val_str, display_scale_val):
        print "Editing %s with value: %s" % (val_str,display_scale_val)
        if val_str == "est":
            self.candidate_est = display_scale_val
        if val_str == "lower":
            self.candidate_lower = display_scale_val
        if val_str == "upper":
            self.candidate_upper = display_scale_val  

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

    def _update_data_table(self):
        '''Fill-in 2x2 table from provided (spreadsheet raw data) information'''
        
        self.two_by_two_table.blockSignals(True)
        raw_data = self._get_raw_data()
        
        print("Updating raw_data with",raw_data)
        
        params = {}
        params["c11"] = raw_data['TP']
        params["c12"] = raw_data['FP']
        params["c21"] = raw_data['FN']
        params["c22"] = raw_data['TN']
        params["r1sum"] = None
        params["r2sum"] = None
        params["c1sum"] = None
        params["c2sum"] = None
        params["total"] = None
        
        computed_params = meta_globals.compute_2x2_table(params)
        if computed_params:
            self._set_vals(computed_params) # computed --> table widget
        
        self.check_table_consistency.run()
        self.two_by_two_table.blockSignals(False)
        
        
        