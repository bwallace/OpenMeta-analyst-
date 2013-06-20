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
import copy
from functools import partial

from PyQt4.Qt import *

import meta_py_r
import meta_globals
from meta_globals import (_is_a_float, _is_empty, DIAGNOSTIC_METRICS,
                          DIAG_FIELDS_TO_RAW_INDICES,EMPTY_VALS)
import calculator_routines as calc_fncs
from forms.ui_diagnostic_data_form import Ui_DiagnosticDataForm

BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS = ["Sens", "Spec"]

class DiagnosticDataForm(QDialog, Ui_DiagnosticDataForm):
    def __init__(self, ma_unit, cur_txs, cur_group_str, conf_level=None, parent=None):
        super(DiagnosticDataForm, self).__init__(parent)
        self.setupUi(self)
        
        if conf_level is None:
            raise ValueError("Confidence level must be specified")
        self.global_conf_level = conf_level
        self.mult = meta_py_r.get_mult_from_r(self.global_conf_level)
        
        self.setup_signals_and_slots()
        
        # Assign stuff
        self.ma_unit = ma_unit
        self.cur_groups = cur_txs
        self.group_str = cur_group_str
        self.cur_effect = "Sens" # arbitrary
        self.entry_widgets = [self.two_by_two_table, self.prevalence_txt_box,
                              self.low_txt_box, self.high_txt_box,
                              self.effect_txt_box,]
        self.text_boxes = [self.low_txt_box, self.high_txt_box,
                           self.effect_txt_box, self.prevalence_txt_box]
        
        self.ci_label.setText("{0:.1f}% Confidence Interval".format(self.global_conf_level))
        self.initialize_form()
        self.setup_inconsistency_checking()
        self.undoStack = QUndoStack(self)
        
        #self.setup_clear_button_palettes()
        self._update_raw_data()    # ma_unit -> table
        self._populate_effect_cmbo_box()     # make cmbo box entries for effects
        self.set_current_effect()   # fill in current effect data in line edits
        self._update_data_table()          # fill in the rest of the data table
        self.enable_back_calculation_btn()
        
        self.current_prevalence = self._get_prevalence_str()
        
#        # Color for clear_button_pallette
#        self.orig_palette = self.clear_Btn.palette()
#        self.pushme_palette = QPalette()
#        self.pushme_palette.setColor(QPalette.ButtonText,Qt.red)
#        self.set_clear_btn_color()
        
#    def setup_clear_button_palettes(self):
#        # Color for clear_button_pallette
#        self.orig_palette = self.clear_Btn.palette()
#        self.pushme_palette = QPalette()
#        self.pushme_palette.setColor(QPalette.ButtonText,Qt.red)
#        self.set_clear_btn_color()


    def initialize_form(self):
        ''' Initialize all cells to empty items '''
        
        nrows = self.two_by_two_table.rowCount()
        ncols = self.two_by_two_table.columnCount()
        
        for row in range(nrows):
            for col in range(ncols):
                self._set_val(row, col, None)

        for txt_box in self.text_boxes:
            txt_box.setText(QString(""))
    
    def setup_signals_and_slots(self):
        QObject.connect(self.two_by_two_table, SIGNAL("cellChanged (int, int)"), self.cell_changed)                          
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"), self.effect_changed) 
        QObject.connect(self.clear_Btn, SIGNAL("clicked()"), self.clear_form)
        QObject.connect(self.back_calc_Btn, SIGNAL("clicked()"), lambda: self.enable_back_calculation_btn(engage=True))
        
        QObject.connect(self.effect_txt_box, SIGNAL("editingFinished()"),   lambda: self.val_changed("est"))
        QObject.connect(self.low_txt_box,    SIGNAL("editingFinished()"),   lambda: self.val_changed("lower"))
        QObject.connect(self.high_txt_box,   SIGNAL("editingFinished()"),   lambda: self.val_changed("upper"))
        QObject.connect(self.prevalence_txt_box, SIGNAL("editingFinished()"),   lambda: self.val_changed("prevalence"))

        # Add undo/redo actions
        undo = QAction(self)
        redo = QAction(self)
        undo.setShortcut(QKeySequence.Undo)
        redo.setShortcut(QKeySequence.Redo)
        self.addAction(undo)
        self.addAction(redo)
        QObject.connect(undo, SIGNAL("triggered()"), self.undo)
        QObject.connect(redo, SIGNAL("triggered()"), self.redo)
        
        
    @pyqtSignature("int, int, int, int")
    def on_two_by_two_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
        self.current_item_data = self._get_int(currentRow, currentColumn)
        print("Current item data @ (%d, %d) is: %s" % (currentRow,
                                                       currentColumn,
                                                       str(self.current_item_data)))

    def setup_inconsistency_checking(self):
        # set-up inconsistency label
        inconsistency_palette = QPalette()
        inconsistency_palette.setColor(QPalette.WindowText,Qt.red)
        self.inconsistencyLabel.setPalette(inconsistency_palette)
        self.inconsistencyLabel.setVisible(False)
        
        def action_consistent_table():    
            self.inconsistencyLabel.setVisible(False)
            self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(True)
        def action_inconsistent_table():
            #show label, disable OK buttonbox button
            self.inconsistencyLabel.setVisible(True)
            self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(False)
        
        self.check_table_consistency = calc_fncs.ConsistencyChecker(
                            fn_consistent=action_consistent_table,
                            fn_inconsistent=action_inconsistent_table,
                            table_2x2 = self.two_by_two_table)

    def _get_int(self, i, j):
        try:
            if not self._is_empty(i,j):
                int_val = int(float(self.two_by_two_table.item(i, j).text()))
                return int_val
        except:
            # Should never appear....
            msg = "Could not convert %s to integer" % self.two_by_two_table.item(i, j)
            QMessageBox.warning(self.parent(), "whoops", msg)
            raise Exception("Could not convert %s to int" % self.two_by_two_table.item(i, j))
            
    def cell_data_invalid(self, celldata_string):
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
        return val is None or val.text() == "" or val.text() == None
    def _is_invalid(self, i, j):
        val = self.two_by_two_table.item(i,j)
        try:
            int(val.text())
        except:
            return True
        return False
    
    def _is_txt_box_invalid(self, txt_box):
        val = txt_box.text()
        empty = val in EMPTY_VALS
        return meta_globals.is_NaN(val) or empty or (not _is_a_float(val))
    
    def _set_val(self, row, col, val):
        if meta_globals.is_NaN(val): # get out quick
            print "%s is not a number" % val
            return
        
        try:
            str_val = "" if val in EMPTY_VALS else str(int(val))
            self.two_by_two_table.blockSignals(True)
            if self.two_by_two_table.item(row, col) == None:
                self.two_by_two_table.setItem(row, col, QTableWidgetItem(str_val))
            else:
                self.two_by_two_table.item(row, col).setText(str_val)
            self.two_by_two_table.blockSignals(False)
            
            if str_val != "": #disable item
                self.two_by_two_table.blockSignals(True)
                item = self.two_by_two_table.item(row, col)
                newflags = item.flags() & ~Qt.ItemIsEditable
                item.setFlags(newflags)
                self.two_by_two_table.blockSignals(False)
        except:
            print("Got to except in _set_val when trying to set (%d,%d)" % (row,col)) 
    
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
        
    def _get_prevalence_str(self):
        return str(self.prevalence_txt_box.text())

    def cell_changed(self, row, col):
        old_ma_unit, old_table = self._save_ma_unit_and_table_state(
                                        table = self.two_by_two_table,
                                        ma_unit = self.ma_unit, 
                                        old_value = self.current_item_data,
                                        row = row, col = col, use_old_value=True)
        old_prevalence = self._get_prevalence_str()
        
        try:
            # Test if entered data is valid (a number)
            warning_msg = self.cell_data_invalid(self.two_by_two_table.item(row, col).text())
            if warning_msg:
                raise Exception("Invalid Cell Data")
    
            self._update_data_table() # calculate rest of table (provisionally) based on new entry
            warning_msg = self.check_table_consistency.run()
            if warning_msg:
                raise Exception("Table no longer consistent.")
        except Exception as e:
            msg = e.args[0]
            QMessageBox.warning(self.parent(), "whoops", msg) #popup warning
            self.restore_ma_unit_and_table(old_ma_unit,old_table, old_prevalence) # brings things back to the way they were
            return                    # and leave
        
        # if we got here, everything seems ok
        self._update_ma_unit()           # 2x2 table --> ma_unit
        self.impute_effects_in_ma_unit() # effects   --> ma_unit
        self.set_current_effect()        # ma_unit   --> effects
    
        new_ma_unit, new_table = self._save_ma_unit_and_table_state(
                                    table = self.two_by_two_table,
                                    ma_unit = self.ma_unit, 
                                    row = row, col = col,
                                    use_old_value = False)
        new_prevalence = self._get_prevalence_str()
        restore_old_f = lambda: self.restore_ma_unit_and_table(old_ma_unit, old_table, old_prevalence)
        restore_new_f = lambda: self.restore_ma_unit_and_table(new_ma_unit, new_table, new_prevalence)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f, restore_old_f=restore_old_f, parent=self)
        self.undoStack.push(command)
        
    def restore_ma_unit(self, old_ma_unit):
        ''' Restores the ma_unit data and resets the form'''
        self.ma_unit.__dict__ = copy.deepcopy(old_ma_unit.__dict__)
        print("Restored ma_unit data: %s" % str(self.ma_unit.get_raw_data_for_groups(self.cur_groups)))
        
        self.initialize_form() # clear form first
        self._update_raw_data()
        self.set_current_effect()
        self._update_data_table()
        self.enable_back_calculation_btn()
        
    def restore_table(self, old_table_data):
        nrows = len(old_table_data)
        ncols = len(old_table_data[0])
        
        for row in range(nrows):
                for col in range(ncols):
                    self.two_by_two_table.blockSignals(True)
                    self._set_val(row, col, old_table_data[row][col])
                    self.two_by_two_table.blockSignals(False)
        self.check_table_consistency.run()
        
    def restore_ma_unit_and_table(self, old_ma_unit, old_table, old_prevalence):
        self.restore_ma_unit(old_ma_unit)
        self.restore_table(old_table)
        self.prevalence_txt_box.setText(old_prevalence)
        
    def _save_ma_unit_and_table_state(self, table, ma_unit, row=None, col=None,
                                      old_value=None, use_old_value=True):
        # Make backup of table info...
        old_table = calc_fncs.save_table_data(table)
        if use_old_value:
            old_table[row][col] = old_value   # ...from BEFORE the cell changed
        
        # Make backup copy of ma_unit
        old_ma_unit = copy.deepcopy(ma_unit)
        return old_ma_unit, old_table

    
    def getTotalSubjects(self):
        try:
            return int(self.table_backup[2][2])
        except:
            return None

    def print_backup_table(self):
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
            
    def impute_effects_in_ma_unit(self):
        '''Calculate and store values for effects in ma_unit based on values in 2x2 table'''
        
        # diagnostic data
        counts = self.get_raw_diag_data()
        tp, fn, fp, tn = counts['TP'], counts['FN'], counts['FP'], counts['TN']
        
        # Do what we can if we don't have all the counts
        can_calculate_sens, can_calculate_spec = True, True
        if None in [tp,fn]:
            can_calculate_sens = False
            tp,fn = 0,0 # dummy data
        if None in [tn,fp]:
            can_calculate_spec = False
            tn, fp = 0,0 # dummy data
        
        # sensitivity and specificity
        ests_and_cis = meta_py_r.diagnostic_effects_for_study(
                                tp, fn, fp, tn, metrics=DIAGNOSTIC_METRICS,
                                conf_level=self.global_conf_level)
        
        # now we're going to set the effect estimate/CI on the MA object.
        for metric in DIAGNOSTIC_METRICS:
            # don't set stuff if it made-up
            if metric.lower()=="sens" and not can_calculate_sens:
                continue
            elif metric.lower()=="spec" and not can_calculate_spec:
                continue
            
            est, lower, upper = ests_and_cis[metric]["calc_scale"]
            self.ma_unit.set_effect_and_ci(metric, self.group_str, est, lower, upper, mult=self.mult)

    def _get_row_col(self, field):
        row = 0 if field in ("FP", "TP") else 1
        col = 1 if field in ("FP", "TN") else 0
        return (row, col)

    def update_2x2_table(self, imputed_dict):
        ''' Fill in entries in 2x2 table and add data to ma_unit'''
        
        print "Updating 2x2......"
        
        # reset relevant column and sums column if we have new data
        if imputed_dict["TP"] and imputed_dict["FN"]:
            print("TP, FN:", imputed_dict["TP"],imputed_dict["FN"])
            print "clearing col 0 and 2"
            self.clear_column(0)
            self.clear_column(2)
        if imputed_dict["TN"] and imputed_dict["FP"]:
            print "clearing col 1 and 2"
            self.clear_column(1)
            self.clear_column(2)
        
        for field in ["FP", "TP", "TN", "FN"]:
            if (field in imputed_dict) and (not imputed_dict[field] is None):
                row, col = self._get_row_col(field)
                self._set_val(row, col, imputed_dict[field])
                # here we update the MA unit
                raw_data_index = DIAG_FIELDS_TO_RAW_INDICES[field]
                
                # TODO: ENC
                self.ma_unit.tx_groups[self.group_str].raw_data[raw_data_index] =\
                    None if not _is_a_float(imputed_dict[field]) else float(imputed_dict[field])
    
    def _update_ma_unit(self):
        '''Copy data from data table to the MA_unit'''
        
        print "updating ma unit...."
        raw_dict = self.get_raw_diag_data() # values are floats or None
        for field in raw_dict.iterkeys():
            i = DIAG_FIELDS_TO_RAW_INDICES[field]
            self.ma_unit.tx_groups[self.group_str].raw_data[i] = raw_dict[field]  # TODO: ENC
    
    def get_raw_diag_data(self,convert_None_to_NA_string=False):
        '''Returns a dictionary of the raw data in the table (TP,FN,FP,TN), 
           None for empty cell'''
        
        NoneValue = "NA" if convert_None_to_NA_string else None
        
        d={}
        d["TP"] = float(self._get_int(0,0)) if not self._is_empty(0,0) else NoneValue
        d["FN"] = float(self._get_int(1,0)) if not self._is_empty(1,0) else NoneValue
        d["FP"] = float(self._get_int(0,1)) if not self._is_empty(0,1) else NoneValue
        d["TN"] = float(self._get_int(1,1)) if not self._is_empty(1,1) else NoneValue
        return d
            
    def _text_box_value_is_between_bounds(self, val_str, new_text):
        display_scale_val = ""
        
        get_disp_scale_val_if_valid = partial(
                calc_fncs.evaluate, new_text=new_text, ma_unit=self.ma_unit,
                curr_effect=self.cur_effect, group_str=self.group_str,
                conv_to_disp_scale = partial(meta_py_r.diagnostic_convert_scale,
                                             metric_name=self.cur_effect,
                                             convert_to="display.scale"),
                parent=self, mult=meta_py_r.get_mult_from_r(self.global_conf_level))
        
        calc_fncs.block_signals(self.entry_widgets, True)
        try:
            if val_str == "est" and not _is_empty(new_text):
                display_scale_val = get_disp_scale_val_if_valid(ci_param='est')
            elif val_str == "lower" and not _is_empty(new_text):
                display_scale_val = get_disp_scale_val_if_valid(ci_param='low')
            elif val_str == "upper" and not _is_empty(new_text):
                display_scale_val = get_disp_scale_val_if_valid(ci_param='high')
            elif val_str == "prevalence" and not _is_empty(new_text):
                get_disp_scale_val_if_valid(opt_cmp_fn = lambda x: 0 <= float(x) <= 1,
                                            opt_cmp_msg="Prevalence must be between 0 and 1.")
        except:
            calc_fncs.block_signals(self.entry_widgets, False)
            return False, False
        calc_fncs.block_signals(self.entry_widgets, False)
        return True, display_scale_val

    def _get_txt_from_val_str(self, val_str):
        if val_str == "est":
            return str(self.effect_txt_box.text())
        elif val_str == "lower":
            return str(self.low_txt_box.text())
        elif val_str == "upper":
            return str(self.high_txt_box.text())
        elif val_str == "prevalence":
            return str(self.prevalence_txt_box.text())
        return None # should never happen
        

    def val_changed(self, val_str):
        # Backup form state
        old_ma_unit, old_table = self._save_ma_unit_and_table_state(
                                table = self.two_by_two_table,
                                ma_unit = self.ma_unit, 
                                use_old_value=False)
        old_prevalence = self.current_prevalence
        
        new_text = self._get_txt_from_val_str(val_str)
        
        no_errors, display_scale_val = self._text_box_value_is_between_bounds(val_str, new_text)
        if no_errors is False: # There are errors
            self.restore_ma_unit_and_table(old_ma_unit,old_table, old_prevalence)
            calc_fncs.block_signals(self.entry_widgets, True)
            if val_str == "est":
                self.effect_txt_box.setFocus()
            elif val_str == "lower":
                self.low_txt_box.setFocus()
            elif val_str == "upper":
                self.high_txt_box.setFocus()
            elif val_str == "prevalence":
                self.prevalence_txt_box.setFocus()
            calc_fncs.block_signals(self.entry_widgets, False)
            return
        
        # If we got to this point it means everything is ok so far        
        try:
            if display_scale_val not in meta_globals.EMPTY_VALS:
                display_scale_val = float(display_scale_val)
            else:
                display_scale_val = None
        except ValueError:
            # a number wasn't entered; ignore
            # should probably clear out the box here, too.
            print "fail."
            return None
        

        calc_scale_val = meta_py_r.diagnostic_convert_scale(display_scale_val,
                                        self.cur_effect, convert_to="calc.scale")
        
        if val_str == "est":
            self.ma_unit.set_effect(self.cur_effect, self.group_str, calc_scale_val)
        elif val_str == "lower":
            self.ma_unit.set_lower(self.cur_effect, self.group_str, calc_scale_val)
        elif val_str == "upper":
            self.ma_unit.set_upper(self.cur_effect, self.group_str, calc_scale_val)
        elif val_str == "prevalence":
            pass

        new_ma_unit, new_table = self._save_ma_unit_and_table_state(
                table = self.two_by_two_table, ma_unit = self.ma_unit,
                use_old_value=False)
        new_prevalence = self._get_prevalence_str()
        restore_old_f = lambda: self.restore_ma_unit_and_table(old_ma_unit, old_table, old_prevalence)
        restore_new_f = lambda: self.restore_ma_unit_and_table(new_ma_unit, new_table, new_prevalence)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f, restore_old_f=restore_old_f, parent=self)
        self.undoStack.push(command)
        
        self.current_prevalence = new_prevalence

    def effect_changed(self):
        self.cur_effect = str(self.effect_cbo_box.currentText()) 
        self.set_current_effect()
        
        self.enable_txt_box_input()
        self.enable_back_calculation_btn()
            
    def _update_raw_data(self):
        ''' populates the 2x2 table with whatever parametric data was provided '''
        self.two_by_two_table.blockSignals(True) 
        field_index = 0
        for col in (0,1):
            for row in (0,1):
                val = self.ma_unit.get_raw_data_for_group(self.group_str)[field_index]
                if val is not None:
                    try:
                        val = str(int(val))
                    except:
                        val = str(val)
                    item = QTableWidgetItem(val)
                    self.two_by_two_table.setItem(row, col, item)
                field_index+=1
        self.two_by_two_table.blockSignals(False)

    def _populate_effect_cmbo_box(self):
        # for now we only back-calculate from sens/spec
        effects = BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS # TODO add more metrics
        self.effect_cbo_box.blockSignals(True)
        self.effect_cbo_box.addItems(effects)
        self.effect_cbo_box.blockSignals(False)
        self.effect_cbo_box.setCurrentIndex(0)
    
    def set_current_effect(self):
        '''Fill in effect text boxes with data from ma_unit'''
        txt_boxes = dict(effect=self.effect_txt_box, lower=self.low_txt_box, upper=self.high_txt_box)
        calc_fncs.helper_set_current_effect(ma_unit=self.ma_unit,
            txt_boxes=txt_boxes, current_effect=self.cur_effect,
            group_str=self.group_str, data_type="diagnostic", mult=meta_py_r.get_mult_from_r(self.global_conf_level))
    
    def print_effects_dict_from_ma_unit(self):
        print self.ma_unit.get_effects_dict()

    def _update_data_table(self):
        '''Try to calculate rest of 2x2 table from existing cells'''
        
        calc_fncs.block_signals(self.entry_widgets, True)
        
        params = self._get_table_vals()
        computed_params = calc_fncs.compute_2x2_table(params)
        print "Computed Params", computed_params
        if computed_params:
            self._set_vals(computed_params) # computed --> table widget
        
        # Compute prevalence if possible
        if (not computed_params['c1sum'] in EMPTY_VALS) and (not computed_params['total'] in EMPTY_VALS):
            prevalence = float(computed_params['c1sum'])/float(computed_params['total'])
            prev_str = str(prevalence)[:7]
            self.prevalence_txt_box.setText("%s" % prev_str)
            self.enable_txt_box_input()
        
        calc_fncs.block_signals(self.entry_widgets, False)
        
    def clear_column(self,col):
        '''Clears out column in table and ma_unit'''
        
        print("Clearing column %d" % col)
        for row in range(3):
            self._set_val(row, col, None)  
        
        self._update_ma_unit()
        
    def clear_form(self):
        # For undo/redo
        old_ma_unit, old_table = self._save_ma_unit_and_table_state(
                                table = self.two_by_two_table,
                                ma_unit = self.ma_unit, 
                                use_old_value=False)
        old_prevalence = self._get_prevalence_str()
        
        keys = ["c11", "c12", "r1sum", "c21", "c22", "r2sum", "c1sum", "c2sum", "total"]
        blank_vals = dict( zip(keys, [""]*len(keys)) )

        self._set_vals(blank_vals)
        self._update_ma_unit()
        
        # clear out effects stuff
        for metric in DIAGNOSTIC_METRICS:
            self.ma_unit.set_effect_and_ci(metric, self.group_str, None, None, None, mult=self.mult)
            
        # clear line edits
        self.set_current_effect()
        self.prevalence_txt_box.blockSignals(True)
        self.prevalence_txt_box.setText("")
        self.prevalence_txt_box.blockSignals(False)

        calc_fncs.reset_table_item_flags(self.two_by_two_table)
        #self.enable_txt_box_input()
        
        new_ma_unit, new_table = self._save_ma_unit_and_table_state(
                        table = self.two_by_two_table, ma_unit = self.ma_unit,
                        use_old_value=False)
        new_prevalence = self._get_prevalence_str()
        restore_old_f = lambda: self.restore_ma_unit_and_table(old_ma_unit, old_table, old_prevalence)
        restore_new_f = lambda: self.restore_ma_unit_and_table(new_ma_unit, new_table, new_prevalence)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f, restore_old_f=restore_old_f, parent=self)
        self.undoStack.push(command)
        
    def enable_txt_box_input(self):
        ''' Enables text boxes if they are empty, disables them otherwise '''
        
        #meta_globals.enable_txt_box_input(self.effect_txt_box, self.low_txt_box,
        #                                  self.high_txt_box, self.prevalence_txt_box)
        pass
    
#    def set_clear_btn_color(self):
#        if calc_fncs._input_fields_disabled(self.two_by_two_table, self.text_boxes):
#            self.clear_Btn.setPalette(self.pushme_palette)
#        else:
#            self.clear_Btn.setPalette(self.orig_palette)
            
    def enable_back_calculation_btn(self, engage = False):
        # For undo/redo
        old_ma_unit, old_table = self._save_ma_unit_and_table_state(
                                table = self.two_by_two_table,
                                ma_unit = self.ma_unit, 
                                use_old_value=False)
        old_prevalence = self._get_prevalence_str()
        
        def build_dict():
            d = {}

            for effect in BACK_CALCULATABLE_DIAGNOSTIC_EFFECTS:    
                est,lower,upper = self.ma_unit.get_effect_and_ci(effect,
                                                                 self.group_str,
                                                                 meta_py_r.get_mult_from_r(self.global_conf_level))
                conv_to_disp_scale = lambda x: meta_py_r.diagnostic_convert_scale(x, effect, convert_to="display.scale")
                d_est,d_lower,d_upper = [conv_to_disp_scale(x) for x in [est,lower,upper]]
                for i,Rsubkey in enumerate(["",".lb",".ub"]):
                    try:
                        d["%s%s" % (effect.lower(), Rsubkey)] = float([d_est,d_lower,d_upper][i])
                    except:
                        pass
            
            x = self.getTotalSubjects()
            d["total"] = float(x) if _is_a_float(x) else None

            x = self.prevalence_txt_box.text()
            d["prev"] = float(x) if _is_a_float(x) else None

            d["conf.level"] = self.global_conf_level
    
            # now grab the raw data, if available
            d.update(self.get_raw_diag_data())
            
            return d
        
        def new_data(diag_data, imputed):
            new_data = (imputed["TP"],
                        imputed["FP"],
                        imputed["FN"],
                        imputed["TN"])
            old_data = (self._get_int(0,0),
                        self._get_int(0,1),
                        self._get_int(1,0),
                        self._get_int(1,1),
                        )
            isBlank = lambda x: x in meta_globals.EMPTY_VALS
            new_item_available = lambda old, new: isBlank(old) and not isBlank(new)
            comparison = [new_item_available(old_data[i], new_data[i]) for i in range(len(new_data))]
            print("Comparison:", comparison)
            if any(comparison):
                changed = True
            else:
                changed = False
            return changed
            
        diag_data = build_dict()
        print("Diagnostic Data for back-calculation: ", diag_data)

        #if diag_data is not None:
            
        imputed = meta_py_r.impute_diag_data(diag_data)
        print "imputed data: %s" % imputed
        
        # Leave if nothing was imputed
        if not (imputed["TP"] or imputed["TN"] or imputed["FP"] or imputed["FN"]):
            print("Nothing could be imputed")
            self.back_calc_Btn.setEnabled(False)
            return None
    
        if new_data(diag_data, imputed):
            self.back_calc_Btn.setEnabled(True)
        else:
            self.back_calc_Btn.setEnabled(False)
        #self.set_clear_btn_color()
            
        if not engage:
            return None
        ########################################################################
        # Actually do stuff with imputed data here if we are 'engaged'
        ########################################################################
        self.update_2x2_table(imputed)
        self._update_data_table()
        self._update_ma_unit()
        #self.set_clear_btn_color()
        
        # For undo/redo
        new_ma_unit, new_table = self._save_ma_unit_and_table_state(
                table = self.two_by_two_table, ma_unit = self.ma_unit,
                use_old_value=False)
        new_prevalence = self._get_prevalence_str()
        restore_old_f = lambda: self.restore_ma_unit_and_table(old_ma_unit, old_table, old_prevalence)
        restore_new_f = lambda: self.restore_ma_unit_and_table(new_ma_unit, new_table, new_prevalence)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f, restore_old_f=restore_old_f, parent=self)
        self.undoStack.push(command)
        
    ####### Undo framework ############
    def undo(self):
        print("undoing....")
        self.undoStack.undo()
        
    def redo(self):
        print("redoing....")
        self.undoStack.redo()
    #################################