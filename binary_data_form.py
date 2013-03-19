######################################
#                                    #
#  Byron C. Wallace                  #
#  George Dietz                      #     
#  CEBM @ Brown                      #     
#  OpenMeta[analyst]                 ########################## 
#  ---                                                        #
#  Binary data form module; for flexible entry of dichotomous #
#  outcome data                                               #
###############################################################

import pdb

#from PyQt4.Qt import *
from PyQt4.Qt import (pyqtSignature, QDialog, QDialogButtonBox, QMessageBox,
                      QObject, QPalette, QString, Qt, QTableWidgetItem, SIGNAL, pyqtRemoveInputHook)
#from PyQt4.QtGui import *

import meta_py_r
import meta_globals
from meta_globals import (BINARY_ONE_ARM_METRICS, BINARY_TWO_ARM_METRICS,
                          _is_a_float, _is_empty, EMPTY_VALS)

import ui_binary_data_form
import ui_choose_bin_back_calc_result_form
#from ui_binary_data_form import Ui_BinaryDataForm

# @TODO this should be an *application global*. It is now a
# global here and in the data_table_view class. (However
# here we show four digits; there it is 3. We want different
# levels of granularity).
NUM_DIGITS = 4 


# this is the maximum size of a residual that we're willing to accept
# when computing 2x2 data
THRESHOLD = 1e-5



class BinaryDataForm2(QDialog, ui_binary_data_form.Ui_BinaryDataForm):
    def __init__(self, ma_unit, cur_txs, cur_group_str, cur_effect, parent=None):
        super(BinaryDataForm2, self).__init__(parent)
        self.setupUi(self)
        self._setup_signals_and_slots()
        self.ma_unit = ma_unit
        self.raw_data_d = {}
        for group in cur_txs:
            raw_data = self.ma_unit.get_raw_data_for_group(group)
            self.raw_data_d[group]  = raw_data
        self.cur_groups = cur_txs
        self.group_str = cur_group_str
        self.cur_effect = cur_effect
        self.entry_widgets = [self.raw_data_table, self.low_txt_box,
                              self.high_txt_box, self.effect_txt_box]
        
        self.setup_inconsistency_checking()
        self.initialize_backup_structures()
        #self.setup_table_effect_dict()
        self._update_raw_data()      # ma_unit --> table
        self._populate_effect_data() # make combo boxes for effects
        self.set_current_effect()    # fill in current effect data in line edits
        self._update_data_table()    # fill in 2x2
        self.enable_back_calculation_btn()
        self.save_form_state()

        ### TEMPORARILY HIDE FOR PRESENTATION:
        #self.pval_lbl.setVisible(False)
        #self.effect_p_txt_box.setVisible(False)
        
    def print_effects_dict_from_ma_unit(self):
        print self.ma_unit.effects_dict
    
    def enable_back_calculation_btn(self, engage = False):
        print("Enabling back-calculation button...")

        def build_back_calc_args_dict():
            
            effect = self.cur_effect
            d = {}
            
            d["metric"] = str(effect)
            
            for key,R_key in zip(["est","lower","upper"],["estimate","lower","upper"]):
                try:
                    d["%s" %  R_key] = float(self.form_effects_dict[effect][key])
                except:
                    d["%s" %  R_key] = None
            
            # TODO: Make this depend on value displayed for alpha on form
            alpha_dummy = 0.05
            d["conf.level"] = (1.0-float(alpha_dummy))*100
            
            d["N_A"] = float(self._get_int(0,2)) if not self._is_empty(0,2) else None
            d["N_B"] = float(self._get_int(1,2)) if not self._is_empty(1,2) else None
            
            return d
        
        # Makes no sense to show the button on a form where the back calculation is not implemented
        if not self.cur_effect in ["OR","RR","RD"]:
            self.back_calc_btn.setVisible(False)
        else:
            self.back_calc_btn.setVisible(True)
            
        bin_data = build_back_calc_args_dict()
        print("Binary data for back_calculation:",bin_data)
        
        imputed = meta_py_r.impute_bin_data(bin_data)
        print("Imputed data: %s", imputed)
        
        # Leave if nothing was imputed
        if "FAIL" in imputed:
            print("Fail to impute")
            self.back_calc_btn.setEnabled(False)
            return None
        
        print("Got beyond the failure, button should be enabled")
        self.back_calc_btn.setEnabled(True)
        
        if not engage:
            return None
        ########################################################################
        # Actually do stuff with imputed data here if we are 'engaged'
        ########################################################################
        for x in range(3):
            self.clear_column(x) # clear out the table

        if len(imputed.keys()) > 1:
            dialog = ChooseBackCalcResultForm(imputed, parent=self)
            if dialog.exec_():
                choice = dialog.getChoice()
            else: # don't do anything if cancelled
                return None
        else: # only one option
            choice = "op1"
            
            
        # set values in table & save in ma_unit
        self.raw_data_table.blockSignals(True)
        self._set_val(0, 0, int(round(imputed[choice]["a"]))  )
        self._set_val(0, 2, int(round(imputed[choice]["b"]))  )  
        self._set_val(1, 0, int(round(imputed[choice]["c"]))  ) 
        self._set_val(1, 2, int(round(imputed[choice]["d"]))  ) 
        self.raw_data_table.blockSignals(False)
        
        self._update_data_table()
        self._update_ma_unit() # save in ma_unit
        self.save_form_state()

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
        
        self.check_table_consistency = meta_globals.ConsistencyChecker(
                            fn_consistent=action_consistent_table,
                            fn_inconsistent=action_inconsistent_table,
                            table_2x2 = self.raw_data_table)

    def initialize_backup_structures(self):
        # Stores form effect info as text
        self.form_effects_dict = {}
        #self.form_effects_dict["alpha"] = ""
        for effect in self.get_effect_names():
            self.form_effects_dict[effect] = {"est":"","lower":"","upper":""}
        
        # Stores table items as text
        self.table_backup = [[None,None,None],[None,None,None],[None,None,None]]
        
    @pyqtSignature("int, int, int, int")
    def on_raw_data_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
        self.current_item_data = self._get_int(currentRow,currentColumn)
        print "Current Item Data:",self.current_item_data
        
    def _setup_signals_and_slots(self):
        QObject.connect(self.raw_data_table, SIGNAL("cellChanged (int, int)"), 
                                                    self.cell_changed)
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                                    self.effect_changed) 
        
        QObject.connect(self.effect_txt_box, SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("est", new_text))
        QObject.connect(self.low_txt_box,    SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("lower", new_text))
        QObject.connect(self.high_txt_box,   SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("upper", new_text))
        
        QObject.connect(self.effect_txt_box, SIGNAL("editingFinished()"), lambda: self.val_changed("est")   )
        QObject.connect(self.low_txt_box,    SIGNAL("editingFinished()"), lambda: self.val_changed("lower") )
        QObject.connect(self.high_txt_box,   SIGNAL("editingFinished()"), lambda: self.val_changed("upper") )
        
        QObject.connect(self.back_calc_btn, SIGNAL("clicked()"), lambda: self.enable_back_calculation_btn(engage=True) )
                                                                                                                               
    def _populate_effect_data(self):
        q_effects = sorted([QString(effect_str) for effect_str in self.ma_unit.effects_dict.keys()])
        self.effect_cbo_box.blockSignals(True)
        self.effect_cbo_box.addItems(q_effects)
        self.effect_cbo_box.blockSignals(False)
        self.effect_cbo_box.setCurrentIndex(q_effects.index(QString(self.cur_effect)))
        
    def get_effect_names(self):
        effects = self.ma_unit.effects_dict.keys()
        return effects
    
    def set_current_effect(self):
        '''Populates text boxes with effects (computed values) from ma unit'''
        
        print("Setting current effect...")
        
        effect_dict = self.ma_unit.effects_dict[self.cur_effect][self.group_str]
        for s, txt_box in zip(['display_est', 'display_lower', 'display_upper'], \
                              [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            if effect_dict[s] is not None:
                txt_box.setText(QString("%s" % round(effect_dict[s], NUM_DIGITS)))
            else:
                txt_box.setText(QString(""))
            
    def effect_changed(self):
        '''Called when a new effect is selected in the combo box'''
        self.cur_effect = unicode(self.effect_cbo_box.currentText().toUtf8(), "utf-8")
        self.try_to_update_cur_outcome()
        self.set_current_effect()
        
        self.enable_back_calculation_btn()
        
    def save_form_state(self):
        ''' Saves the state of all objects on the form '''
        
        print("Saving form state...")
        
        def save_table_data():
            for row in range(3):
                for col in range(3):
                    item = self.raw_data_table.item(row, col)
                    contents = "" if item is None else item.text()
                    self.table_backup[row][col]=contents
                    
        def save_displayed_effects_data(effect=None):
            print "Saving Displayed Effects data...."
            
            if effect is None:
                effect = self.cur_effect
            
            self.form_effects_dict[effect]["est"]   = self.effect_txt_box.text() 
            self.form_effects_dict[effect]["lower"] = self.low_txt_box.text()    
            self.form_effects_dict[effect]["upper"] = self.high_txt_box.text()    
            #self.form_effects_dict["alpha"]      = self.alpha_edit.text() 
        
            self.candidate_est        = self.effect_txt_box.text()
            self.candidate_lower      = self.low_txt_box.text()
            self.candidate_upper      = self.high_txt_box.text()
            #self.candidate_alpha      = self.alpha_edit.text()

        save_table_data()
        save_displayed_effects_data()
        self.enable_back_calculation_btn()
    
    def block_all_signals(self, state):
        for widget in self.entry_widgets:
            widget.blockSignals(state)
        
    def restore_form_state(self):
        ''' Restores the state of all objects on the form '''
        
        # Block all signals on the form 
        self.block_all_signals(True)
        ########################################################################
        
        def restore_displayed_effects_data():
            print "Restoring displayed effects data..."
            
            self.effect_txt_box.setText(    self.form_effects_dict[self.cur_effect]["est"]  )    
            self.low_txt_box.setText(       self.form_effects_dict[self.cur_effect]["lower"])       
            self.high_txt_box.setText(      self.form_effects_dict[self.cur_effect]["upper"])              
            #self.alpha_edit.setText(        self.form_effects_dict["alpha"]                 )        
            
            self.candidate_est        = self.effect_txt_box.text()
            self.candidate_lower      = self.low_txt_box.text()
            self.candidate_upper      = self.high_txt_box.text()
            #self.candidate_alpha      = self.alpha_edit.text()
        
        def restore_table():
            #print "Table to restore:"
            #self.print_backup_table()
        
            for row in range(3):
                for col in range(3):
                    self.raw_data_table.blockSignals(True)
                    self._set_val(row, col, self.table_backup[row][col])
                    self.raw_data_table.blockSignals(False)
            self.check_table_consistency.run()
            
            #print("Backed-up table:")
            #self.print_backup_table()
        
        restore_displayed_effects_data()
        restore_table()
        self.enable_back_calculation_btn()
        
        
        ########################################################################
        # Unblock the signals
        self.block_all_signals(False)
        
    def val_changed(self, val_str):        
        print "--------------\nEntering val_changed...."
        
        def is_between_bounds(est=self.form_effects_dict[self.cur_effect]["est"], 
                              low=self.form_effects_dict[self.cur_effect]["lower"], 
                              high=self.form_effects_dict[self.cur_effect]["upper"]):
            return meta_globals.between_bounds(est=est, low=low, high=high)

        ###### ERROR CHECKING CODE#####
        # Make sure entered value is numeric and between the appropriate bounds
        self.block_all_signals(True)
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
#            elif val_str == "alpha" and not _is_empty(self.candidate_alpha):
#                if not _is_a_float(self.candidate_alpha):
#                    QMessageBox.warning(self.parent(), "whoops", float_msg)
#                    raise Exception("error")
#                if _is_a_float(self.candidate_alpha) and not 0 < float(self.candidate_alpha) < 1:
#                    QMessageBox.warning(self.parent(), "whoops", "Alpha must be between 0 and 1 (closer to zero please!")
#                    raise Exception("error")
        except:
            print "Error flag is true"
            self.restore_form_state()
            self.block_all_signals(True)
            if val_str == "est":
                self.effect_txt_box.setFocus()
            elif val_str == "lower":
                self.low_txt_box.setFocus()
            elif val_str == "upper":
                self.high_txt_box.setFocus()
            #elif val_str == "alpha":
            #    self.alpha_edit.setFocus()
            self.block_all_signals(False)
            return


            
        self.block_all_signals(False)
        # If we got to this point it means everything is ok so far
        
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
            
    # Todo: Impute 2x2 from here if est,low,high all filled out
        self.save_form_state()
        self.enable_back_calculation_btn()
    
    def val_edit(self, val_str, display_scale_val):
        #print "Editing %s with value: %s" % (val_str,display_scale_val)
        if val_str == "est":
            self.candidate_est = display_scale_val
        if val_str == "lower":
            self.candidate_lower = display_scale_val
        if val_str == "upper":
            self.candidate_upper = display_scale_val
        
    def _update_raw_data(self):
        ''' Generates the 2x2 table with whatever parametric data was provided '''
        ''' Sets #events and #subjects in binary table'''
        self.raw_data_table.blockSignals(True)
        for row, group in enumerate(self.cur_groups):
            for col in (0,2):
                adjusted_index = 0 if col==0 else 1
                val = self.raw_data_d[group][adjusted_index]
                if val is not None:
                    try:
                        val = str(int(val))
                    except:
                        val = str(val)
                    item = QTableWidgetItem(val)
                    self.raw_data_table.setItem(row, col, item)
        self.raw_data_table.blockSignals(False)
      
    def _update_ma_unit(self):
        ''' Copy data from binary data table to the MA_unit'''
        ''' 
        Walk over the entries in the matrix (which may have been updated
        via imputation in the cell_changed method) corresponding to the 
        raw data in the underlying meta-analytic unit and update the values.
        '''
        for row in range(2):
            for col in (0,2):
                adjusted_col = 1 if col==2 else 0
                self.raw_data_d[self.cur_groups[row]][adjusted_col] = self._get_int(row, col)
                print "%s, %s: %s" % (row, col, self._get_int(row, col))
        print "ok -- raw data is now: %s" % self.raw_data_d
        
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
        
    def cell_changed(self, row, col):
        # tries to make sense of user input before passing
        # on to the R routine
        
        print("Entering cell changed...")
        print("New cell data(%d,%d): %s" % (row,col, self.raw_data_table.item(row, col).text()))   
        
        try:
            # Test if entered data is valid (a number)
            warning_msg = self._cell_data_not_valid(self.raw_data_table.item(row, col).text())
            if warning_msg:
                raise Exception("Invalid Cell Data")
    
            self._update_data_table() # calculate rest of table (provisionally) based on new entry
            warning_msg = self.check_table_consistency.run()
            if warning_msg:
                raise Exception("Table no longer consistent.")
        except Exception as e:
            msg = e.args[0]
            QMessageBox.warning(self.parent(), "whoops", msg) #popup warning
            self.restore_form_state() # brings things back to the way they were
            return                    # and leave
        
        self.save_form_state()
        # need to try and update metric here     
        self._update_ma_unit() # table widget --> ma_unit
        self.try_to_update_cur_outcome()
        self.enable_back_calculation_btn()
        self.save_form_state()
        
        # disable just-edited cell
        self.block_all_signals(True)
        item = self.raw_data_table.item(row, col)
        newflags = item.flags() & ~Qt.ItemIsEditable
        item.setFlags(newflags)
        self.block_all_signals(False)
        
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
    
    def clear_column(self,col):
        '''Clears out column in table and ma_unit'''

        for row in range(3):
            self.raw_data_table.blockSignals(True)
            self._set_val(row, col, None)  
            self.raw_data_table.blockSignals(False)
        
        self._update_ma_unit()
        self.save_form_state()
         
    def _set_vals(self, computed_d):
        '''Sets values in table widget'''
        self.raw_data_table.blockSignals(True)
        self._set_val(0, 0, computed_d["c11"])
        self._set_val(0, 1, computed_d["c12"])
        self._set_val(1, 0, computed_d["c21"])
        self._set_val(1, 1, computed_d["c22"])  
        self._set_val(0, 2, computed_d["r1sum"])
        self._set_val(1, 2, computed_d["r2sum"])
        self._set_val(2, 0, computed_d["c1sum"])
        self._set_val(2, 1, computed_d["c2sum"])  
        self._set_val(2, 2, computed_d["total"])  
        self.raw_data_table.blockSignals(False)  
        
    def _set_val(self, row, col, val):
        if meta_globals.is_NaN(val): # get out quick
            print "%s is not a number" % val
            return
        
        try:
            str_val = "" if val in EMPTY_VALS else str(int(val))
            if self.raw_data_table.item(row, col) == None:
                self.raw_data_table.setItem(row, col, QTableWidgetItem(str_val))
            else:
                self.raw_data_table.item(row, col).setText(str_val)
            
            #disable item
            if str_val != "": 
                item = self.raw_data_table.item(row, col)
                newflags = item.flags() & ~Qt.ItemIsEditable
                item.setFlags(newflags)
        except:
            print("Got to except in _set_val when trying to set (%d,%d)" % (row,col))
            raise
 
    def _build_dict(self):
        d =  dict(zip(["control.n.outcome", "control.N", "tx.n.outcome", "tx.N"], self.raw_data))
        print "\n!%s" % self.ma_unit.effects_dict[self.cur_effect]
        d["estimate"] = self.ma_unit.effects_dict[self.cur_effect][self.group_str]['est']
        print d["estimate"] == ""
        print d["estimate"] is None
        return d
        
    def _update_data_table(self):        
        '''Fill in 2x2 table from other entries in the table '''
        
        self.raw_data_table.blockSignals(True)
        
        params = self._get_table_vals()
        computed_params = meta_globals.compute_2x2_table(params)
        print "Computed Params", computed_params
        if computed_params:
            self._set_vals(computed_params) # computed --> table widget
            
        self.raw_data_table.blockSignals(False)
        
    def _is_empty(self, i, j):
        val = self.raw_data_table.item(i,j)
        return val is None or val.text() == ""
        
    def _get_int(self, i, j):
        '''Get value from cell specified by row=i, col=j as an integer'''
        if not self._is_empty(i,j):
            val = int(float(self.raw_data_table.item(i, j).text()))
            print("Val from _get_int: %d" % val)
            return val
        else:
            return None # its good to be explicit
            
    def _none_or_empty(self, x):
        return x is None or x == ""
        
    def try_to_update_cur_outcome(self):
        e1, n1, e2, n2 = self.ma_unit.get_raw_data_for_groups(self.cur_groups)
        # if None is in the raw data, should we clear out current outcome?
        if not (any([self._none_or_empty(x) for x in [e1, n1, e2, n2]]) or
                   (not any([self._none_or_empty(x) for x in [e1, n1]]) and self.cur_effect in BINARY_ONE_ARM_METRICS)):
            if self.cur_effect in BINARY_TWO_ARM_METRICS:
                est_and_ci_d = meta_py_r.effect_for_study(e1, n1, e2, n2, metric=self.cur_effect)
            else:
                # binary, one-arm
                est_and_ci_d = meta_py_r.effect_for_study(e1, n1, two_arm=False, metric=self.cur_effect)
        
            display_est, display_low, display_high = est_and_ci_d["display_scale"]
            self.ma_unit.set_display_effect_and_ci(self.cur_effect, self.group_str, display_est, display_low, display_high)                            
            est, low, high = est_and_ci_d["calc_scale"] # calculation (e.g., log) scale
            self.ma_unit.set_effect_and_ci(self.cur_effect, self.group_str, est, low, high)
            self.set_current_effect()
           
#    def start_over(self):
#        keys = ["c11", "c12", "r1sum", "c21", "c22", "r2sum", "c1sum", "c2sum", "total"]
#        blank_vals = dict( zip(keys, [""]*len(keys)) )
#
#        self._set_vals(blank_vals)
#        self._update_ma_unit()
#        
#        # clear out effects stuff
#        for metric in BINARY_ONE_ARM_METRICS + BINARY_TWO_ARM_METRICS:
#            if ((self.cur_effect in BINARY_TWO_ARM_METRICS and metric in BINARY_TWO_ARM_METRICS) or
#                (self.cur_effect in BINARY_ONE_ARM_METRICS and metric in BINARY_ONE_ARM_METRICS)):
#                self.ma_unit.set_effect_and_ci(metric, self.group_str, None, None, None)
#                self.ma_unit.set_display_effect_and_ci(metric, self.group_str, None, None, None)
#            else:
#                #TODO
#                # do nothing for now..... treat the case where we have to switch group strings down the line
#                pass
#            
#        # clear line edits
#        self.set_current_effect()

class ChooseBackCalcResultForm(QDialog, ui_choose_bin_back_calc_result_form.Ui_ChooseBackCalcResultForm):
    def __init__(self, imputed_data, parent=None):
        super(ChooseBackCalcResultForm, self).__init__(parent)
        self.setupUi(self)
        
        op1 = imputed_data["op1"] # option 1 data
        a,b,c,d = op1["a"],op1["b"],op1["c"],op1["d"]
        a,b,c,d = int(round(a)),int(round(b)),int(round(c)),int(round(d))
        option1_txt = "Group 1:\n  #events: %d\n  Total: %d\nGroup 2:\n  #events: %d\n  Total: %d" % (a,b,c,d)
        
        try:
            op2 = imputed_data["op2"]
            a,b,c,d = op2["a"],op2["b"],op2["c"],op2["d"]
            a,b,c,d = int(round(a)),int(round(b)),int(round(c)),int(round(d))
            option2_txt = "Group 1:\n  #events: %d\n  Total: %d\nGroup 2:\n  #events: %d\n  Total: %d" % (a,b,c,d)
        except:
            pyqtRemoveInputHook()
            pdb.set_trace()
        

        self.choice1_lbl.setText(option1_txt)
        self.choice2_lbl.setText(option2_txt)

    def getChoice(self):
        choices = ["op1", "op2"]
        
        if self.choice1_btn.isChecked():
            return choices[0] # op1
        else:
            return choices[1] # op2