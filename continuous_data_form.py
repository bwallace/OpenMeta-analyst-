##################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Continuous data form module; for flexible entry of continuous
#  outcome data.
#
# TODO there is some redundancy here with binary_data_form
#      should likely refactor
# 
# Note that we don't make use of the table/custom model
# design here. Rather, we edit the ma_unit object
# directly, based on what the user inputs. This seemed a more
# straightforward approach, because the table itself displays
# many fields that do not ultimately belong in the raw_data --
# it's mostly imputation going on here.
#
##################################################

import pdb

from PyQt4.Qt import *
from PyQt4 import QtGui

import meta_py_r
import meta_globals
#from meta_globals import *
from meta_globals import CONTINUOUS_ONE_ARM_METRICS,CONTINUOUS_TWO_ARM_METRICS, _is_a_float,_is_empty
import ui_continuous_data_form
#from ui_continuous_data_form import Ui_ContinuousDataForm

# @TODO this should be an *application global*. It is now a
# global here and in the data_table_view class. (However
# here we show four digits; there it is 3. We want different
# levels of granularity).
NUM_DIGITS = 4 
default_col_width = 65

# because the output from R is a string ("TRUE"/"FALSE")
_is_true = lambda x: x == "TRUE"

class ContinuousDataForm(QDialog, ui_continuous_data_form.Ui_ContinuousDataForm):
    def __init__(self, ma_unit, cur_txs, cur_group_str, cur_effect, parent=None):
        super(ContinuousDataForm, self).__init__(parent)
        self.setupUi(self)
        self.setup_signals_and_slots()
        self.ma_unit = ma_unit
        self.raw_data_dict = {}
        for group in cur_txs:
            raw_data = self.ma_unit.get_raw_data_for_group(group)
            self.raw_data_dict[group] = raw_data
        self.cur_groups = cur_txs
        self.cur_effect = cur_effect
        self.group_str = cur_group_str
        
        self.entry_widgets = [self.simple_table, self.g1_pre_post_table,
                              self.g2_pre_post_table, self.effect_txt_box,
                              self.low_txt_box, self.high_txt_box,
                              self.correlation_pre_post]
        
        self.CI_spinbox.setValue(meta_globals.DEFAULT_CONF_LEVEL)
        self.ci_label.setText("{0:.1f}% Confidence Interval".format(self.CI_spinbox.value()))
        
        
        # Set the table headers to reflect the group names
        groups_names = QStringList(self.cur_groups)
        self.simple_table.setVerticalHeaderLabels(groups_names)
        
        for table in (self.simple_table, self.g1_pre_post_table, self.g2_pre_post_table):
            self._set_col_widths(table)
            
        self.grp_1_lbl.setText(QString(self.cur_groups[0]))
        self.grp_2_lbl.setText(QString(self.cur_groups[1]))
        
        self.initialize_backup_structures()
        # Color for clear_button_pallette
        self.setup_clear_button_palettes()
        self._populate_effect_data()
        self.update_raw_data()
        self.enable_back_calculation_btn()
        self.save_form_state()

    def setup_signals_and_slots(self):
        QObject.connect(self.simple_table, SIGNAL("cellChanged (int, int)"), self._cell_changed)
        QObject.connect(self.g1_pre_post_table, SIGNAL("cellChanged (int, int)"), lambda: self.impute_pre_post_data(self.g1_pre_post_table, 0))
        QObject.connect(self.g2_pre_post_table, SIGNAL("cellChanged (int, int)"), lambda: self.impute_pre_post_data(self.g2_pre_post_table, 1))
        
        QObject.connect(self.CI_spinbox, SIGNAL("valueChanged(double)"), self._change_ci) 
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"), self.effect_changed)
        QObject.connect(self.clear_Btn, SIGNAL("clicked()"), self.clear_form)
                                                                                
        QObject.connect(self.effect_txt_box, SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("est", new_text))
        QObject.connect(self.low_txt_box,    SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("lower", new_text))
        QObject.connect(self.high_txt_box,   SIGNAL("textEdited(QString)"), lambda new_text : self.val_edit("upper", new_text))
        QObject.connect(self.correlation_pre_post, SIGNAL("textEdited(QString)"), lambda new_text: self.val_edit("correlation_pre_post", new_text))
        
        QObject.connect(self.effect_txt_box, SIGNAL("editingFinished()"), lambda: self.val_changed("est")   )
        QObject.connect(self.low_txt_box,    SIGNAL("editingFinished()"), lambda: self.val_changed("lower") )
        QObject.connect(self.high_txt_box,   SIGNAL("editingFinished()"), lambda: self.val_changed("upper") )
        QObject.connect(self.correlation_pre_post, SIGNAL("editingFinished()"), lambda: self.val_changed("correlation_pre_post") )
        
                                                                                
    def _set_col_widths(self, table):
        for column in range(table.columnCount()):
            table.setColumnWidth(column, default_col_width)
    
    def _change_ci(self,val):
        self.ci_label.setText("{0:.1F} % Confidence Interval".format(val))
        print("New CI val:",val)
        
        self.enable_back_calculation_btn()
        
    def _populate_effect_data(self):
        q_effects = sorted([QString(effect_str) for \
                             effect_str in self.ma_unit.effects_dict.keys()])
        self.effect_cbo_box.blockSignals(True)
        self.effect_cbo_box.addItems(q_effects)
        self.effect_cbo_box.blockSignals(False)
        self.effect_cbo_box.setCurrentIndex(q_effects.index(QString(self.cur_effect)))
        # populate fields with current effect data
        self.set_current_effect()
        
    def effect_changed(self):
        self.cur_effect = unicode(self.effect_cbo_box.currentText().toUtf8(), "utf-8")
        self.try_to_update_cur_outcome()
        self.set_current_effect()
        
        self.enable_txt_box_input()
        self.enable_back_calculation_btn()
      
    def val_changed(self, val_str):
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
            elif val_str == "correlation_pre_post" and not _is_empty(self.candidate_correlation_pre_post):
                if not _is_a_float(self.candidate_correlation_pre_post):
                    QMessageBox.warning(self.parent(), "whoops", float_msg)
                    raise Exception("error")
                if _is_a_float(self.candidate_correlation_pre_post) and not -1 <= float(self.candidate_correlation_pre_post) <= 1:
                    QMessageBox.warning(self.parent(), "whoops", "Correlation must be between -1 and +1")
                    raise Exception("error")
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
            elif val_str == "correlation_pre_post":
                self.correlation_pre_post.setFocus()
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
        elif val_str == "upper":
            self.ma_unit.set_upper(self.cur_effect, self.group_str, calc_scale_val)
            self.ma_unit.set_display_upper(self.cur_effect, self.group_str, display_scale_val)
        elif val_str == "correlation_pre_post":
            print "ok -- correlation set to %s" % self.correlation_pre_post.text()
            # Recompute the estimates
            self.impute_pre_post_data(self.g1_pre_post_table, 0)
            self.impute_pre_post_data(self.g2_pre_post_table, 1)
            
        self.enable_txt_box_input()
        self.save_form_state()
        self.enable_back_calculation_btn()
    
    def val_edit(self, val_str, display_scale_val):
        print "Editing %s with value: %s" % (val_str,display_scale_val)
        if val_str == "est":
            self.candidate_est = display_scale_val
        if val_str == "lower":
            self.candidate_lower = display_scale_val
        if val_str == "upper":
            self.candidate_upper = display_scale_val
        if val_str == "correlation_pre_post":
            self.candidate_correlation_pre_post = display_scale_val

    def setup_clear_button_palettes(self):
        # Color for clear_button_pallette
        self.orig_palette = self.clear_Btn.palette()
        self.pushme_palette = QPalette()
        self.pushme_palette.setColor(QPalette.ButtonText,Qt.red)
        self.set_clear_btn_color()
        
    def set_clear_btn_color(self):
        if self.input_fields_disabled():
            self.clear_Btn.setPalette(self.pushme_palette)
        else:
            self.clear_Btn.setPalette(self.orig_palette)
            
    def input_fields_disabled(self):
        table_disabled = True
        for row in range(3):
            for col in range(3):
                item = self.simple_table.item(row, col)
                if item is None:
                    continue
                if (item.flags() & Qt.ItemIsEditable) == Qt.ItemIsEditable:
                    table_disabled = False
                    
        txt_boxes_disabled = self._txt_boxes_disabled()

        if table_disabled and txt_boxes_disabled:
            self.CI_spinbox.setEnabled(False) # weird place for ?this? but whatever
            return True
        return False
    
    def _txt_boxes_disabled(self):
        return not (self.effect_txt_box.isEnabled() or
                    self.low_txt_box.isEnabled() or
                    self.high_txt_box.isEnabled()) # or
                    #self.correlation_pre_post.isEnabled())

    def set_current_effect(self):
        effect_dict = self.ma_unit.effects_dict[self.cur_effect][self.group_str]
        for s, txt_box in zip(['display_est', 'display_lower', 'display_upper'], \
                              [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            if effect_dict[s] is not None:
                txt_box.setText(QString("%s" % round(effect_dict[s], NUM_DIGITS)))
            else:
                txt_box.setText(QString(""))
        
    def update_raw_data(self):
        '''Updates table widget with data from ma_unit'''
        self.simple_table.blockSignals(True)
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.raw_data_dict[group_name]
            for col in range(len(grp_raw_data)):
                if grp_raw_data[col] is not None:
                    val = QTableWidgetItem(str(grp_raw_data[col]))
                    self.simple_table.setItem(row_index, col, val)
            # also insert the SEs, if we have them
            se_col = 3
            se = self.ma_unit.effects_dict[self.cur_effect][self.group_str]["SE"]
            if se is not None:
                se_item = QTableWidgetItem(str(se))
                self.simple_table.setItem(row_index, se_col, se_item)
                
        self.impute_data()
        self.simple_table.blockSignals(False)
        
    def _cell_data_not_valid(self, celldata_string, cell_header=None):
        # ignore blank entries
        if celldata_string.trimmed() == "" or celldata_string is None:
            return None

        if not meta_globals._is_a_float(celldata_string):
            return "Raw data needs to be numeric."

        if cell_header in ['n','sd','se','var','pval'] and float(celldata_string) < 0:
            return "%s cannot be negative." % (cell_header,)
        
        if cell_header == 'pval' and not (0 <= float(celldata_string) <= 1):
            return "pval must be between 0 and 1"
        return None
          
    def _cell_changed(self, row, col):
        # Just for simple_table for now
        
        print("CELL CHANGED: (ROW,COL)=(%d,%d)" % (row,col))
        print "previous cell data:",self.current_item_data
        print "new cell data:", self.simple_table.item(row, col).text()
        
        column_headers = self.get_column_header_strs()
        
        try:
            column_headers = self.get_column_header_strs()
            warning_msg = self._cell_data_not_valid(self.simple_table.item(row, col).text(),column_headers[col])
            if warning_msg:
                raise Exception("Invalid Cell Data")
            
            self.impute_data()
        except Exception as e:
            msg = e.args[0]
            QMessageBox.warning(self.parent(), "whoops", msg)
            self.restore_form_state()
            return
        
        self.save_form_state()
        self._update_ma_unit() # table --> ma_unit
        self.try_to_update_cur_outcome()
        self.enable_back_calculation_btn()
        self.save_form_state()
        
        # disable just-edited cell
        self.block_all_signals(True)
        item = self.simple_table.item(row, col)
        newflags = item.flags() & ~Qt.ItemIsEditable
        item.setFlags(newflags)
        self.block_all_signals(False)
        
        self.enable_txt_box_input() # if the effect was imputed
        self.set_clear_btn_color()
    
    def _set_val(self, row_index, var_index, val):
        row,col = row_index, var_index    
        if meta_globals.is_NaN(val): # get out quick
            print "%s is not a number" % val
            return
        
        try:
            str_val = "" if val in meta_globals.EMPTY_VALS else str(float(val))
            if self.simple_table.item(row, col) == None:
                self.simple_table.setItem(row, col, QTableWidgetItem(str_val))
            else:
                self.simple_table.item(row, col).setText(str_val)
            
            if str_val != "": #disable item
                #self.block_all_signals(True)
                item = self.simple_table.item(row, col)
                newflags = item.flags() & ~Qt.ItemIsEditable
                item.setFlags(newflags)
                #self.block_all_signals(False)
        except:
            print("Got to except in _set_val when trying to set (%d,%d)" % (row,col))
    
    def _update_ma_unit(self):
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.raw_data_dict[group_name]
            for col_index in range(len(grp_raw_data)):
                cur_val = self._get_float(row_index, col_index)
                self.raw_data_dict[group_name][col_index] = cur_val

            ## also check if SEs have been entered directly
            ##se_index = 3
            ##self.ma_unit.effects_dict[self.cur_effect][self.group_str]["SE"] = self._get_float(row_index, se_index)
        
    def impute_data(self):
        ''' compute what we can for each study from what has been given in the table'''
        
        # note that we rely on the variable names corresponding to what
        # the meta_py_r routine expects.
        var_names = self.get_column_header_strs()
        print "current groups: ", self.cur_groups #######################
        print "----------------------------"      #######################
        for row_index, group_name in enumerate(self.cur_groups):
            print "Group:",group_name             #######################
            print "--------"                      #######################
            # assemble the fields in a dictionary; pass off to meta_py_r
            cur_dict = {}
            for var_index, var_name in enumerate(var_names):
                var_value = self._get_float(row_index, var_index)
                if var_value is not None:
                    cur_dict[var_name] = var_value

            # now pass off what we have for this study to the
            # imputation routine
            #results_from_r = meta_py_r.impute_cont_data(cur_dict, self.alpha)
            results_from_r = meta_py_r.impute_cont_data(cur_dict, self.conf_level_to_alpha())

            print "Raw results from R (imputation): %s" % results_from_r
            print results_from_r

            print "Results from r succeeded?:", results_from_r["succeeded"]
            if results_from_r["succeeded"]:
                computed_vals = results_from_r["output"]
                # and then iterate over the columns again, 
                # populating the table with any available
                # computed fields
            
                self.simple_table.blockSignals(True)
                
                print "Computed vals:",computed_vals
                for var_index, var_name in enumerate(var_names):  
                    float_str = self.float_to_str(float(computed_vals[var_name]))
                    #self.simple_table.setItem(row_index, var_index, QTableWidgetItem(QString(float_str)))
                    self._set_val(row_index, var_index, float_str)

                    # update the raw data for N, mean and SD fields (this is all that is actually stored)
                    if var_index < 3:
                        #self.ma_unit.tx_groups[group_name].raw_data[var_index] = computed_vals[var_name]
                        self.raw_data_dict[group_name][var_index] = computed_vals[var_name]
                self._update_ma_unit()
                self.simple_table.blockSignals(False)
                
    def conf_level_to_alpha(self):
        alpha = 1-self.CI_spinbox.value()/100.0
        return alpha
           
    def impute_pre_post_data(self, table, group_index):
        ''' 
        The row index corresponds to the group that will be
        affected by the data edits. E.g., a row index of 0 will result
        in the data for the first group (row 0 in the simple_table)
        being modified.
        '''
        group_name = self.cur_groups[group_index]
        var_names = self.get_column_header_strs_pre_post()
        params_dict = {}
        # A, B correspond to pre, post
        for a_b_index, a_b_name in enumerate(["A", "B"]):
            # assemble the fields in a dictionary; pass off to meta_py_r
            for var_index, var_name in enumerate(var_names):
                var_value = self._get_float(a_b_index, var_index, table)
                if var_value is not None:
                    params_dict["%s.%s" % (var_name, a_b_name)] = var_value

        # now pass off what we have for this study to the
        # imputation routine
        results_from_r = meta_py_r.impute_pre_post_cont_data(params_dict,
                                        float(self.correlation_pre_post.text()),
                                        self.conf_level_to_alpha())
 
        print "imputation results from R: %s" % results_from_r

        if _is_true(results_from_r["succeeded"]):
            ### 
            # first update the simple table
            computed_vals = results_from_r["output"]
            self.simple_table.blockSignals(True)
            for var_index, var_name in enumerate(self.get_column_header_strs()):
                float_str = self.float_to_str(float(computed_vals[var_name]))

                #self.simple_table.setItem(group_index, var_index, QTableWidgetItem(QString(float_str)))
                self._set_val(group_index, var_index, float_str)

                # update the raw data for N, mean and SD fields (this is all that is actually stored)
                if var_index < 3:
                    #self.ma_unit.tx_groups[group_name].raw_data[var_index] = computed_vals[var_name]
                    self.raw_data_dict[group_name][var_index] = computed_vals[var_name]
            
            self.try_to_update_cur_outcome()        
            self.simple_table.blockSignals(False)
            
            ###
            # also update the pre/post tables
            pre_vals = results_from_r["pre"]
            post_vals = results_from_r["post"]
            table.blockSignals(True)
            for var_index, var_name in enumerate(var_names):
                pre_val = self.float_to_str(float(pre_vals[var_name]))
                post_val = self.float_to_str(float(post_vals[var_name]))
                table.setItem(0, var_index, QTableWidgetItem(QString(pre_val)))
                table.setItem(1, var_index, QTableWidgetItem(QString(post_val)))
            table.blockSignals(False)
            
            
    def float_to_str(self, float_val):
        float_str = "NA"
        if not meta_globals.is_NaN(float_val):
            # TODO note the hard-coded number of digits here
            float_str = str(round(float_val, 4))
        return float_str     
                    
    def get_column_header_strs(self, table=None):
        if table is None:
            table = self.simple_table

        return [str(h_item.text()) for h_item in \
                                [table.horizontalHeaderItem(col) for col in \
                                    range(table.columnCount())]]
        
    def get_column_header_strs_pre_post(self):
        return self.get_column_header_strs(table=self.g1_pre_post_table)
    
    @pyqtSignature("int, int, int, int")
    def on_simple_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
        self.current_item_data = self._get_float(currentRow,currentColumn)
        print "Current Item Data:",self.current_item_data
        

    def _is_empty(self, i, j, table):
        val = table.item(i,j)
        return val is None or val.text() == ""
        
    def _get_float(self, i, j, table=None):
        if table is None:
            table = self.simple_table
            
        if not self._is_empty(i, j, table) and not table.item(i,j).text() == "NA":
            try:
                return float(table.item(i,j).text())
            except:
                print("Could not convert %s to float" % table.item(i,j))
                return None
        return None
        
    def no_val(self, x):
        return x is None or x == ""
        
    def try_to_update_cur_outcome(self):

        n1, m1, sd1, n2, m2, sd2 = self.ma_unit.get_raw_data_for_groups(self.cur_groups)
        se1, se2 = self._get_float(0, 3), self._get_float(1, 3)
        
        # here we check whether or not we have sufficient data to compute an outcome
        ## TODO we should make this conditional more readable.
        if not any([self.no_val(x) for x in [n1, m1, sd1, n2, m2, sd2 ]]) or \
                    not any([self.no_val(x) for x in [m1, se1, m2, se2]]) and self.cur_effect=="MD" or \
                    not any([self.no_val(x) for x in [n1, m1, sd1]]) and self.cur_effect in CONTINUOUS_ONE_ARM_METRICS:
            est_and_ci_d = None
            if self.cur_effect in CONTINUOUS_TWO_ARM_METRICS:
                est_and_ci_d = meta_py_r.continuous_effect_for_study(n1, m1, sd1, se1=se1, \
                                                        n2=n2, m2=m2, sd2=sd2, se2=se2,\
                                                        metric=self.cur_effect)
            else:
                # continuous, one-arm metric
                est_and_ci_d = meta_py_r.continuous_effect_for_study(n1, m1, sd1, \
                                      two_arm=False, metric=self.cur_effect)
            
            
            display_est, display_low, display_high = est_and_ci_d["display_scale"]
            self.ma_unit.set_display_effect_and_ci(self.cur_effect, self.group_str, display_est, display_low, display_high)                            
            
            est, low, high = est_and_ci_d["calc_scale"] # calculation (e.g., log) scale
            self.ma_unit.set_effect_and_ci(self.cur_effect, self.group_str, est, low, high)    
            self.set_current_effect()
    
    def enable_txt_box_input(self):
        ''' Enables text boxes if they are empty, disables them otherwise '''
        
        meta_globals.enable_txt_box_input(self.effect_txt_box, self.low_txt_box,
                                          self.high_txt_box, self.correlation_pre_post)
    
    def reset_table_item_flags(self):
        row = 0
        
        self.block_all_signals(True)
        
        def _reset_flags(table,range_num):
            for col in range(range_num):
                # top table
                item = table.item(row, col)
                if not item is None:
                    newflags = item.flags() | Qt.ItemIsEditable
                    item.setFlags(newflags)
        
        _reset_flags(self.simple_table,8)
        _reset_flags(self.g1_pre_post_table,7)
        _reset_flags(self.g2_pre_post_table,7)

        self.block_all_signals(False)
        
    def enable_back_calculation_btn(self, engage = False):
        
        self.set_clear_btn_color()
        pass
        # TODO: Write body of function
        

    def clear_form(self): 
        self.block_all_signals(True)
        # reset tables
        for table in [self.simple_table, self.g1_pre_post_table, self.g2_pre_post_table]:
            var_names = self.get_column_header_strs(table=table)
            for row_index, group_name in enumerate(self.cur_groups):
                for var_index, var_name in enumerate(var_names):  
                    self._set_val(row_index, var_index, "")
        self.block_all_signals(False)
    
        self._update_ma_unit()

        # clear out effects stuff
        for metric in CONTINUOUS_ONE_ARM_METRICS + CONTINUOUS_TWO_ARM_METRICS:
            if ((self.cur_effect in CONTINUOUS_TWO_ARM_METRICS and metric in CONTINUOUS_TWO_ARM_METRICS) or
                (self.cur_effect in CONTINUOUS_ONE_ARM_METRICS and metric in CONTINUOUS_ONE_ARM_METRICS)):
                self.ma_unit.set_effect_and_ci(metric, self.group_str, None, None, None)
                self.ma_unit.set_display_effect_and_ci(metric, self.group_str, None, None, None)
            else:
                # TODO: Do nothing for now..... treat the case where we have to switch group strings down the line
                pass
            
        # clear line edits
        self.set_current_effect()
        self.block_all_signals(True)
        self.correlation_pre_post.setText("")
        self.block_all_signals(False)
        
        self.save_form_state()
        
        self.reset_table_item_flags()
        self.initialize_backup_structures()
        self.enable_txt_box_input()
        self.CI_spinbox.setValue(meta_globals.DEFAULT_CONF_LEVEL)
        self.CI_spinbox.setEnabled(True)



    def save_form_state(self):
        ''' Saves the state of all objects on the form '''
        
        def save_table_data():
            row = 0
            def _backup_table(table,backup_table,range_num):
                for col in range(range_num):
                    item = table.item(row, col)
                    contents = "" if item is None else item.text()
                    backup_table[row][col]=contents
            _backup_table(self.simple_table, self.simple_table_backup,8)
            _backup_table(self.g1_pre_post_table, self.g1_pre_post_table_backup,7)
            _backup_table(self.g2_pre_post_table, self.g2_pre_post_table_backup,7)
                    
        def save_displayed_effects_data(effect=None):
            print "Saving Displayed Effects data...."
            
            if effect is None:
                effect = self.cur_effect
            
            self.form_effects_dict[effect]["est"]   = self.effect_txt_box.text() 
            self.form_effects_dict[effect]["lower"] = self.low_txt_box.text()    
            self.form_effects_dict[effect]["upper"] = self.high_txt_box.text()    
            self.form_effects_dict["correlation_pre_post"] = self.correlation_pre_post.text()
        
            self.candidate_est        = self.effect_txt_box.text()
            self.candidate_lower      = self.low_txt_box.text()
            self.candidate_upper      = self.high_txt_box.text()
            self.candidate_correlation_pre_post = self.correlation_pre_post.text()

        save_table_data()
        save_displayed_effects_data()
        self.enable_back_calculation_btn()
    
    def restore_form_state(self):
        ''' Restores the state of all objects on the form '''
        
        # Block all signals on the form 
        self.block_all_signals(True)
        ########################################################################
        
        def restore_displayed_effects_data():
            print "Restoring displayed effects data..."
            
            self.effect_txt_box.setText(self.form_effects_dict[self.cur_effect]["est"])    
            self.low_txt_box.setText(self.form_effects_dict[self.cur_effect]["lower"])       
            self.high_txt_box.setText(self.form_effects_dict[self.cur_effect]["upper"])                    
            self.correlation_pre_post.setText(self.form_effects_dict["correlation_pre_post"])
            
            self.candidate_est        = self.effect_txt_box.text()
            self.candidate_lower      = self.low_txt_box.text()
            self.candidate_upper      = self.high_txt_box.text()
            self.candidate_correlation_pre_post = self.correlation_pre_post.text()
        
        def restore_tables():
            row = 0
            def _restore_table(table, table_backup, range_num):
                for col in range(range_num):
                    self.table.blocksSignals(True)
                    self._set_val(row, col, self.table_backup[row][col])
                    self.table.blockSignals(False)
            _restore_table(self.simple_table, self.simple_table_backup, 8)
            _restore_table(self.g1_pre_post_table, self.g1_pre_post_table_backup, 7)
            _restore_table(self.g2_pre_post_table, self.g2_pre_post_table_backup, 7)
        
        self.CI_spinbox.setValue(meta_globals.DEFAULT_CONF_LEVEL)
        restore_displayed_effects_data()
        restore_tables()
        self.enable_back_calculation_btn()
        
        ########################################################################
        # Unblock the signals
        self.block_all_signals(False)
    
    def initialize_backup_structures(self):
        # Stores form effect info as text
        self.form_effects_dict = {}
        for effect in self.get_effect_names():
            self.form_effects_dict[effect] = {"est":"","lower":"","upper":""}
        
        # Stores table items as text
        self.simple_table_backup = [[None,]*8]
        self.g1_pre_post_table_backup = [[None,]*7]
        self.g2_pre_post_table_backup = [[None,]*7]
        
    def get_effect_names(self):
        effects = self.ma_unit.effects_dict.keys()
        return effects
        
    def block_all_signals(self,state):
        for widget in self.entry_widgets:
            widget.blockSignals(state)