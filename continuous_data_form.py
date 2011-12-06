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
from meta_globals import *
import ui_continuous_data_form
from ui_continuous_data_form import Ui_ContinuousDataForm

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
        self.alpha = .05
        self.correlation = 0.0
        self.correlation_changed= False
        
        ##
        # set the table headers to reflect the group names
        groups_names = QStringList(self.cur_groups)
        self.simple_table.setVerticalHeaderLabels(groups_names)
        
        for table in (self.simple_table, self.g1_pre_post_table, self.g2_pre_post_table):
            self._set_col_widths(table)
            
        self.grp_1_lbl.setText(QString(self.cur_groups[0]))
        self.grp_2_lbl.setText(QString(self.cur_groups[1]))
        self._populate_effect_data()
        self.update_raw_data()

    def setup_signals_and_slots(self):
        QObject.connect(self.simple_table, SIGNAL("cellChanged (int, int)"), 
                                            self.impute_data)
        QObject.connect(self.simple_table, SIGNAL("cellChanged (int, int)"), 
                                            self._cell_changed)
        QObject.connect(self.alpha_edit, SIGNAL("textChanged (QString)"), 
                                            self.update_alpha)                    
        QObject.connect(self.correlation_pre_post, SIGNAL("textChanged (QString)"), 
                                            self.update_correlation)          
        QObject.connect(self.g1_pre_post_table, SIGNAL("cellChanged (int, int)"),
                                            lambda: self.impute_pre_post_data(self.g1_pre_post_table, 0))
        QObject.connect(self.g2_pre_post_table, SIGNAL("cellChanged (int, int)"),
                                            lambda: self.impute_pre_post_data(self.g2_pre_post_table, 1))
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                                                                self.effect_changed) 
                                                                                
        QObject.connect(self.effect_txt_box, SIGNAL("textChanged(QString)"), lambda new_text : self.val_edit("est", new_text))
        QObject.connect(self.low_txt_box, SIGNAL("textChanged(QString)"), lambda new_text : self.val_edit("lower", new_text))
        QObject.connect(self.high_txt_box, SIGNAL("textChanged(QString)"), lambda new_text : self.val_edit("upper", new_text)) 
        
                                                                                
    def _set_col_widths(self, table):
        for column in range(table.columnCount()):
            table.setColumnWidth(column, default_col_width)
          
    def update_correlation(self):
        success = False
        try:
            val = float(self.correlation_pre_post.text())
            if val >= 0.0 and val <= 1.0:
                self.correlation = val
                success = True
                self.correlation_changed = True
        except:
            pass

        if not success:
            print "invalid correlation entered! %s" % val
            return None

        print "ok -- correlation set to %s" % self.correlation
        # otherwise recompute the estimates
        self.impute_pre_post_data(self.g1_pre_post_table, 0)
        self.impute_pre_post_data(self.g2_pre_post_table, 1)
        
        
    def update_alpha(self):
        success = False
        try:
            val = float(self.alpha_edit.text())
            if val > 0.0 and val < 1.0:
                self.alpha = val
                # recompute ?
                success = True
        except:
            pass
            
        if not success:
            self.ci_label.setText(QString("Invalid alpha specified."))
        else:
            self.ci_label.setText(QString("(%s confidence interval)" % (1-self.alpha)))


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
        
    def val_edit(self, val_str, display_scale_val):
        ''' val_str is one of `est`, `lower`, `upper` '''
        
        try:
            display_scale_val = float(display_scale_val)
        except:
            # a number wasn't entered; ignore
            # should probably clear out the box here, too.
            print "fail."
            return None
            
        calc_scale_val = meta_py_r.continuous_convert_scale(display_scale_val, \
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
            
    def set_current_effect(self):
        effect_dict = self.ma_unit.effects_dict[self.cur_effect][self.group_str]
        for s, txt_box in zip(['display_est', 'display_lower', 'display_upper'], \
                              [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            if effect_dict[s] is not None:
                txt_box.setText(QString("%s" % round(effect_dict[s], NUM_DIGITS)))
            else:
                txt_box.setText(QString(""))
        
    def update_raw_data(self):
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
          
    def _cell_changed(self, row, col):
        self._update_ma_unit()
        self.try_to_update_cur_outcome()
    
    def _update_ma_unit(self):
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.raw_data_dict[group_name]
            for col_index in range(len(grp_raw_data)):
                cur_val = self._get_float(row_index, col_index)
                self.raw_data_dict[group_name][col_index] = cur_val

            # also check if SEs have been entered directly
            se_index = 3
            self.ma_unit.effects_dict[self.cur_effect][self.group_str]["SE"] = self._get_float(row_index, se_index)
        
    def impute_data(self):
        ''' compute what we can for each study from what has been given '''
        # note that we rely on the variable names corresponding to what
        # the meta_py_r routine expects.
        var_names = self.get_column_header_strs()
        for row_index, group_name in enumerate(self.cur_groups):
            # assemble the fields in a dictionary; pass off to meta_py_r
            cur_dict = {}
            for var_index, var_name in enumerate(var_names):
                var_value = self._get_float(row_index, var_index)
                if var_value is not None:
                    cur_dict[var_name] = var_value
            
            # now pass off what we have for this study to the
            # imputation routine
            results_from_r = meta_py_r.impute_cont_data(cur_dict, self.alpha)
            print "results from R (imputation): %s" % results_from_r
            print results_from_r


            if _is_true(results_from_r["succeeded"]):
                computed_vals = results_from_r["output"]
                # and then iterate over the columns again, 
                # populating the table with any available
                # computed fields
            
                self.simple_table.blockSignals(True)
                for var_index, var_name in enumerate(var_names):
                    float_str = self.float_to_str(float(computed_vals[var_name]))
                    self.simple_table.setItem(row_index, var_index, QTableWidgetItem(QString(float_str)))

                    # update the raw data for N, mean and SD fields (this is all that is actually stored)
                    if var_index < 3:
                        #self.ma_unit.tx_groups[group_name].raw_data[var_index] = computed_vals[var_name]
                        self.raw_data_dict[group_name][var_index] = computed_vals[var_name]
                self._update_ma_unit()
                self.simple_table.blockSignals(False)
        
        
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
        results_from_r = meta_py_r.impute_pre_post_cont_data(params_dict, self.correlation, self.alpha)
 
        print "imputation results from R: %s" % results_from_r

        #if self.correlation_changed:
        #    pyqtRemoveInputHook()
        #    pdb.set_trace()

        if _is_true(results_from_r["succeeded"]):
            ### 
            # first update the simple table
            computed_vals = results_from_r["output"]
            self.simple_table.blockSignals(True)
            for var_index, var_name in enumerate(self.get_column_header_strs()):
                float_str = self.float_to_str(float(computed_vals[var_name]))

                self.simple_table.setItem(group_index, var_index, QTableWidgetItem(QString(float_str)))

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
        if not isNaN(float_val):
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
                pyqtRemoveInputHook()
                pdb.set_trace()
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
            
def isNaN(x):
    # there's no built-in for checking if a number is a NaN in
    # Python < 2.6. checking if a number is equal to itself
    # does the trick, though purportedly does not always work.
    return x != x