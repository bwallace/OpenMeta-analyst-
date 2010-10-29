##################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Continuous data form module; for flexible entry of continuous
#  outcome data.
#
# Note that we don't make use of the table/custom model
# design here. Rather, we edit the raw_data_dict 
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
import ui_continuous_data_form
from ui_continuous_data_form import Ui_ContinuousDataForm

# @TODO this should be an *application global*. It is now a
# global here and in the data_table_view class. (However
# here we show four digits; there it is 3. We want different
# levels of granularity).
NUM_DIGITS = 4 
default_col_width = 65

class ContinuousDataForm(QDialog, ui_continuous_data_form.Ui_ContinuousDataForm):
    def __init__(self, ma_unit, cur_txs, cur_effect, parent=None):
        super(ContinuousDataForm, self).__init__(parent)
        self.setupUi(self)
        self.setup_signals_and_slots()
        self.ma_unit = ma_unit
        self.raw_data_dict = {}
        for group in cur_txs:
            raw_data = self.ma_unit.get_raw_data_for_group(group)
            self.raw_data_dict[group]  = raw_data
        self.cur_groups = cur_txs
        self.cur_effect = cur_effect
        self.alpha = .05
        self.correlation = 0
        
        ##
        # set the table headers to reflect the group names
        groups_names = QStringList(self.cur_groups)
        self.simple_table.setVerticalHeaderLabels(groups_names)
        
        for table in (self.simple_table, self.g1_pre_post_table, self.g2_pre_post_table):
            self._set_col_widths(table)
            
        self.grp_1_lbl.setText(QString(self.cur_groups[0]))
        self.grp_2_lbl.setText(QString(self.cur_groups[1]))
        self.update_raw_data()

    def setup_signals_and_slots(self):
        QObject.connect(self.simple_table, SIGNAL("cellChanged (int, int)"), 
                                            self.impute_data)
        QObject.connect(self.simple_table, SIGNAL("cellChanged (int, int)"), 
                                            self._cell_changed)
        QObject.connect(self.alpha_edit, SIGNAL("textChanged (QString)"), 
                                            self.update_alpha)                    
        QObject.connect(self.correlation_simple, SIGNAL("textChanged (QString)"), 
                                            self.update_correlation)          
        QObject.connect(self.g1_pre_post_table, SIGNAL("cellChanged (int, int)"),
                                            lambda: self.impute_pre_post_data(self.g1_pre_post_table, 0))
        QObject.connect(self.g2_pre_post_table, SIGNAL("cellChanged (int, int)"),
                                            lambda: self.impute_pre_post_data(self.g2_pre_post_table, 1))
                                                                                
    def _set_col_widths(self, table):
        for column in range(table.columnCount()):
            table.setColumnWidth(column, default_col_width)
          
    def update_correlation(self):
        success = False
        try:
            val = float(self.correlation_simple.text())
            if val > 0.0 and val < 1.0:
                self.correlation = val
                success = True
        except:
            pass
        
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

        
    def update_raw_data(self):
        self.simple_table.blockSignals(True)
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.raw_data_dict[group_name]
            for col in range(len(grp_raw_data)):
                val = QTableWidgetItem(str(grp_raw_data[col]))
                self.simple_table.setItem(row_index, col, val)
        
        self.impute_data()
        self.simple_table.blockSignals(False)
          
    def _cell_changed(self, row, col):
        self._update_ma_unit()
    
    def _update_ma_unit(self):
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.raw_data_dict[group_name]
            for col_index in range(len(grp_raw_data)):
                cur_val = self._get_float(row_index, col_index)
                self.raw_data_dict[group_name][col_index] = cur_val
               
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
            print results_from_r
            if results_from_r["succeeded"]:
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
        var_names = self.get_column_header_strs()
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
        #print results_from_r
        if results_from_r["succeeded"]:
            ### 
            # first update the simple table
            computed_vals = results_from_r["output"]
            self.simple_table.blockSignals(True)
            for var_index, var_name in enumerate(var_names):
                float_str = self.float_to_str(float(computed_vals[var_name]))

                self.simple_table.setItem(group_index, var_index, QTableWidgetItem(QString(float_str)))

                # update the raw data for N, mean and SD fields (this is all that is actually stored)
                if var_index < 3:
                    #self.ma_unit.tx_groups[group_name].raw_data[var_index] = computed_vals[var_name]
                    self.raw_data_dict[group_name][var_index] = computed_vals[var_name]
                    
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
                    
    def get_column_header_strs(self):
        return [str(h_item.text()) for h_item in \
                                [self.simple_table.horizontalHeaderItem(col) for col in \
                                    range(self.simple_table.columnCount())]]
            
    def _is_empty(self, i, j, table):
        val = table.item(i,j)
        return val is None or val.text() == ""
        
    def _get_float(self, i, j, table=None):
        if table is None:
            table = self.simple_table
            
        if not self._is_empty(i, j, table) and not table.item(i,j).text() == "NA":
            return float(table.item(i,j).text())
        return None
        
def isNaN(x):
    # there's no built-in for checking if a number is a NaN in
    # Python < 2.6. checking if a number is equal to itself
    # does the trick, though purportedly does not always work.
    return x != x