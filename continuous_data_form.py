##################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Continuous data form module; for flexible entry of continuous
#  outcome data
##################################################

from PyQt4.Qt import *
from PyQt4 import QtGui
import pdb


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
        self._setup_signals_and_slots()
        self.ma_unit = ma_unit
        self.raw_data = self.ma_unit.get_raw_data_for_groups(cur_txs)
        self.cur_groups = cur_txs
        self.cur_effect = cur_effect
        
        ##
        # set the table headers to reflect the group names
        groups_names = QStringList(self.cur_groups)
        self.simple_table.setVerticalHeaderLabels(groups_names)
        
        for table in (self.simple_table, self.g1_pre_post_table, self.g2_pre_post_table):
            self._set_col_widths(table)
        self.grp_1_lbl.setText(QString(self.cur_groups[0]))
        self.grp_2_lbl.setText(QString(self.cur_groups[1]))
        self._update_raw_data()
        
        #self._update_data_table()
        #self._populate_effect_data()
    
    def _setup_signals_and_slots(self):
        QObject.connect(self.simple_table, SIGNAL("cellChanged (int, int)"), 
                                                                                self._impute_data)
                                                                                
        
        
    def _set_col_widths(self, table):
        for column in range(table.columnCount()):
            table.setColumnWidth(column, default_col_width)
                
    def _update_raw_data(self):
        self.simple_table.blockSignals(True)
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.ma_unit.tx_groups[group_name].raw_data
            for col in range(len(grp_raw_data)):
                val = QTableWidgetItem(str(grp_raw_data[col]))
                self.simple_table.setItem(row_index, col, val)
        
        self._impute_data()
        self.simple_table.blockSignals(False)
        
    def _impute_data(self):
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
            computed_vals = meta_py_r.impute_cont_data(cur_dict)["output"]
            # and then iterate over the columns again, 
            # populating the table with any available
            # computed fields
            self.simple_table.blockSignals(True)
            for var_index, var_name in enumerate(var_names):
                # TODO note the hard-coded number of digits here
                self.simple_table.setItem(row_index, var_index, QTableWidgetItem(QString(str(computed_vals[var_name])[:5])))
            self.simple_table.blockSignals(True)
            #pyqtRemoveInputHook()
            #pdb.set_trace()
                
    def get_column_header_strs(self):
        return [str(h_item.text()) for h_item in \
                                [self.simple_table.horizontalHeaderItem(col) for col in \
                                    range(self.simple_table.columnCount())]]
            
    def _is_empty(self, i, j):
        val = self.simple_table.item(i,j)
        return val is None or val.text() == ""
        
    def _get_float(self, i, j):
        if not self._is_empty(i,j):
            return float(self.simple_table.item(i,j).text())
        return None