###############################################################
#                                                             #
#  George E. Dietz                                            #
#  Byron C. Wallace                                           #
#                                                             #
#  CEBM @ Brown                                               #
#  OpenMeta[analyst]                                          #
#  ---                                                        #
#  Binary data form module; for flexible entry of dichotomous #
#  outcome data                                               #
###############################################################

from PyQt4.Qt import *
from functools import partial

import meta_globals
from meta_globals import *
from meta_globals import _is_a_float, _is_empty
import meta_py_r

def between_bounds(est=None, low=None, high=None):
    def my_lt(a,b):
        if _is_a_float(a) and _is_a_float(b):
            return float(a) < float(b)
        else:
            return None
        
    good_result = my_lt(low,est)
    okay = True if not (good_result is None) else False
    if okay and not good_result:
        msg = "The lower CI must be less than the point estimate!"
        return False,msg
    
    good_result = my_lt(est,high)
    okay = True if not (good_result is None) else False
    if okay and not good_result:
        msg = "The higher CI must be greater than the point estimate!"
        return False,msg
    
    good_result = my_lt(low,high)
    okay = True if not (good_result is None) else False
    if okay and not good_result:
        msg = "The lower CI must be less than the higher CI!"
        return False,msg
    
    return True,None

def cast_to_int(value, name=None):
    '''Converts value to int if possible'''
    try:
        rounded = round(float(value))
        return int(rounded)
    except:
        if not name is None:
            print("Could not convert %s='%s' to int" % (name,str(value)))
        else:
            print("Could not convert '%s' to int" % (str(value)))
        return None

def compute_2x2_table(params):
    ''' Computes values for the whole 2x2 table if possible based on partial values from the rest of the table'''
    
    # Realized R code is screwy.... now for some more screwy code that hopefully works better
    table = [[ params['c11'],   params['c12'],   params['r1sum']],
             [ params['c21'],   params['c22'],   params['r2sum']],
             [ params['c1sum'], params['c2sum'], params['total'] ]]
    
    while True:
        changed = False 
        for row in range(3):
            for col in range(3):
                # go through row-wise
                if table[row][col] in EMPTY_VALS:
                    if col == 0:
                        try:
                            table[row][col] = table[row][2] - table[row][1]
                            changed = True
                        except:
                            pass
                    if col == 1:
                        try:
                            table[row][col] = table[row][2] - table[row][0]
                            changed = True
                        except:
                            pass
                    if col == 2:
                        try:
                            table[row][col] = table[row][0] + table[row][1]
                            changed = True
                        except:
                            pass
                # and now column-wise
                if table[row][col] in EMPTY_VALS:
                    if row == 0:
                        try:
                            table[row][col] = table[2][col] - table[1][col]
                            changed = True
                        except:
                            pass
                    if row == 1:
                        try:
                            table[row][col] = table[2][col] - table[0][col]
                            changed = True
                        except:
                            pass
                    if row == 2:
                        try:
                            table[row][col] = table[0][col] + table[1][col]
                            changed = True
                        except:
                            pass
        if not changed:
            break
    ## end of big while loop
        
    coef = {}
    coef['c11']   = table[0][0]
    coef['c12']   = table[0][1]
    coef['r1sum'] = table[0][2]
    coef['c21']   = table[1][0]
    coef['c22']   = table[1][1]
    coef['r2sum'] = table[1][2]
    coef['c1sum'] = table[2][0]
    coef['c2sum'] = table[2][1]
    coef['total'] = table[2][2]
    
    return coef

# Consistency checking code for 2x2 tables (binary and diagnostic)
########################### CONSISTENCY CHECKING CODE ##########################
class ConsistencyChecker():
    def __init__(self,fn_consistent=None,fn_inconsistent=None,table_2x2=None):
        functions_passed = (not fn_consistent is None) and (not fn_inconsistent is None)
        assert functions_passed, "Not enough functions passed to check_for_consistencies"
        assert not table_2x2 is None, "No table argument passed."
        
        self.inconsistent = False
        self.inconsistent_action = fn_inconsistent
        self.consistent_action = fn_consistent
        self.table = table_2x2
        
    def run(self):
        msg = self.check_for_consistencies()
        
        if not self.inconsistent:
            self._color_all(color=OK_COLOR)
        return msg
     
    def check_for_consistencies(self):
        self.inconsistent = False
        rows_sum  = self.check_that_rows_sum() # also colors non-summing rows
        cols_sum = self.check_that_cols_sum()
        all_pos  = self.check_that_values_positive()
        
        if self.inconsistent:
            self.inconsistent_action()
        else:
            self.consistent_action()
        
        if not rows_sum:
            return "Rows must sum!"
        elif not cols_sum:
            return "Columns must sum!"
        elif not all_pos:
            return "Counts must be positive!"
        else:
            return None
        
    def check_that_rows_sum(self):
        rows_sum = True
        for row in range(3):
            if self._row_is_populated(row):
                row_sum = 0
                for col in range(2):
                    row_sum += self._get_int(row, col)
                if not row_sum == self._get_int(row, 2):
                    self._color_row(row)
                    self.inconsistent = True
                    rows_sum = False
        return rows_sum
    
    def _get_int(self, i, j):
        '''Get value from cell specified by row=i, col=j as an integer'''
        if not self._is_empty_cell(i,j):
            return int(float(self.table.item(i, j).text()))
        else:
            return None # its good to be explicit
                    
    def check_that_cols_sum(self):
        cols_sum = True
        for col in range(3):
            if self._col_is_populated(col):
                col_sum = 0
                for row in range(2):
                    col_sum += self._get_int(row,col)
                if not col_sum == self._get_int(2,col):
                    self._color_col(col)
                    self.inconsistent = True
                    cols_sum = False
        return cols_sum
                    
    def check_that_values_positive(self):
        all_positive = True
        
        for row in range(3):
            for col in range(3):
                value = self._get_int(row,col)
                if not value in EMPTY_VALS:
                    if value < 0:
                        # Color item
                        self.table.blockSignals(True)
                        self.table.item(row,col).setTextColor(ERROR_COLOR)
                        self.table.blockSignals(False)
                        # Set flag
                        self.inconsistent = True
                        all_positive = False
        return all_positive
                        
    def _color_all(self, color=ERROR_COLOR):
        self.table.blockSignals(True)
        for row in range(3):
            for col in range(3):
                #print "setting row: %s, col: %s" % (row, col)
                item = self.table.item(row, col)
                if item is not None:
                    item.setTextColor(color)
        self.table.blockSignals(False)
        
    def _color_row(self, row):
        self.table.blockSignals(True)
        for col in range(3):
            print "setting row: %s, col: %s" % (row, col)
            self.table.item(row, col).setTextColor(ERROR_COLOR)
        self.table.blockSignals(False)
        
    def _color_col(self, col):
        self.table.blockSignals(True)
        for row in range(3):
            print "setting row: %s, col: %s" % (row, col)
            self.table.item(row, col).setTextColor(ERROR_COLOR)
        self.table.blockSignals(False)
        
    def _row_is_populated(self, row):
        
        result = not True in [self._is_empty_cell(row, col) for col in range(3)]
        if result:
            print "Row %d is populated" % row
        return result
    def _col_is_populated(self, col):
        return not True in [self._is_empty_cell(row, col) for row in range(3)]
    
    def _is_empty_cell(self, i, j):
        val = self.table.item(i,j)
        return val is None or val.text() == ""
########################### END CONSISTENCY CHECKER ############################

####### SHARED BINARY, CONTINUOUS, DIAGNOSTIC DATA FORM UTILITY FUNCTIONS ######
def enable_txt_box_input(*args):
    ''' Enables text boxes if they are empty, disables them otherwise
        Input is textbox(es) '''
    
    for text_box in args:
        text_box.blockSignals(True)
        
        text_box.setEnabled(False)
        if text_box.text() in EMPTY_VALS:
            text_box.setEnabled(True)
            
        text_box.blockSignals(False)
        
def init_ci_spinbox_and_label(ci_spinbox, ci_label, value=None):
    if value is None:
        value = meta_globals.get_global_conf_level()
    
    ci_spinbox.blockSignals(True)
    ci_spinbox.setValue(value)
    ci_label.setText("{0:.1f}% Confidence Interval".format(ci_spinbox.value()))
    ci_spinbox.blockSignals(False)
 
CHANGE_CI_ALERT_BASE_MSG = (
    "The size of the confidence level used for a particular study in this "
    "calculator need not correspond with the global confidence level "
    "(currently set at {0:.1%}) chosen for data display on spreadsheets and "
    "forest plots.")
def get_CHANGE_CI_ALERT_MSG():
    return CHANGE_CI_ALERT_BASE_MSG.format(meta_globals.get_global_conf_level()/100.0)

def helper_set_current_effect(ma_unit, txt_boxes, current_effect, group_str, data_type):
    '''Fills in text boxes on calculator forms with data from ma unit.
    I noticed all 3 set_current_effect functions in the 3 calculators are
    nearly identical so it makes sense to share the similiar parts'''
    
    if data_type == "binary":
        conv_to_disp_scale = lambda x: meta_py_r.binary_convert_scale(x, current_effect, convert_to="display.scale")
    elif data_type == "continuous":
        conv_to_disp_scale = lambda x: meta_py_r.continuous_convert_scale(x, current_effect, convert_to="display.scale")
    elif data_type == "diagnostic":
        conv_to_disp_scale = lambda x: meta_py_r.diagnostic_convert_scale(x, current_effect, convert_to="display.scale")
    else:
        raise Exception("data_type unrecognized")
    effect_tbox, lower_tbox, upper_tbox = [txt_boxes[box_name] for box_name in ("effect","lower","upper")]
    
    (est,lower,upper) = ma_unit.get_effect_and_ci(current_effect, group_str)
    (d_est,d_lower,d_upper) = [conv_to_disp_scale(x) for x in (est,lower,upper)]
    for val, txt_box in zip((d_est,d_lower,d_upper),
                          [effect_tbox, lower_tbox, upper_tbox]):
        txt_box.blockSignals(True)
        if val is not None:
            txt_box.setText(QString("%s" % round(val, CALC_NUM_DIGITS)))
        else:
            txt_box.setText(QString(""))
        txt_box.blockSignals(False)

def save_table_data(table):
    nrows, ncols = table.rowCount(), table.columnCount()
    
    none_row = [None]*ncols
    table_backup = []
    for dummy in range(nrows):
        table_backup.append(none_row[:])
    
    for row in range(nrows):
        for col in range(ncols):
            item = table.item(row, col)
            contents = "" if item is None else item.text()
            table_backup[row][col] = contents
    return table_backup

class CommandFieldChanged(QUndoCommand):
    def __init__(self, restore_new_f = None, restore_old_f = None,
                 parent=None, description=""):
        super(CommandFieldChanged, self).__init__(description)
        
        self.parent = parent
        self.just_created = True
        self.restore_new_f = restore_new_f
        self.restore_old_f = restore_old_f
        
    def redo(self):
        if self.just_created:
            self.just_created = False
            self.parent.enable_back_calculation_btn()
        else:
            print("Restoring new ma_unit")
            self.restore_new_f()
            #self.parent.enable_back_calculation_btn() ##
            
    def undo(self):
        print("Restoring old ma_unit")
        self.restore_old_f()
        #self.parent.enable_back_calculation_btn() ##

# Currently unused?
def reset_table_item_flags(table):
    nrows = table.rowCount()
    ncols = table.columnCount()
    
    table.blockSignals(True)
    for row in range(nrows):
        for col in range(ncols):
            item = table.item(row, col)
            if not item is None:
                newflags = item.flags() | Qt.ItemIsEditable
                item.setFlags(newflags)
    table.blockSignals(False)
    
def block_signals(widgets, state):
    for widget in widgets:
        widget.blockSignals(state)

# Only used in binary and continuous?     
def get_raw_data(ma_unit, groups):
    raw_data_dict = {}
    for group in groups:
        raw_data = ma_unit.get_raw_data_for_group(group)
        raw_data_dict[group] = raw_data
    return raw_data_dict

def _input_fields_disabled(table, text_boxes):
    table_disabled = table_cells_editable(table)
    txt_boxes_disabled = _txt_boxes_disabled(text_boxes)

    if table_disabled and txt_boxes_disabled:
        return True
    return False

def table_cells_editable(table):
    cells_uneditable = True
    nrows = table.rowCount()
    ncols = table.columnCount()
    for row in range(nrows):
        for col in range(ncols):
            item = table.item(row, col)
            if item is None:
                continue
            if (item.flags() & Qt.ItemIsEditable) == Qt.ItemIsEditable:
                cells_uneditable = False
    return cells_uneditable
    
def _txt_boxes_disabled(text_boxes):
    return not any([box.isEnabled() for box in text_boxes])

# Function for testing validity and range conditions in form txt boxes
def evaluate(new_text, ma_unit, curr_effect, group_str, conv_to_disp_scale, ci_param = None,
             parent=None, opt_cmp_fn=None, opt_cmp_msg=None):
    '''opt_cmp_fn i.e. 'Optional Compare Function' should return True when the
    desired condition is met and False otherwise. It is a function of new_text:
    opt_cmp_fn(new_text)'''
        
    est,lower,upper = ma_unit.get_effect_and_ci(curr_effect, group_str) # calc scale
    d_est,d_lower,d_upper = [conv_to_disp_scale(x) for x in (est,lower,upper)]
    is_between_bounds = partial(between_bounds, est=d_est, low=d_lower, high=d_upper)
    ###### ERROR CHECKING CODE#####
        # Make sure entered value is numeric and between the appropriate bounds
    if not _is_a_float(new_text) :
        QMessageBox.warning(parent, "whoops", "Must be numeric!")
        raise Exception("error")
    if not opt_cmp_fn: # est, lower, upper
        (good_result, msg) = is_between_bounds(**{ci_param:new_text})
        if not good_result:
            QMessageBox.warning(parent, "whoops", msg)
            raise Exception("error")
    else: # something other than est, lower, upper (like correlation or prevalence)
        print("Result of correlation evalauation is: %s" % str(opt_cmp_fn(new_text)))
        if not opt_cmp_fn(new_text):
            QMessageBox.warning(parent, "whoops", opt_cmp_msg)
            print("raising exception")
            raise Exception("error")
    return float(new_text) # display_scale_val