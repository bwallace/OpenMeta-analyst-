##################################################
#
#  Byron C. Wallace
#  George Dietz
#  OpenMeta[analyst]
#  ---
#  Continuous data form module; for flexible entry of continuous
#  outcome data.
#
# TODO there is some redundancy here with binary_data_form
#      should probably refactor
# 
# Note that we don't make use of the table/custom model
# design here. Rather, we edit the ma_unit object
# directly, based on what the user inputs. This seemed a more
# straightforward approach, because the table itself displays
# many fields that do not ultimately belong in the raw_data --
# it's mostly imputation going on here.
#
##################################################

#import pdb
import sys
import copy

from PyQt4.Qt import *
from functools import partial
import calculator_routines as calc_fncs

import meta_py_r
from meta_globals import *
import forms.ui_continuous_data_form
import forms.ui_choose_back_calc_result_form

default_col_width = 65

# because the output from R is a string ("TRUE"/"FALSE")
# Remove this? GD
_is_true = lambda x: x == "TRUE"

def is_list(x):
    try:
        list(x)
        return True
    except:
        return False

class ContinuousDataForm(QDialog, forms.ui_continuous_data_form.Ui_ContinuousDataForm):
    def __init__(self, ma_unit, cur_txs, cur_group_str, cur_effect, conf_level=None, parent=None):
        super(ContinuousDataForm, self).__init__(parent)
        self.setupUi(self)
        self.setup_signals_and_slots()
        
        if conf_level is None:
            QMessageBox.critical(self, "insufficient arguments", "Confidence interval must be specified")
            raise ValueError("Confidence interval must be specified")
        self.conf_level = conf_level
        self.mult = meta_py_r.get_mult_from_r(self.conf_level)
        
        self.ma_unit = ma_unit
        self.cur_groups = cur_txs
        self.cur_effect = cur_effect
        self.group_str = cur_group_str
        self.metric_parameter = None
        self.entry_widgets = [self.simple_table, self.g1_pre_post_table,
                              self.g2_pre_post_table, self.effect_txt_box,
                              self.low_txt_box, self.high_txt_box,
                              self.correlation_pre_post]
        self.text_boxes = [self.low_txt_box, self.high_txt_box,
                           self.effect_txt_box, self.correlation_pre_post]
        self.ci_label.setText("{0:.1f}% Confidence Interval".format(self.conf_level))
        self.current_item_data = {}
        
        # Set the table headers to reflect the group names
        groups_names = QStringList(self.cur_groups)
        self.simple_table.setVerticalHeaderLabels(groups_names)
        
        self.tables = [self.simple_table, self.g1_pre_post_table, self.g2_pre_post_table]
        for table in self.tables:
            self._set_col_widths(table)
            
        self.grp_1_lbl.setText(QString(self.cur_groups[0]))
        self.grp_2_lbl.setText(QString(self.cur_groups[1]))
        
        self.setup_clear_button_palettes() # Color for clear_button_pallette
        self.initialize_form() # initialize cells to empty items 
        self.undoStack = QUndoStack(self)
        
        self.update_raw_data()
        self._populate_effect_data()
        self.set_current_effect()
        self.impute_data()
        self.enable_back_calculation_btn()
        
        print("current effect: %s" % str(self.cur_effect))
        # Hide pre-post for SMD until it is implemented
        if self.cur_effect not in ["MD","SMD"]:
            self.grp_box_pre_post.setVisible(False)
            self.adjustSize()
            
        self.current_correlation = self._get_correlation_str()
        
    def initialize_form(self, table=None):
        ''' Initialize all cells to empty items
        If table is specified, only clear that table, leave the others alone'''
        
        if table is None:
            for row in range(2):
                for col in range(self.simple_table.columnCount()):
                    self._set_val(row, col, None)
                    self._set_val(row, col, None, self.g1_pre_post_table)
                    self._set_val(row, col, None, self.g2_pre_post_table)
        else:
            for row in range(2):
                for col in range(self.table.columnCount()):
                    self._set_val(row, col, None, table)
        
        for txt_box in self.text_boxes:
            txt_box.setText(QString(""))
            if txt_box == self.correlation_pre_post:
                txt_box.setText(QString("0.0"))
        
    def setup_signals_and_slots(self):
        QObject.connect(self.simple_table,      SIGNAL("cellChanged (int, int)"), self._cell_changed)
        QObject.connect(self.g1_pre_post_table, SIGNAL("cellChanged (int, int)"), lambda row,col: self.impute_pre_post_data(self.g1_pre_post_table, 0, row, col))
        QObject.connect(self.g2_pre_post_table, SIGNAL("cellChanged (int, int)"), lambda row,col: self.impute_pre_post_data(self.g2_pre_post_table, 1, row, col))
        
        QObject.connect(self.effect_cbo_box, SIGNAL("currentIndexChanged(QString)"), self.effect_changed)
        QObject.connect(self.clear_Btn,      SIGNAL("clicked()"), self.clear_form)
        QObject.connect(self.back_calc_btn,  SIGNAL("clicked()"), lambda: self.enable_back_calculation_btn(engage=True) )
                                                                                
        QObject.connect(self.effect_txt_box, SIGNAL("editingFinished()"),   lambda: self.val_changed("est"))
        QObject.connect(self.low_txt_box,    SIGNAL("editingFinished()"),   lambda: self.val_changed("lower"))
        QObject.connect(self.high_txt_box,   SIGNAL("editingFinished()"),   lambda: self.val_changed("upper"))
        QObject.connect(self.correlation_pre_post, SIGNAL("editingFinished()"),   lambda: self.val_changed("correlation_pre_post"))     
        
        # Add undo/redo actions
        undo = QAction(self)
        redo = QAction(self)
        undo.setShortcut(QKeySequence.Undo)
        redo.setShortcut(QKeySequence.Redo)
        self.addAction(undo)
        self.addAction(redo)
        QObject.connect(undo, SIGNAL("triggered()"), self.undo)
        QObject.connect(redo, SIGNAL("triggered()"), self.redo)
            
                                                                                
    def _set_col_widths(self, table):
        for column in range(table.columnCount()):
            table.setColumnWidth(column, default_col_width)
        
    def _populate_effect_data(self):
        effect_names = self.ma_unit.get_effect_names()
        q_effects = sorted([QString(effect_str) for effect_str in effect_names])
        self.effect_cbo_box.blockSignals(True)
        self.effect_cbo_box.addItems(q_effects)
        self.effect_cbo_box.blockSignals(False)
        self.effect_cbo_box.setCurrentIndex(q_effects.index(QString(self.cur_effect)))
        
    def effect_changed(self):
        self.cur_effect = unicode(self.effect_cbo_box.currentText().toUtf8(), "utf-8")
        
        # hide pre-post for SMD
        if self.cur_effect not in ["MD","SMD"]:
            self.grp_box_pre_post.setVisible(False)
            self.adjustSize()
        else:
            self.grp_box_pre_post.setVisible(True)
            self.adjustSize()
        
        self.group_str = self.get_cur_group_str()

        self.try_to_update_cur_outcome()
        self.set_current_effect()
        self.enable_txt_box_input()
        
        self.metric_parameter = None       # zusammen
        self.enable_back_calculation_btn() # zusammen
        
        
    def _text_box_value_is_between_bounds(self, val_str, new_text):
        display_scale_val = ""
        
        get_disp_scale_val_if_valid = partial(
                calc_fncs.evaluate, new_text=new_text, ma_unit=self.ma_unit,
                curr_effect=self.cur_effect, group_str=self.group_str,
                conv_to_disp_scale = partial(meta_py_r.continuous_convert_scale,
                                             metric_name=self.cur_effect,
                                             convert_to="display.scale"),
                                             parent=self, mult=self.mult)

        calc_fncs.block_signals(self.entry_widgets, True)
        try:
            if val_str == "est" and not is_empty(new_text):
                display_scale_val = get_disp_scale_val_if_valid(ci_param='est')
            elif val_str == "lower" and not is_empty(new_text):
                display_scale_val = get_disp_scale_val_if_valid(ci_param='low')
            elif val_str == "upper" and not is_empty(new_text):
                display_scale_val = get_disp_scale_val_if_valid(ci_param='high')
            elif val_str == "correlation_pre_post" and not is_empty(new_text):
                get_disp_scale_val_if_valid(opt_cmp_fn = lambda x: -1<=float(x)<=1,
                                            opt_cmp_msg="Correlation must be between -1 and +1")
        except:
            calc_fncs.block_signals(self.entry_widgets, False)
            return False, False
        calc_fncs.block_signals(self.entry_widgets, False)
        print("Val_str: %s" % val_str)
        return True,display_scale_val
    
    def _get_txt_from_val_str(self, val_str):
        if val_str == "est":
            return str(self.effect_txt_box.text())
        elif val_str == "lower":
            return str(self.low_txt_box.text())
        elif val_str == "upper":
            return str(self.high_txt_box.text())
        elif val_str == "correlation_pre_post":
            return str(self.correlation_pre_post.text())
        return None # should never happen
      
    def val_changed(self, val_str):
        # Backup form state
        old_ma_unit, old_tables_data = self._save_ma_unit_and_table_states(
                                tables = [self.simple_table,
                                          self.g1_pre_post_table,
                                          self.g2_pre_post_table],
                                ma_unit = self.ma_unit, 
                                use_old_value=False)
        old_correlation = self.current_correlation
        
        new_text = self._get_txt_from_val_str(val_str)
            
        
        no_errors, display_scale_val = self._text_box_value_is_between_bounds(val_str, new_text)
        if no_errors is False:
            print("There was an error while in val_changed")
            self.restore_ma_unit_and_tables(old_ma_unit,old_tables_data, old_correlation)
            calc_fncs.block_signals(self.entry_widgets, True)
            if val_str == "est":
                self.effect_txt_box.setFocus()
            elif val_str == "lower":
                self.low_txt_box.setFocus()
            elif val_str == "upper":
                self.high_txt_box.setFocus()
            elif val_str == "correlation_pre_post":
                self.correlation_pre_post.setFocus()
            calc_fncs.block_signals(self.entry_widgets, False)
            return
        
        # If we got to this point it means everything is ok so far
        try:
            if display_scale_val not in EMPTY_VALS:
                display_scale_val = float(display_scale_val)
            else:
                display_scale_val = None
        except ValueError:
            # a number wasn't entered; ignore
            # should probably clear out the box here, too.
            print "fail."
            return None
        
        calc_scale_val = meta_py_r.continuous_convert_scale(display_scale_val,
                                        self.cur_effect, convert_to="calc.scale")
                      
        if val_str == "est":
            self.ma_unit.set_effect(self.cur_effect, self.group_str, calc_scale_val)
        elif val_str == "lower":
            self.ma_unit.set_lower(self.cur_effect, self.group_str, calc_scale_val)
        elif val_str == "upper":
            self.ma_unit.set_upper(self.cur_effect, self.group_str, calc_scale_val)
        elif val_str == "correlation_pre_post":
            print "ok -- correlation set to %s" % self.correlation_pre_post.text()
            # Recompute the estimates
            self.impute_pre_post_data(self.g1_pre_post_table, 0)
            self.impute_pre_post_data(self.g2_pre_post_table, 1)
        
        self.impute_data() #### experimental
        

        new_ma_unit, new_tables_data = self._save_ma_unit_and_table_states(
                        tables = [self.simple_table,
                                  self.g1_pre_post_table,
                                  self.g2_pre_post_table],
                        ma_unit = self.ma_unit, 
                        use_old_value=False)
        new_correlation = self._get_correlation_str()
        restore_old_f = lambda: self.restore_ma_unit_and_tables(old_ma_unit, old_tables_data, old_correlation)
        restore_new_f = lambda: self.restore_ma_unit_and_tables(new_ma_unit, new_tables_data, new_correlation)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f,
                                                restore_old_f=restore_old_f,
                                                parent=self)
        self.undoStack.push(command)
        
        self.current_correlation = new_correlation
        
    def setup_clear_button_palettes(self):
        # Color for clear_button_pallette
        self.orig_palette = self.clear_Btn.palette()
        self.pushme_palette = QPalette()
        self.pushme_palette.setColor(QPalette.ButtonText,Qt.red)
        self.set_clear_btn_color()
        
    def set_clear_btn_color(self):
        if calc_fncs._input_fields_disabled(self.simple_table, [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            self.clear_Btn.setPalette(self.pushme_palette)
        else:
            self.clear_Btn.setPalette(self.orig_palette)

    def set_current_effect(self):
        txt_boxes = dict(effect=self.effect_txt_box, lower=self.low_txt_box, upper=self.high_txt_box)
        calc_fncs.helper_set_current_effect(ma_unit=self.ma_unit,
                                            txt_boxes=txt_boxes,
                                            current_effect=self.cur_effect,
                                            group_str=self.group_str,
                                            data_type="continuous",
                                            mult=self.mult)
        
        self.change_row_color_according_to_metric()
    
    def change_row_color_according_to_metric(self):
        # Change color of bottom rows of table according one or two-arm metric
        curr_effect_is_one_arm = self.cur_effect in CONTINUOUS_ONE_ARM_METRICS
        row = 1
        for col in range(len(self.get_column_header_strs(self.simple_table))):
            item = self.simple_table.item(row, col)
            if curr_effect_is_one_arm:
                item.setBackground(QBrush(QColor(Qt.gray)))
            else:
                # just reset the item
                text = item.text()
                self.simple_table.blockSignals(True)
                popped_item = self.simple_table.takeItem(row, col)
                self.simple_table.blockSignals(False)
                del popped_item
                self._set_val(row, col, text, self.simple_table)
        
    def update_raw_data(self):
        '''Updates table widget with data from ma_unit'''

        self.simple_table.blockSignals(True)
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.ma_unit.get_raw_data_for_group(group_name)
            for col in range(len(grp_raw_data)):
                self._set_val(row_index, col, grp_raw_data[col], self.simple_table)
            # also insert the SEs, if we have them
            se_col = 3
            se = self.ma_unit.get_se(self.cur_effect, self.group_str, self.mult)
            self._set_val(row_index, col, grp_raw_data[col], self.simple_table)
        self.simple_table.blockSignals(False) 
        self.impute_data()

        
    def _cell_data_not_valid(self, celldata_string, cell_header=None):
        # ignore blank entries
        if celldata_string.trimmed() == "" or celldata_string is None:
            return None

        if not is_a_float(celldata_string):
            return "Raw data needs to be numeric."

        if cell_header in ['n','sd','se','var','pval'] and float(celldata_string) < 0:
            return "%s cannot be negative." % (cell_header,)
        
        if cell_header == 'pval' and not (0 <= float(celldata_string) <= 1):
            return "pval must be between 0 and 1"
        return None
    
    def _get_correlation_str(self):
        return str(self.correlation_pre_post.text())
          
    def _cell_changed(self, row, col):
        
        old_ma_unit, old_tables_data = self._save_ma_unit_and_table_states(
                                    tables=self.tables,
                                    ma_unit=self.ma_unit,
                                    table=self.simple_table,
                                    row=row, col=col,
                                    old_value=self.current_item_data[self.simple_table],
                                    use_old_value=True)
        old_correlation = self._get_correlation_str()
        
        # Just for simple_table for now 
        column_headers = self.get_column_header_strs()
        try:
            warning_msg = self._cell_data_not_valid(self.simple_table.item(row, col).text(),column_headers[col])
            if warning_msg:
                raise Exception("Invalid Cell Data")
            self.impute_data()
        except Exception as e:
            msg = e.args[0]
            QMessageBox.warning(self.parent(), "whoops", msg)
            self.restore_ma_unit_and_tables(old_ma_unit, old_tables_data, old_correlation)
            return
        
        self._copy_raw_data_from_table_to_ma_unit() # table --> ma_unit
        self.try_to_update_cur_outcome()
        
        new_ma_unit, new_tables_data = self._save_ma_unit_and_table_states(
                            tables=self.tables,
                            ma_unit=self.ma_unit,
                            table=self.simple_table,
                            row=row, col=col,
                            use_old_value=False)
        new_correlation = self._get_correlation_str()
        restore_old_f = lambda: self.restore_ma_unit_and_tables(old_ma_unit, old_tables_data, old_correlation)
        restore_new_f = lambda: self.restore_ma_unit_and_tables(new_ma_unit, new_tables_data, new_correlation)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f,
                                                restore_old_f=restore_old_f,
                                                parent=self)
        self.undoStack.push(command)
        
        ###self.enable_txt_box_input() # if the effect was imputed
        ###self.set_clear_btn_color()
    
    def _set_val(self, row_index, var_index, val, table=None):
        if table == None:
            table = self.simple_table
        
        row,col = row_index, var_index    
        if is_NaN(val): # get out quick
            print "%s is not a number" % val
            return
        
        try:
            table.blockSignals(True)
            str_val = "" if val in EMPTY_VALS else str(float(val))
            if table.item(row, col) is None:
                table.setItem(row, col, QTableWidgetItem(str_val))
            else:
                table.item(row, col).setText(str_val)
            table.blockSignals(False)
            
            ###self._disable_row_if_filled(table, row, col)
        except:
            print "Unexpected error:", sys.exc_info()[0]
            print("Got to except in _set_val when trying to set (%d,%d) to %s" % (row,col, str(val)))    
            #raise  

    def _disable_row_if_filled(self, table, row, col):
        #if str_val != "": #disable item
        table.blockSignals(True)
        N_col = table.columnCount()
        
        print("Row is filled? %s" % str(self._table_row_filled(table, row)))
        
        if self._table_row_filled(table, row):
            print("Disabling row... %d" % row)
            for col in range(N_col):
                self._disable_cell(table, row, col)
        table.blockSignals(False)
        
    def _disable_cell(self, table, row, col):
        table.blockSignals(True)
        item = table.item(row, col)
        newflags = item.flags() & ~Qt.ItemIsEditable
        item.setFlags(newflags)
        table.blockSignals(False)
        
    def _table_row_filled(self, table, row):
        N_col = table.columnCount()
        row_filled = True
        for col in range(N_col):
            item = table.item(row, col)
            if item is None or item.text() == "":
                row_filled = False
        return row_filled

    def _copy_raw_data_from_table_to_ma_unit(self):
        for row_index, group_name in enumerate(self.cur_groups):
            grp_raw_data = self.ma_unit.get_raw_data_for_group(group_name)
            for col_index in range(len(grp_raw_data)):
                cur_val = self._get_float(row_index, col_index)
                self.ma_unit.get_raw_data_for_group(group_name)[col_index] = cur_val

            ## also check if SEs have been entered directly
            ##se_index = 3
            ##se = self._get_float(row_index, se_index)
            ##self.ma_unit.set_SE(self.cur_effect, self.group_str, se):
            
    def restore_ma_unit(self, old_ma_unit):
        ''' Restores the ma_unit data and resets the form'''
        self.ma_unit.__dict__ = copy.deepcopy(old_ma_unit.__dict__)
        print("Restored ma_unit data: %s" % str(self.ma_unit.get_raw_data_for_groups(self.cur_groups)))
        
        self.initialize_form() # clear form first
        self.update_raw_data()
        self.set_current_effect()
        self.impute_data()
        self.enable_back_calculation_btn()
        #self.set_clear_btn_color()
        
    def restore_tables(self, old_tables_data):
        '''Assumes old tables data given in follow order:
        simple_table, g1_pre_post_table, g2_pre_post_table
        '''
        
        for i,old_table_data in enumerate(old_tables_data):
            nrows = len(old_table_data)
            ncols = len(old_table_data[0])
            table = self.tables[i]
            
            for row in range(nrows):
                    for col in range(ncols):
                        table.blockSignals(True)
                        self._set_val(row, col, old_table_data[row][col], table=table)
                        table.blockSignals(False)
                        
    def restore_ma_unit_and_tables(self, old_ma_unit, old_tables_data,
                                   old_correlation):
        self.restore_ma_unit(old_ma_unit)
        self.restore_tables(old_tables_data)
        self.correlation_pre_post.setText(old_correlation)

    def save_tables_data(self):
        old_tables_data = []
        for table in self.tables:
            old_tables_data.append(calc_fncs.save_table_data(table))
        return old_tables_data
            
    def _save_ma_unit_and_table_states(self, tables, ma_unit, table=None, row=None,
                                       col=None, old_value=None,
                                       use_old_value=True):
        # Make backup of tables info...
        old_tables_data = self.save_tables_data()
        if use_old_value:
            # From before most recently changed cell changed
            old_tables_data[self._get_index_of_table(table)][row][col] = old_value
            
        # Make backup copy of ma_unit
        old_ma_unit = copy.deepcopy(ma_unit)
        return old_ma_unit, old_tables_data
            
    
    def _get_index_of_table(self, table):
        index = -1
        for i,x in enumerate(self.tables):
            if table is x:
                index = i
        return index
        
            
    def impute_data(self):
        ''' compute what we can for each study from what has been given in the table'''
        
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
            alpha = self.conf_level_to_alpha()
            results_from_r = meta_py_r.impute_cont_data(cur_dict, alpha)

            print "Raw results from R (imputation): %s" % results_from_r
            print results_from_r

            print "Results from r succeeded?:", results_from_r["succeeded"]
            if results_from_r["succeeded"]:
                computed_vals = results_from_r["output"]
                # and then iterate over the columns again, 
                # populating the table with any available
                # computed fields
            
                print "Computed vals:",computed_vals
                for var_index, var_name in enumerate(var_names):  
                    self._set_val(row_index, var_index, computed_vals[var_name])
                self._copy_raw_data_from_table_to_ma_unit()
            else:
                try:
                    print("Why didn't it succeed?: '%s'" % results_from_r["comment"])
                except KeyError:
                    pass
    def conf_level_to_alpha(self):
        alpha = 1-self.conf_level/100.0
        return alpha
           
    def impute_pre_post_data(self, table, group_index, row=None, col=None):
        ''' 
        The row index corresponds to the group that will be
        affected by the data edits. E.g., a row index of 0 will result
        in the data for the first group (row 0 in the simple_table)
        being modified.
        '''
        
        if not (row,col) == (None, None): # means this was called through user interaction, not programmatically
            old_ma_unit, old_tables_data = self._save_ma_unit_and_table_states(
                                tables=self.tables,
                                ma_unit=self.ma_unit,
                                table=table,
                                row=row, col=col,
                                old_value=self.current_item_data[table],
                                use_old_value=True)
            old_correlation = self._get_correlation_str()
        
        
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
        params_dict['metric']= ("'%s'" % self.cur_effect)

        # now pass off what we have for this study to the
        # imputation routine
        results_from_r = meta_py_r.impute_pre_post_cont_data(params_dict,
                                        float(self.correlation_pre_post.text()),
                                        self.conf_level_to_alpha())
 
        print "imputation results from R: %s" % results_from_r
        
        if not results_from_r["succeeded"]:
            return None
            
        print("Prepost-imputation succeeded")
        
        ### 
        # first update the simple table
        computed_vals = results_from_r["output"]
        
        for var_index, var_name in enumerate(self.get_column_header_strs()):
            val = computed_vals[var_name]
            self._set_val(group_index, var_index, val)

            # update the raw data for N, mean and SD fields (this is all that is actually stored)
            if var_index < 3:
                self.ma_unit.get_raw_data_for_group(group_name)[var_index] = computed_vals[var_name] #
        
        self.try_to_update_cur_outcome()        
        
        ###
        # also update the pre/post tables
        pre_vals = results_from_r["pre"]
        post_vals = results_from_r["post"]
        for var_index, var_name in enumerate(var_names):
            pre_val = pre_vals[var_name]
            post_val = post_vals[var_name]
            self._set_val(0, var_index, pre_val, table)
            self._set_val(1, var_index, post_val, table)
            
        self._copy_raw_data_from_table_to_ma_unit()
        self.set_clear_btn_color()
        
        # function was invoked as a result of user interaction, not
        # programmatically
        if not (row,col) == (None, None):
            new_ma_unit, new_tables_data = self._save_ma_unit_and_table_states(
                                tables=self.tables,
                                ma_unit=self.ma_unit,
                                table=table,
                                row=row, col=col,
                                use_old_value=False)
            new_correlation = self._get_correlation_str()
            restore_old_f = lambda: self.restore_ma_unit_and_tables(old_ma_unit, old_tables_data, old_correlation)
            restore_new_f = lambda: self.restore_ma_unit_and_tables(new_ma_unit, new_tables_data, new_correlation)
            command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f,
                                                    restore_old_f=restore_old_f,
                                                    parent=self)
            self.undoStack.push(command)
                 
    def float_to_str(self, float_val):
        float_str = ""
        if not is_NaN(float_val):
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
        self.current_item_data[self.simple_table] = self._get_float(currentRow,currentColumn)
        ###print "Current Item Data:",self.current_item_data
        
    @pyqtSignature("int, int, int, int")
    def on_g1_pre_post_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
        self.current_item_data[self.g1_pre_post_table] = self._get_float(currentRow,currentColumn)
        ###print "Current Item Data:",self.current_item_data
        
    @pyqtSignature("int, int, int, int")
    def on_g2_pre_post_table_currentCellChanged(self,currentRow,currentColumn,previousRow,previousColumn):
        self.current_item_data[self.g2_pre_post_table] = self._get_float(currentRow,currentColumn)
        ###print "Current Item Data:",self.current_item_data
        

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
        if not any([self.no_val(x) for x in [n1, m1, sd1, n2, m2, sd2 ]]) or \
                    not any([self.no_val(x) for x in [m1, se1, m2, se2]]) and self.cur_effect=="MD" or \
                    not any([self.no_val(x) for x in [n1, m1, sd1]]) and self.cur_effect in CONTINUOUS_ONE_ARM_METRICS:
            est_and_ci_d = None
            if self.cur_effect in CONTINUOUS_TWO_ARM_METRICS:
                est_and_ci_d = meta_py_r.continuous_effect_for_study(n1, m1, sd1, se1=se1, 
                                                                     n2=n2, m2=m2, sd2=sd2, se2=se2,
                                                                     metric=self.cur_effect,
                                                                     conf_level=self.conf_level)
            else:
                # continuous, one-arm metric
                est_and_ci_d = meta_py_r.continuous_effect_for_study(n1, m1, sd1,
                                      two_arm=False, metric=self.cur_effect, conf_level=self.conf_level)                          
            
            est, low, high = est_and_ci_d["calc_scale"] # calculation (e.g., log) scale
            self.ma_unit.set_effect_and_ci(self.cur_effect, self.group_str, est, low, high, mult=self.mult)    
            self.set_current_effect()
    
    def enable_txt_box_input(self):
        ''' Enables text boxes if they are empty, disables them otherwise '''
        pass
        #meta_globals.enable_txt_box_input(self.effect_txt_box, self.low_txt_box,
        #                                  self.high_txt_box, self.correlation_pre_post)
        
    def enable_back_calculation_btn(self, engage = False):
        # For undo/redo
        old_ma_unit, old_tables_data = self._save_ma_unit_and_table_states(
                                tables = [self.simple_table,
                                          self.g1_pre_post_table,
                                          self.g2_pre_post_table],
                                ma_unit = self.ma_unit, 
                                use_old_value=False)
        old_correlation = self._get_correlation_str()
        
        # Choose metric parameter if not already chosen
        if (self.metric_parameter is None) and self.cur_effect in ["MD","SMD"]:
            print("need to choose metric parameter because it is %s" % str(self.metric_parameter))
            if self.cur_effect == "MD":
                info = "In order to perform back-calculation most accurately, we need to know something about the assumptions about the two population standard deviations.\n*Are we assuming that both of the population standard deviations are the same (as in most parametric data analysis techniques)"
                option0_txt = "yes (default)."
                option1_txt = "no"
                dialog = ChooseBackCalcResultForm(info, option0_txt, option1_txt)
                dialog.setWindowTitle("Population SD Assumptions")
                if dialog.exec_():
                    self.metric_parameter = True if dialog.getChoice() == 0 else False
            elif self.cur_effect == "SMD":
                info = "In order to perform back-calculation most accurately, we need to know if the the bias in the SMD been corrected i.e. should we use Hedge's g or Cohen's d when performing the back calculation?"
                option0_txt = "Hedges' g (default)" 
                option1_txt = "Cohen's d"
                dialog = ChooseBackCalcResultForm(info, option0_txt, option1_txt)
                dialog.setWindowTitle("SMD bias correction")
                if dialog.exec_():
                    self.metric_parameter = True if dialog.getChoice() == 0 else False
            print("metric_parameter is now %s" % str(self.metric_parameter))
                
        def build_data_dicts():
            var_names = self.get_column_header_strs()
            tmp = []
            for row_index in range(2):
                value = lambda x: self._get_float(row_index, x)
                tmp.append([(var_name, value(i)) for i, var_name in enumerate(var_names) if value(i) is not None])
            group1_data = dict(tmp[0])
            group2_data = dict(tmp[1])
            
            tmp = self.ma_unit.get_effect_and_ci(self.cur_effect, self.group_str, self.mult)
            effect_data = {"est":tmp[0], "low":tmp[1], "high":tmp[2],
                           "metric":self.cur_effect,
                           "met.param":self.metric_parameter}
            
            #print("Group 1 Data: ", group1_data)
            #print("Group 2 Data: ", group2_data)
            #print("Effect Data: ", effect_data)
            
            return (group1_data, group2_data, effect_data)
        def new_data(g1_data, g2_data, imputed):
            changed = False
            
            new_data = (imputed["n1"],
                        imputed["sd1"],
                        imputed["mean1"],
                        imputed["n2"],
                        imputed["sd2"],
                        imputed["mean2"])
            old_data = (g1_data["n"]    if "n"    in g1_data else None,
                        g1_data["sd"]   if "sd"   in g1_data else None,
                        g1_data["mean"] if "mean" in g1_data else None,
                        g2_data["n"]    if "n"    in g2_data else None,
                        g2_data["sd"]   if "sd"   in g2_data else None,
                        g2_data["mean"] if "mean" in g2_data else None,
                        )
            new_item_available = lambda old, new: (old is None) and (new is not None)
            comparison = [new_item_available(old_data[i], new_data[i]) for i in range(len(new_data))]
            print("Comparison:", comparison)
            if any(comparison):
                changed = True
            else:
                changed = False
            return changed
            
        if self.cur_effect not in ["MD", "SMD"]:
            self.back_calc_btn.setVisible(False)
            return None
        else:
            self.back_calc_btn.setVisible(True)
            
        (group1_data, group2_data, effect_data) = build_data_dicts()
        imputed = meta_py_r.back_calc_cont_data(group1_data, group2_data, effect_data, self.conf_level)
        print("Imputed data: ", imputed)
        
        # Leave if there was a failure
        if "FAIL" in imputed:
            print("Failure to impute")
            self.back_calc_btn.setEnabled(False)
            return None
        
        if new_data(group1_data, group2_data, imputed):
            self.back_calc_btn.setEnabled(True)
        else:
            self.back_calc_btn.setEnabled(False)
        self.set_clear_btn_color()
        
        if not engage:
            return None
        
        ########################################################################
        # Actually do stuff with imputed data here if we are 'engaged'
        ########################################################################
        # Choose one of the values if multiple ones were returned in the output
        keys_to_names = {"n1":"group 1 sample size",
                         "n2":"group 2 sample size",
                         "sd1":"group 1 standard deviation",
                         "sd2":"group 2 standard deviation",
                         "mean1":"group 1 mean",
                         "mean2":"group 2 mean"}
        for key,value in imputed.iteritems():
            # TODO: (maybe).....: The R code which generates results can
            # POTENTIALLY yield a maximum of 4 numbers for n1 and n2. However,
            # empirical testing has shown that this doesn't really happen.
            # However, for completeness in the future the number of
            # ChooseBackCalcResultForm options should be generated dynamically
            
            if is_list(value):
                info = ("The back calculation has resulted in multiple results for "
                        + keys_to_names[key] + "\n\nPlease choose one of the following:")
                option0_txt = keys_to_names[key] + " = " + str(value[0])
                option1_txt = keys_to_names[key] + " = " + str(value[1])
                print("Options (0,1)", value[0], value[1])
                
                dialog = ChooseBackCalcResultForm(info, option0_txt, option1_txt)
                if dialog.exec_():
                    imputed[key] = value[0] if dialog.getChoice() == 0 else value[1]
                else: # pressed cancel
                    return None # do nothing and leave
    
        # Write the data to the table
        var_names = self.get_column_header_strs()
        group1_data = {"n":imputed["n1"],
                       "sd":imputed["sd1"],
                       "mean":imputed["mean1"]}
        group2_data = {"n":imputed["n2"],
                       "sd":imputed["sd2"],
                       "mean":imputed["mean2"]}
        for row in range(len(self.cur_groups)):
            for var_index, var_name in enumerate(var_names):
                if var_name not in ["n","sd","mean"]:
                    continue
                val = group1_data[var_name] if row == 0 else group2_data[var_name]
                if var_name == 'n' and val not in EMPTY_VALS: 
                    val = int(round(val)) # convert float to integer
                self._set_val(row, var_index, val, self.simple_table)
        
        self.impute_data()
        self._copy_raw_data_from_table_to_ma_unit()
        #self.set_clear_btn_color()
        
        # For undo/redo
        self.enable_back_calculation_btn()
        new_ma_unit, new_tables_data = self._save_ma_unit_and_table_states(
                                            tables = [self.simple_table,
                                                      self.g1_pre_post_table,
                                                      self.g2_pre_post_table],
                                            ma_unit = self.ma_unit, 
                                            use_old_value=False)
        new_correlation = self._get_correlation_str()
        restore_old_f = lambda: self.restore_ma_unit_and_tables(old_ma_unit, old_tables_data, old_correlation)
        restore_new_f = lambda: self.restore_ma_unit_and_tables(new_ma_unit, new_tables_data, new_correlation)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f,
                                                restore_old_f=restore_old_f,
                                                parent=self)
        self.undoStack.push(command)
         
    def clear_form(self):
        # For undo/redo
        old_ma_unit, old_tables_data = self._save_ma_unit_and_table_states(
                                tables = [self.simple_table,
                                          self.g1_pre_post_table,
                                          self.g2_pre_post_table],
                                ma_unit = self.ma_unit, 
                                use_old_value=False)
        old_correlation = self._get_correlation_str()
        
        self.metric_parameter = None  # } these two should go together
        self.enable_txt_box_input()   # }
        
        calc_fncs.block_signals(self.entry_widgets, True)
        # reset tables
        for table in self.tables:
            for row_index in range(len(self.cur_groups)):
                for var_index in range(table.columnCount()):
                    self._set_val(row_index, var_index, "", table=table)
        calc_fncs.block_signals(self.entry_widgets, False)
    
        self._copy_raw_data_from_table_to_ma_unit()

        # clear out effects stuff
        for metric in CONTINUOUS_ONE_ARM_METRICS + CONTINUOUS_TWO_ARM_METRICS:
            if ((self.cur_effect in CONTINUOUS_TWO_ARM_METRICS and metric in CONTINUOUS_TWO_ARM_METRICS) or
                (self.cur_effect in CONTINUOUS_ONE_ARM_METRICS and metric in CONTINUOUS_ONE_ARM_METRICS)):
                self.ma_unit.set_effect_and_ci(metric, self.group_str, None, None, None, mult=self.mult)
            else:
                # TODO: Do nothing for now..... treat the case where we have to switch group strings down the line
                pass
            
        # clear line edits
        self.set_current_effect()
        calc_fncs.block_signals(self.entry_widgets, True)
        self.correlation_pre_post.setText("0.0")
        calc_fncs.block_signals(self.entry_widgets, False)
        
        # For undo/redo
        self.enable_back_calculation_btn()
        new_ma_unit, new_tables_data = self._save_ma_unit_and_table_states(
                                            tables = [self.simple_table,
                                                      self.g1_pre_post_table,
                                                      self.g2_pre_post_table],
                                            ma_unit = self.ma_unit, 
                                            use_old_value=False)
        new_correlation = self._get_correlation_str()
        restore_old_f = lambda: self.restore_ma_unit_and_tables(old_ma_unit, old_tables_data, old_correlation)
        restore_new_f = lambda: self.restore_ma_unit_and_tables(new_ma_unit, new_tables_data, new_correlation)
        command = calc_fncs.CommandFieldChanged(restore_new_f=restore_new_f,
                                                restore_old_f=restore_old_f,
                                                parent=self)
        self.undoStack.push(command)
        
        
    def get_effect_names(self):
        effects = self.ma_unit.get_effect_names()
        return effects

    def get_cur_group_str(self):
        # Inspired from get_cur_group_str of ma_data_table_model

        if self.cur_effect in CONTINUOUS_ONE_ARM_METRICS:
            group_str = self.cur_groups[0]
        else:
            group_str = "-".join(self.cur_groups)
        return group_str
    
    ####### Undo framework ############
    def undo(self):
        print("undoing....")
        self.undoStack.undo()
        
    def redo(self):
        print("redoing....")
        self.undoStack.redo()


################################################################################
class ChooseBackCalcResultForm(QDialog, forms.ui_choose_back_calc_result_form.Ui_ChooseBackCalcResultForm):
    def __init__(self, info_text, op1_txt, op2_txt, parent=None, op3_txt=None, op4_txt=None):
        super(ChooseBackCalcResultForm, self).__init__(parent)
        self.setupUi(self)
                
        ####self.choice1_lbl.setText(op1_txt)
        ####self.choice2_lbl.setText(op2_txt)
        
        self.choice1_btn.setText(op1_txt)
        self.choice2_btn.setText(op2_txt)
        
        
        self.info_label.setText(info_text)

    def getChoice(self):
        # Choice data to be returned is index of data item
        if self.choice1_btn.isChecked():
            return 0
        else:
            return 1
################################################################################
        