#########################################################################################
#                                                                                       #
#  Byron C. Wallace                                                                     #
#  George Dietz                                                                         #
#  CEBM @ Brown                                                                         #
#  OpenMeta[analyst]                                                                    #
#  ---                                                                                  #
#  Proxy class, interfaces between the underlying representation (in ma_dataset.py)     #
#  and the DataTableView UI. Basically deals with keeping track of which outcomes/      #
#  follow-ups/treatments are being viewed. See Summerfield's chapters on M-V-C          #
#  in "Rapid GUI Programming with Python and QT" for an overview of the architecture.   #
#########################################################################################

# core libraries
#import PyQt4
from PyQt4 import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from PyQt4.QtCore import pyqtRemoveInputHook
import pdb

# home-grown
from ma_dataset import Dataset,Outcome,Study,Covariate
import meta_py_r
import meta_globals
from meta_globals import *
import calculator_routines as calc_fncs

# number of (empty) rows in the spreadsheet to show
# following the last study.
DUMMY_ROWS = 20

def DebugHelper(function):
    def _DebugHelper(*args, **kw):
        print("Entered %s" % function.func_name)
        res = function(*args, **kw)
        print("Left %s" % function.func_name)
        return res
    return _DebugHelper

class DatasetModel(QAbstractTableModel):
    '''
    This module mediates between the classes comprising a dataset
    (i.e., study & ma_unit objects) and the view. In particular, we
    subclass the QAbstractTableModel and provide the fields of interest
    to the view.

    Apologies for the mixing of camelCase and lower_case style method
    names; the former are due to the QT framework, but I just couldn't
    bring myself to maintain this blighted style.
    '''
    
        #
    # column indices; these are a core component of this class,
    # as these indices are what maps the UI to the model. The following
    # columns are constant across datatypes, but some (e.g., the 
    # columns corresponding to raw data) are variable. see the
    # update_column_indices method for more.
    INCLUDE_STUDY = 0
    NAME, YEAR = [col+1 for col in range(2)]
    
    headers = ["include", "study name", "year"]
    
    
    def __init__(self, filename=QString(), dataset=None, add_blank_study=True):
        super(DatasetModel, self).__init__()

        self.dataset = dataset
        if dataset is None:
            self.dataset = Dataset()

        if add_blank_study:
            # include an extra blank study to begin with
            self.dataset.studies.append(Study(self.max_study_id() +1))
            # ... and mark this study as such.
            self.study_auto_added = self.dataset.studies[-1].id
        
        # these variables track which meta-analytic unit,
        # i.e., outcome and time period, are being viewed
        self.current_outcome = None   # Current outcome name, not an outcome object # SHOULD BE REFACTORED to self.current_outcome_name to be more accurate
        self.current_time_point = 0
        
        # we also track which groups are being viewed
        self.tx_index_a = 0
        self.tx_index_b = 1

        self.update_current_group_names()
        
        print("calling update column indices from ma_data_table_model init")
        self.update_column_indices()
         
         
        # @TODO parameterize; make variable
        self.current_effect = "OR" 

        # @TODO presumably the COVARIATES will contain the column
        # indices and the currently_displayed... will contain the names
        # of the covariates being displayed in said columns, in order
        self.COVARIATES = None
        self.currently_displayed_covariates = []

        # @TODO
        self.LABELS = None
        

        self.NUM_DIGITS = 3
        self.dirty = False

        
    def set_current_metric(self, metric):
        self.current_effect = metric
        print "OK! metric updated."
        
#    def delete_stored_lower_upper(self):
#        ''' Deletes the values stored in upper and lower since these are just
#        used temporarily to calculate the SE. Probably called when the
#        confidence level is changed '''
#        
#        group_str = self.get_cur_group_str()
#        
#        for study_index in range(len(self.dataset.studies)): # FINISH
#            ma_unit = self.get_current_ma_unit_for_study(self, study_index)
#            ma_unit.set_lower(self.current_effect, group_str, None)
#            ma_unit.set_upper(self.current_effect, group_str, None)
            
        
    def update_current_outcome(self):
        outcome_names = self.dataset.get_outcome_names()
        ###
        # @TODO we need to maintain a current outcome
        # index here, as we do for groups (below), so that
        # when the user edits the currently displayed outcome,
        # the edited outcome is shown in its place
        self.current_outcome = outcome_names[0] if len(outcome_names)>0 else None
        self.reset()
        
    def update_current_time_points(self):
        if self.current_outcome is not None:
            # note that the user cannot delete all follow-ups; so it's safe to assume this dictionary has 
            # at least one entry
            self.current_time_point = self.dataset.outcome_names_to_follow_ups[self.current_outcome].keys()[0]
        else:
            self.current_time_point = 0
        self.reset()
        
    def update_current_group_names(self):
        '''
        This is to be called after the model has been
        edited (via, e.g., the edit_dialog module)
        '''
        group_names = self.dataset.get_group_names()        
        n_groups = len(group_names)
        if n_groups > 1:
            # make sure the indices are within range -- the
            # model may have changed without our knowing.
            # may have been nicer to have a notification
            # framework here (i.e., have the underlying model
            # notify us when a group has been deleted) rather
            # than doing it on the fly...
            self.tx_index_a = self.tx_index_a % n_groups
            self.tx_index_b = self.tx_index_b % n_groups
            while self.tx_index_a == self.tx_index_b:
                self._next_group_indices(group_names)
            self.current_txs = [group_names[self.tx_index_a], group_names[self.tx_index_b]]
        else:
            if not self.is_diag():
                #self.current_txs = ["tx A", "tx B"]
                self.current_txs = meta_globals.DEFAULT_GROUP_NAMES
            else:
                self.current_txs = ["test 1"]
        self.previous_txs = self.current_txs
        self.reset()
        
    def update_column_indices(self):
        # Here we update variable column indices, contingent on 
        # the type data being displayed, the number of covariates, etc. 
        # It is extremely important that these are updated as necessary
        # from the view side of things
        
        #######################################################################
        current_data_type = self.get_current_outcome_type()
        outcome_subtype = self.get_current_outcome_subtype()

        self.RAW_DATA, self.OUTCOMES = self.get_column_indices(current_data_type, outcome_subtype)
            
    @staticmethod
    def get_column_indices(data_type, sub_type):
        '''
        Basically to support getting the column indices w/o having to
        instantiate an instance of the class
        '''
        
        raws, outcomes = [],[] # Raw & outcome indices
        
        # offset corresponds to the first three columns, which 
        # are include study, name, and year.
        offset = 3
        if data_type == "binary":
            raws = [col+offset for col in range(4)]
            outcomes = [7, 8, 9]
        elif data_type == "continuous":
            raws = [col+offset for col in range(6)]
            outcomes = [9, 10, 11]
            if sub_type == 'generic_effect': # generic effect and se
                print("Detected generic effect outcome in update_column_indices")
                raws = []
                outcomes = [offset, offset+1] #effect and se
        else: # diagnostic
            raws = [col+offset for col in range(4)]
            outcomes = [7, 8, 9, 10, 11, 12] # sensitivity & specificity
        
        return raws, outcomes
        
        
    def format_float(self, float_var, num_digits=None):
        ''' this method assumes the input can be cast to a float! '''
        float_var = float(float_var)
        precision = num_digits or self.NUM_DIGITS
        formatted_str = "'%." + str(precision) + "f'" 
        # kind of hacky; I can't find a better way to make the
        # number of digits in the formatting parametric. oh well.
        return eval(formatted_str + "% float_var")



    def data(self, index, role=Qt.DisplayRole):
        '''
        Implements the required QTTableModel data method. There is a lot of switching on 
        role/index/datatype here, but this seems consistent with the QT paradigm (see 
        Summerfield's book)
        '''

        # number of digits to show in edit mode. this, I think, is enough.
        NUM_DIGITS_PRECISE = 12 
        # by default, we'll use the global NUM_DIGITS; this is the default
        # used by the format_float method unless num_digits is set to 
        # something else, i.e., NUM_DIGITS_PRECISE in the case of editing
        num_digits = None

        if not index.isValid() or not (0 <= index.row() < len(self.dataset)):
            return QVariant()
        study = self.dataset.studies[index.row()]
        current_data_type = self.dataset.get_outcome_type(self.current_outcome)
        outcome_subtype = self.dataset.get_outcome_subtype(self.current_outcome)
        column = index.column()


        if role in (Qt.DisplayRole, Qt.EditRole):
            if column == self.NAME:
                return QVariant(study.name)
            elif column == self.YEAR:
                if study.year == 0:
                    return QVariant("")
                else:
                    return QVariant(study.year)
            elif self.current_outcome is not None and column in self.RAW_DATA:
                adjusted_index = column - 3
                if self.current_outcome in study.outcomes_to_follow_ups:
                    ma_unit = self.get_current_ma_unit_for_study(index.row())
                    cur_raw_data = ma_unit.get_raw_data_for_groups(self.current_txs)                            
                    if len(cur_raw_data) > adjusted_index:
                        val = cur_raw_data[adjusted_index]
                        if val == "" or val is None:
                            return QVariant(val)
                        try:
                            # these are the continuous columns containing sample
                            # size; they will be integers, presumably
                            N_columns = (self.RAW_DATA[0], self.RAW_DATA[3])

                            # issue #31 -- make sure digits are consistent
                            if current_data_type == CONTINUOUS and not column in N_columns:
                                # issue #151 -- show greater precision on double-click
                                if role == Qt.EditRole:
                                    # then we're editing, so show greater precision
                                    num_digits = NUM_DIGITS_PRECISE 
                                return QVariant(str(self.format_float(val, num_digits=num_digits)))
                            else:
                                return QVariant(round(val, self.NUM_DIGITS))
                        except:
                            #pyqtRemoveInputHook()
                            #pdb.set_trace()
                            pass
                    else:
                        return QVariant("")
                else:
                    return QVariant("")
            elif column in self.OUTCOMES:
                # more precision in edit moe -- issue #151
                if role == Qt.EditRole:
                    # then we're editing, so show greater precision
                    num_digits = NUM_DIGITS_PRECISE 

                group_str = self.get_cur_group_str()
                # either the point estimate, or the lower/upper
                # confidence interval
                outcome_index = column - self.OUTCOMES[0]
                outcome_val = None
                ma_unit = self.get_current_ma_unit_for_study(index.row())
                
                if not self.is_diag():
                    eff,grp = self.current_effect, group_str
                    
                    if current_data_type == BINARY:
                        conv_to_disp_scale = lambda x: meta_py_r.binary_convert_scale(x, eff, convert_to="display.scale")
                    elif current_data_type == CONTINUOUS:
                        conv_to_disp_scale = lambda x: meta_py_r.continuous_convert_scale(x, eff, convert_to="display.scale")
                    
                    if current_data_type == CONTINUOUS and outcome_subtype == 'generic_effect':
                        d_est_and_se = ma_unit.get_display_effect_and_se(eff, grp, conv_to_disp_scale)
                        print("DEST AND SE: %s" % str(d_est_and_se))
                        outcome_val = d_est_and_se[outcome_index]
                    else: # normal case of no outcome subtype
                        d_est_and_ci = ma_unit.get_display_effect_and_ci(eff, grp, conv_to_disp_scale)
                        outcome_val = d_est_and_ci[outcome_index]
                    
                    if outcome_val is None:
                        return QVariant("")
                    return QVariant(self.format_float(outcome_val, num_digits=num_digits))  # issue #31
                else: # This is the diagnostic case
                    study_index = index.row()
                    # note that we do things quite differently in the diagnostic case,
                    # because there is no notion of a 'current effect'. instead,
                    # we always show sensitivity and specificity. thus we parse
                    # out the estimates and CIs for these manually here.
                    m_str = "Sens"
                    if column in self.OUTCOMES[3:]:
                        m_str = "Spec"
                    
                    #est_and_ci = ma_unit.get_effect_and_ci(m_str, group_str)
                    #c_val = est_and_ci[outcome_index % 3]
                    #outcome_val = meta_py_r.diagnostic_convert_scale(c_val, m_str, convert_to="display.scale") 
                    
                    d_est_and_ci = ma_unit.get_display_effect_and_ci(m_str, group_str)
                    outcome_val = d_est_and_ci[outcome_index % 3]
                    
                    if outcome_val is None:
                        return QVariant("")
         
                    return QVariant(self.format_float(outcome_val, num_digits=num_digits)) # issue #31
                
            elif column != self.INCLUDE_STUDY and column > max(self.OUTCOMES):
                # here the column is to the right of the outcomes (and not the 0th, or
                # 'include study' column), and thus must correspond to a covariate.
                cov_obj = self.get_cov(column)
                if cov_obj is None:
                    return QVariant("")
                    
                cov_name = cov_obj.name
                cov_value = study.covariate_dict[cov_name] if \
                    study.covariate_dict.has_key(cov_name) else None
                if cov_value is None:
                    cov_value = ""
                
                if cov_value != "" and cov_obj.data_type == CONTINUOUS:
                    return QVariant(self.format_float(cov_value, num_digits=num_digits))
                else:
                    # factor
                    return QVariant(cov_value)
        elif role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        elif role == Qt.CheckStateRole:
            # this is where we deal with the inclusion/exclusion of studies
            if column == self.INCLUDE_STUDY:
                checked_state = Qt.Unchecked
                if index.row() < self.rowCount()-1 and study.include:
                    checked_state = Qt.Checked
                return QVariant(checked_state)
        elif role == Qt.BackgroundColorRole:
            if column in self.OUTCOMES:
                return QVariant(QColor(Qt.yellow))
            elif column in self.RAW_DATA[len(self.RAW_DATA)/2:] and \
                        self.current_effect in ONE_ARM_METRICS:
                return QVariant(QColor(Qt.gray))
            else:
                return QVariant(QColor(Qt.white))


    def get_cur_group_str(self):
        # we have to build a key (string) here to index into the
        # correct outcome in the meta-analytic unit. the protocol is
        # as follows. if we are dealing with a two group outcome,
        # then the string is:
        #    tx A-tx B
        # if we have a one group outcome, the string is just:
        #    tx A
        if self.current_effect in ONE_ARM_METRICS:
            group_str = self.current_txs[0] 
        else:
            group_str = "-".join(self.current_txs)
        return group_str
        

    def _verify_raw_data(self, s, col, data_type, index_of_s=None):
        # ignore blank entries
        if s.trimmed() == "" or s is None:
            return True, None

        if not meta_globals._is_a_float(s):
            return False, "Raw data needs to be numeric."

        if data_type in (BINARY, DIAGNOSTIC):
            if not meta_globals._is_an_int(s):
                return False, "Expecting count data -- you provided a float (?)"
            if int(s) < 0:
                return False, "Counts cannot be negative."
        
        # fix for issue #193
        # do not allow TxA to be greater than N_A, or TxB to be greater than N_B
        msg = "Number of events cannot be greater than number of samples."
        (row,col) = (index_of_s.row(), index_of_s.column())
        if data_type == BINARY:
            if col in [3,5]: # col is TxA or TxB
                N_samples = self.data(self.index(row, col+1)).toString() # string representation of N_samples
                if meta_globals._is_an_int(N_samples):
                    if int(s) > int(N_samples): #uh oh
                        return False, msg
            elif col in [4,6]: # col is N_A or N_B
                N_events = self.data(self.index(row, col-1)).toString()
                if meta_globals._is_an_int(N_events):
                    if int(s) < int(N_events):
                        return False, msg
            
        
        if data_type == CONTINUOUS:
            if float(s) < 0:
                if col in [3,6]:
                    return False,"Count cannot be negative"
                if col in [5,8]:
                    return False,"Standard Deviation cannot be negative"
            
        return True, None


    def _verify_outcome_data(self, s, col, row, data_type):
        outcome_subtype = self.dataset.get_outcome_subtype(self.current_outcome)
        
        if not meta_globals._is_a_float(s):
            return False, "Outcomes need to be numeric, you crazy person"

        ma_unit = self.get_current_ma_unit_for_study(row)
        group_str = self.get_cur_group_str()
        
        ###binary_display_scale = meta_py_r.binary_convert_scale(x, metric_name, convert_to)
        binary_display_scale = lambda x: meta_py_r.binary_convert_scale(x, self.current_effect, convert_to="display.scale")
        continuous_display_scale = lambda x: meta_py_r.continuous_convert_scale(x, self.current_effect, convert_to="display.scale")
        
        
        if data_type in [BINARY, CONTINUOUS]: 
            prev_est, prev_lower, prev_upper = ma_unit.get_effect_and_ci(self.current_effect, group_str)
        if data_type == BINARY:
            prev_est, prev_lower, prev_upper = [binary_display_scale(x) for x in [prev_est, prev_lower, prev_upper]]
            print("Previous binary: %s" % str([prev_est, prev_lower, prev_upper]))
        elif data_type == CONTINUOUS:
            #prev_est, prev_lower, prev_upper = ma_unit.get_display_effect_and_ci(self.current_effect, group_str)
            prev_est, prev_lower, prev_upper = [continuous_display_scale(x) for x in [prev_est, prev_lower, prev_upper]]
            print("Previous continuous: %s" % str([prev_est, prev_lower, prev_upper]))
        elif data_type == DIAGNOSTIC:
            m_str = "Sens" if col in self.OUTCOMES[:3] else "Spec"
            #prev_est, prev_lower, prev_upper = ma_unit.get_display_effect_and_ci(m_str, group_str)
            prev_est, prev_lower, prev_upper = ma_unit.get_effect_and_ci(m_str, group_str)
            prev_est, prev_lower, prev_upper = [meta_py_r.diagnostic_convert_scale(x, m_str, convert_to="display.scale") for x in [prev_est, prev_lower, prev_upper]]
            print("Previous diagnostic: %s" % str([prev_est, prev_lower, prev_upper]))
            
        # here we check if there is raw data for this study; 
        # if there is, we don't allow entry of outcomes
        raw_data = self.get_cur_raw_data_for_study(row)
    
        if not all([meta_globals._is_empty(s_i) for s_i in raw_data]):
            # fix for issue #180 
            # sort of hacky. we check here to see if the outcome
            # in fact was "changed", by which we mean the value
            # has been set to a 'sufficiently' different
            # value. this avoids the UI annoyingly bugging users when
            # they are tabbing along. probably a better fix would
            # be to modify the actual tabbing behavior of the spreadsheet
            # for the last 'raw data' column.
            d = dict(zip(self.OUTCOMES, [prev_est, prev_lower, prev_upper]))
            new_val = float(s)
            previously_was_none = d[col] is None
            delta = None
            if previously_was_none:
                # then it was previously not set;
                # go ahead and let the user override.
                delta = float("-inf")
            else:
                delta = abs(new_val - d[col])
                print "new val {0}, prev val {1}".format(new_val, d[col])
                print "DELTA {0}".format(delta)
            epsilon = 10E-6 
            if delta > epsilon:
                return False, '''You have already entered raw data for this study. If you want to enter the outcome directly, delete the raw data first.'''

        if s.trimmed() == '':
            # in this case, they've deleted a value
            # (i.e., left it blank) -- this is OK.
            return True, None 
        if self.current_effect in ("OR", "RR"):
            if float(s) < 0:
                return False, "Ratios cannot be negative."
        
        #figure out why type of column we are in
        fields = ["est","lower","upper"]
        if data_type == DIAGNOSTIC:
            fields.extend(fields[:])
        col_to_type = dict(zip(self.OUTCOMES,fields))
        val_str = col_to_type[col]
        
        if outcome_subtype == "generic_effect":
            if col == self.OUTCOMES[1]: # se column
                if float(s) < 0:
                    return False, "Standard Error cannot be negative"
        else:
            def is_between_bounds(est=prev_est, low=prev_lower, high=prev_upper):
                return calc_fncs.between_bounds(est=est, low=low, high=high)
            good_result = None
            if val_str == "est":
                (good_result,msg) = is_between_bounds(est=float(s))
            elif val_str == "lower":
                (good_result,msg) = is_between_bounds(low=float(s))
            elif val_str == "upper":
                (good_result,msg) = is_between_bounds(high=float(s))
            assert not good_result is None, "Why don't we have a result for what outcome we're in?"
            if not good_result:
                return False, msg

        return True, None

    def _verify_year(self, s):
        if s.trimmed() == '':
            return True, None

        if not meta_globals._is_an_int(s):
            return False, "Years need to be integers."

        return True, None


    def setData(self, index, value, role=Qt.EditRole, import_csv = False):
        '''
        Implementation of the AbstractDataTable method. The view uses this method
        to request data to display. Thus we here return values to render in the table
        based on the index (row, column).

        For more, see: http://doc.trolltech.com/4.5/qabstracttablemodel.html
        '''
        group_str = self.get_cur_group_str()
        study_added_due_to_edit = None
        if index.isValid() and 0 <= index.row() < len(self.dataset):
            current_data_type = self.dataset.get_outcome_type(self.current_outcome)
            outcome_subtype = self.dataset.get_outcome_subtype(self.current_outcome)
            column = index.column()
            old_val = self.data(index)
            study = self.dataset.studies[index.row()]
        else:
            return False
            
        if column == self.NAME:
            study.name = unicode(value.toString().toUtf8(), encoding="utf8")
            if study.name == "":
                # just ignore -- we don't allow empty study names
                return False
            elif index.row() == self.rowCount()-DUMMY_ROWS-1:
                # if the last study was just edited, append a
                # new, blank study
                # TODO bug: if a new tx group is added, and then a new study
                # is added, the program throws up because the study doesn't have
                # the new outcome in its meta-analytic unit object -- need to check
                # for this at runtime as we do with follow-up and outcome
                new_study = Study(self.max_study_id()+1)
                # issue #133 fix; exclude newly added studies by default
                new_study.include=False
                self.dataset.add_study(new_study)
                self.study_auto_added = int(new_study.id)
                study_added_due_to_edit = int(new_study.id)
                self.reset()
                # new_index is where the user *should* be editing.
                new_index = self.index(index.row(), index.column()+1)
                self.emit(SIGNAL("modelReset(QModelIndex)"), new_index)
        elif column == self.YEAR:
            year_ok, msg = self._verify_year(value.toString())
            if not year_ok:
                self.emit(SIGNAL("dataError(QString)"), QString(msg))
                return False
            study.year = value.toInt()[0]
        elif self.current_outcome is not None and column in self.RAW_DATA:
            data_ok, msg = self._verify_raw_data(value.toString(), column, current_data_type, index)
            if not data_ok:
                # this signal is (-- presumably --) handled by the UI
                # i.e., meta_form, which reports the problem to the
                # user. the model is not affected.
                self.emit(SIGNAL("dataError(QString)"), QString(msg))
                return False

            # @TODO make module-level constant?
            adjust_by = 3 # include study, study name, year columns
            ma_unit = self.get_current_ma_unit_for_study(index.row())
            group_name = self.current_txs[0]
            if current_data_type == BINARY:
                if column in self.RAW_DATA[2:]:
                    adjust_by += 2 
                    group_name = self.current_txs[1]
            elif current_data_type == CONTINUOUS:
                if column in self.RAW_DATA[3:]:
                    adjust_by += 3
                    group_name = self.current_txs[1]
            else:
                # diagnostic
                pass
                    
            adjusted_index = column-adjust_by
            val = value.toDouble()[0] if value.toDouble()[1] else ""
            ma_unit.tx_groups[group_name].raw_data[adjusted_index] = val # TODO: ENC
            
            # If a raw data column value is being edited, attempt to
            # update the corresponding outcome (if data permits)
            self.update_outcome_if_possible(index.row())
            
            
        elif column in self.OUTCOMES:
            print("Value %s in outcomes" % str(value.toString()))
            
            row = index.row()
            
            if value.toString().trimmed() == "":
                delete_value = True  
                display_scale_val = None
                calc_scale_val = None
            else:
                # sanity check -- is this a number?
                data_ok, msg = self._verify_outcome_data(value.toString(), column, row, current_data_type)
                if not data_ok and import_csv == False:
                    self.emit(SIGNAL("dataError(QString)"), QString(msg))
                    return False

                # the user can also explicitly set the effect size / CIs
                # @TODO what to do if the entered estimate contradicts the raw data?
                display_scale_val, converted_ok = value.toDouble()
                
                print("Display scale value: %s" % str(display_scale_val))

            if display_scale_val is None or converted_ok:
                if not self.is_diag():
                    # note that we convert from the display/continuous
                    # scale on which the metric is assumed to have been
                    # entered into the 'calculation' scale (e.g., log)
                    calc_scale_val = None
                    print("Input value is %s" % str(display_scale_val))

                    # Will be binary or continuous
                    calc_scale_val = self._get_calc_scale_value(display_scale_val,
                                                                data_type=current_data_type,
                                                                effect=self.current_effect)
                    conv_to_disp_scale = self._get_conv_to_display_scale(data_type=current_data_type,
                                                                         effect=self.current_effect)
                    
                    ma_unit = self.get_current_ma_unit_for_study(index.row())
                    if outcome_subtype == "generic_effect":
                        if column == self.OUTCOMES[0]: #estimate
                            ma_unit.set_effect(self.current_effect, group_str, calc_scale_val)
                            #ma_unit.set_display_effect(self.current_effect, group_str, display_scale_val)
                        elif column == self.OUTCOMES[1]: # se
                            ma_unit.set_SE(self.current_effect, group_str, calc_scale_val)
                            #ma_unit.set_display_se(self.current_effect, group_str, display_scale_val)
                    else: # normal case
                        if column == self.OUTCOMES[0]: # estimate
                            print("Setting estimate: %s" % str(calc_scale_val))
                            ma_unit.set_effect(self.current_effect, group_str, calc_scale_val)
                            #ma_unit.set_display_effect(self.current_effect, group_str, display_scale_val)
                        elif column == self.OUTCOMES[1]: #lower
                            ma_unit.set_lower(self.current_effect, group_str, calc_scale_val)
                            #ma_unit.set_display_lower(self.current_effect, group_str, display_scale_val)
                        else: #upper
                            ma_unit.set_upper(self.current_effect, group_str, calc_scale_val)
                            #ma_unit.set_display_upper(self.current_effect, group_str, display_scale_val)
                        print("calculating se")
                        
                        # in normal case, only calculate SE when all data is filled in
                        if None not in ma_unit.get_entered_effect_and_ci(self.current_effect, group_str):              
                            se = ma_unit.calculate_SE_if_possible(self.current_effect, group_str)
                            print("setting se to %s" % str(se))
                        else:
                            se = None
                        ma_unit.set_SE(self.current_effect, group_str, se)
                        
                    # Now calculate display_effect and CI
                    ma_unit.calculate_display_effect_and_ci(self.current_effect, group_str, conv_to_disp_scale)

                            
                else: #outcome is diagnostic
                    ma_unit = self.get_current_ma_unit_for_study(index.row())
                    # figure out if this column is sensitivity or specificity
                    m_str = "Sens"
                    if column in self.OUTCOMES[3:]:
                        # by convention, the last three columns are specificity
                        m_str = "Spec"
                        
                    calc_scale_val = self._get_calc_scale_value(display_scale_val,
                                                                data_type=current_data_type,
                                                                effect=m_str)
                    
                    # now we switch on what outcome column we're on ... kind of ugly, but eh.
                    if column in (self.OUTCOMES[0], self.OUTCOMES[3]):
                        ma_unit.set_effect(m_str, group_str, calc_scale_val)
                        #ma_unit.set_display_effect(m_str, group_str, display_scale_val)
                    elif column in (self.OUTCOMES[1], self.OUTCOMES[4]):
                        ma_unit.set_lower(m_str, group_str, calc_scale_val)
                        #ma_unit.set_display_lower(m_str, group_str, display_scale_val)    
                    else:
                        ma_unit.set_upper(m_str, group_str, calc_scale_val)
                        #ma_unit.set_display_upper(m_str, group_str, display_scale_val)
                    conv_to_display_scale = self._get_conv_to_display_scale(self,
                                                                            data_type=current_data_type,
                                                                            effect=m_str)
                    ma_unit.calculate_display_effect_and_ci(m_str, group_str, conv_to_disp_scale)
                    
        elif column == self.INCLUDE_STUDY:
            study.include = value.toBool()
            # we keep note if a study was manually 
            # excluded; this differs from just being
            # `included' because the latter is TRUE
            # automatically when a study first acquires
            # sufficient data to be included in an MA
            if not value.toBool():
                study.manually_excluded = True
        else:
            # then a covariate value has been edited.
            cov = self.get_cov(column)
            cov_name = cov.name
            new_value = None
            if cov.data_type == FACTOR:
                new_value = value.toString()
            else:
                # continuous
                new_value, converted_ok = value.toDouble()
                if not converted_ok: 
                    new_value = None
            study.covariate_dict[cov_name] = new_value
            
        self.emit(SIGNAL("dataChanged(QModelIndex, QModelIndex)"), index, index)

        # tell the view that an entry in the table has changed, and what the old
        # and new values were. This for undo/redo purposes.
        new_val = self.data(index)

        self.emit(SIGNAL("pyCellContentChanged(PyQt_PyObject, PyQt_PyObject, PyQt_PyObject, PyQt_PyObject)"), 
                           index, old_val, new_val, study_added_due_to_edit)
     
        if not self.is_diag():
            group_str = self.get_cur_group_str()

            print group_str
            print "ok checking it; cur outcome: %s. cur group: %s" % (self.current_outcome, group_str)
            if self.current_outcome is not None:
                effect_d = self.get_current_ma_unit_for_study(index.row()).effects_dict[self.current_effect][group_str]
                print effect_d
                
                
                # if the study has not been explicitly excluded by the user, then we automatically
                # include it once it has sufficient data.
                if not study.manually_excluded:
                    study.include = True
                    
                if current_data_type == CONTINUOUS and outcome_subtype == "generic_effect":
                    if None in [effect_d[key] for key in ["est","SE"]]:
                        study.include = False
                else: # normal case, binary or continuous
                    # if any of the effect values are empty, we cannot include this study in the analysis, so it
                    # is automatically excluded.
                    if any([val is None for val in [effect_d[effect_key] for effect_key in ("upper", "lower", "est")]]):
                        study.include = False
        return True
        
    
    #def _setData_NAME(self, value, index, study):
    #def _setData_YEAR(self):
    #def _setData_RAW_DATA(self):
    #def _setData_OUTCOMES(self):

    @staticmethod
    def helper_basic_horizontal_headerData(section, data_type, sub_type,
            raw_columns, outcome_columns, current_effect, groups, outcome_not_None=True):
        ''' Allows access to basic display role headerData information w/o
        having to make a data model '''
        
        if section == DatasetModel.INCLUDE_STUDY:
            return QVariant(DatasetModel.headers[DatasetModel.INCLUDE_STUDY])
        elif section == DatasetModel.NAME:
            return QVariant(DatasetModel.headers[DatasetModel.NAME])
        elif section == DatasetModel.YEAR:
            return QVariant(DatasetModel.headers[DatasetModel.YEAR])
        # note that we're assuming here that raw data
        # always shows only two tx groups at once.
        elif outcome_not_None and section in raw_columns:
            # switch on the outcome type 
            current_tx = groups[0] # i.e., the first group
            if data_type == BINARY:
                if section in raw_columns[2:]:
                    current_tx = groups[1]
                    
                if section in (raw_columns[0], raw_columns[2]):
                    return QVariant(current_tx + " #evts")
                else:
                    return QVariant(current_tx + " #total")
            elif data_type == CONTINUOUS:
                # continuous data
                if len(raw_columns) < 6:
                    return QVariant("")
                    
                if sub_type == "generic_effect":
                    return QVariant("")
                else:
                    if section in raw_columns[3:]:
                        current_tx = groups[1]
                    if section in (raw_columns[0], raw_columns[3]):
                        return QVariant(current_tx + " N")
                    elif section in (raw_columns[1], raw_columns[4]):
                        return QVariant(current_tx + " mean")
                    else:
                        return QVariant(current_tx + " SD")
            elif data_type == DIAGNOSTIC:
                # ordering per sir Tom Trikalinos
                # "it makes sense -- it goes like this in the matrix!"
                #       - (said while making bizarre gesticulation) Tom.
                if section == raw_columns[0]:
                    return QVariant("TP")
                elif section == raw_columns[1]:
                    return QVariant("FN")
                elif section == raw_columns[2]:
                    return QVariant("FP")
                else:
                    return QVariant("TN")
                    
        elif section in outcome_columns:
            if data_type == BINARY:
                # effect size, lower CI, upper CI
                if section == outcome_columns[0]:
                    return QVariant(current_effect)
                elif section == outcome_columns[1]:
                    return QVariant("lower")
                else:
                    return QVariant("upper")
            elif data_type == CONTINUOUS:
                if sub_type == "generic_effect":
                    if section == outcome_columns[0]:
                        return QVariant(current_effect)
                    if section == outcome_columns[1]:
                        return QVariant("se")
                else: # normal case with no outcome_subtype
                    if section == outcome_columns[0]:
                        return QVariant(current_effect)
                    elif section == outcome_columns[1]:
                        return QVariant("lower")
                    elif section == outcome_columns[2]:
                        return QVariant("upper")
            elif data_type == DIAGNOSTIC:
                #### 
                # we're going to do three columns per outcome
                #   est, lower, upper
                outcome_index = section - outcome_columns[0]
                outcome_headers = ["sens.", "lower", "upper", "spec.", "lower", "upper"]
                return QVariant(outcome_headers[outcome_index])
        
        return None # Only get here if section doesn't match
            

        

    def headerData(self, section, orientation, role=Qt.DisplayRole):
        '''
        Implementation of the abstract method inherited from the base table
        model class. This is responsible for providing header data for the
        respective columns.
        '''
    
        
        outcome_type = self.dataset.get_outcome_type(self.current_outcome)
        outcome_subtype = self.dataset.get_outcome_subtype(self.current_outcome)
        length_dataset = len(self.dataset)
        
        sectionOK = section < length_dataset
        ############################### TOOLTIPS ###############################
        if role == Qt.ToolTipRole:
            if orientation == QtCore.Qt.Horizontal:
                #return QtCore.QString("Horizontal Header %s Tooltip" % str(section))
                if section == self.INCLUDE_STUDY:
                    return QString("Check if you want to include this study in the meta-analysis")
                elif section == self.NAME:
                    return QString("Name to identify the study")
                elif section == self.YEAR:
                    return QString("Year of publication")
                elif self.current_outcome is not None and section in self.RAW_DATA:
                    # switch on the outcome type 
                    current_tx = self.current_txs[0] # i.e., the first group
                    
                    rename_col_msg = "\nRename group by right-clicking the column header and selecting 'rename group...'"
                    sort_msg = "\nSort on this column by right-clicking the column header and selecting 'sort studies...'"
                    if outcome_type == BINARY:
                        if section in self.RAW_DATA[2:]:
                            current_tx = self.current_txs[1]
            
                        if section in (self.RAW_DATA[0], self.RAW_DATA[2]):
                            num_events_msg = "# of Events in group {0} (numerator)".format(current_tx)
                            return QString(num_events_msg + rename_col_msg + sort_msg)
                        else:
                            num_sujets_msg = "# of Subjects in group {0} (numerator)".format(current_tx)
                            return QString(num_sujets_msg + rename_col_msg + sort_msg)
                    elif outcome_type == CONTINUOUS:
                        # continuous data
                        if outcome_subtype == "generic_effect":
                            # Logic note: we should never reach this point
                            return QString("leave me alone!")
                            
                        else: # normal case with no outcome subtype
                            if section in self.RAW_DATA[3:]:
                                current_tx = self.current_txs[1]
                                
                            if section in (self.RAW_DATA[0], self.RAW_DATA[3]):
                                N_sujets_msg = "# Subjects in group {0}".format(current_tx)
                                return QString(N_sujets_msg + rename_col_msg + sort_msg)
                            elif section in (self.RAW_DATA[1], self.RAW_DATA[4]):
                                mean_msg = "Mean of group %s" % current_tx
                                return QString(mean_msg + rename_col_msg + sort_msg)
                            else:
                                sd_msg = "Standard Deviation of group %s" % current_tx
                                return QString(sd_msg)
                    elif outcome_type == DIAGNOSTIC:
                        if section == self.RAW_DATA[0]:
                            return QString("# True Positives"  + sort_msg)
                        elif section == self.RAW_DATA[1]:
                            return QString("# False Negatives" + sort_msg)
                        elif section == self.RAW_DATA[2]:
                            return QString("# False Positives" + sort_msg)
                        else:
                            return QString("# True Negatives"  + sort_msg)
                elif section in self.OUTCOMES:
                    help_msg = "For information about how the confidence interval was obtained,\n"
                    help_msg += "please consult the the help at {0}".format(HELP_URL)
                    lower_msg = "Lower bound of {0:.1%} confidence interval".format(meta_globals.get_global_conf_level()/100.0)
                    lower_msg += "\n" + help_msg
                    upper_msg = "Upper bound of {0:.1%} confidence interval\n".format(meta_globals.get_global_conf_level()/100.0)
                    upper_msg += "\n" + help_msg
                    se_msg = "Standard Error"
                    
                    if outcome_type == BINARY:
                        # effect size, lower CI, upper CI
                        if section == self.OUTCOMES[0]:
                            return QString(BINARY_METRIC_NAMES[self.current_effect])
                        elif section == self.OUTCOMES[1]:
                            return QString(lower_msg)
                        else:
                            return QString(upper_msg)
                    elif outcome_type == CONTINUOUS:
                        if outcome_subtype == "generic_effect":
                            if section == self.OUTCOMES[0]:
                                return QString(CONTINUOUS_METRIC_NAMES[self.current_effect])
                            if section == self.OUTCOMES[1]:
                                return QString(se_msg)
                        else: # normal case with no outcome_subtype
                            if section == self.OUTCOMES[0]:
                                return QString(CONTINUOUS_METRIC_NAMES[self.current_effect])
                            elif section == self.OUTCOMES[1]:
                                return QString(lower_msg)
                            elif section == self.OUTCOMES[2]:
                                return QString(upper_msg)
   
                        
                    elif outcome_type == DIAGNOSTIC:
                        if section in (self.OUTCOMES[1],self.OUTCOMES[4]):
                            return QString(lower_msg)
                        elif section in (self.OUTCOMES[2],self.OUTCOMES[5]):
                            return QString(upper_msg)
                        else: # in metric name
                            if section == self.OUTCOMES[0]: # Sens
                                return QString(DIAGNOSTIC_METRIC_NAMES["Sens"])
                            elif section == self.OUTCOMES[3]: # Spec
                                return QString(DIAGNOSTIC_METRIC_NAMES["Spec"])
                    
            else: # vertical
                if sectionOK:
                    return QtCore.QString("Use calculator to fill-in missing information")
                
        # For cool calculator icon
        if role == Qt.DecorationRole:
            if orientation == Qt.Vertical and sectionOK:
                return QIcon(":/misc/calculator-34.png")
        
        if role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        #if role != Qt.DisplayRole:
        #    return QVariant()
        ############################# DISPLAY ROLE #############################
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                
                res = self.helper_basic_horizontal_headerData(
                            section,
                            data_type=outcome_type,
                            sub_type=outcome_subtype,
                            raw_columns=self.RAW_DATA,
                            outcome_columns=self.OUTCOMES,
                            current_effect=self.current_effect,
                            groups=self.current_txs,
                            outcome_not_None=self.current_outcome is not None)
                if res:
                    return res                
                elif self.current_outcome is not None and section > max(self.OUTCOMES):
                    # then the column is to the right of the outcomes, and must
                    # be a covariate.
                    ### issue #156 -- always show covariate type
                    cur_cov = self.get_cov(section)
                    if cur_cov == None:
                        return QVariant("")
                    
                    cov_name = cur_cov.name
                    cov_type = cur_cov.get_type_str()
                    # note that I'm only returning the *first* letter
                    # of the type (c or f) because the whole thing
                    # is too long..
                    return QVariant("%s (%s)" % (cov_name, cov_type[0]))
                else:
                    # pass, basically
                    return QVariant("")
            else: # vertical case
                # this is the vertical -- non-table header -- case.    
                # we just show row numbers (not zero-based; hence the +1).
                return QVariant(int(section+1))
            

        
        


    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        elif index.column() == self.INCLUDE_STUDY:
            return Qt.ItemFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled |
                                Qt.ItemIsUserCheckable | Qt.ItemIsSelectable)
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)

    def rowCount(self, index=QModelIndex()):
        return self.dataset.num_studies() + DUMMY_ROWS

    def columnCount(self, index=QModelIndex()):
        return self._get_col_count()

    def get_cov(self, table_col_index):
        # we map (ie, adjust) the table column index to the covariate
        # index. if there is currently an outcome, this means we
        # subtract off the indices up to the last outcomes column; otherwise
        # we just subtract the include, study name and year columns (giving 3)
        cov_index = table_col_index - (self.OUTCOMES[-1]+1) if self.current_outcome is not None else table_col_index - 3
        try:
            return self.dataset.covariates[cov_index]
        except:
            print("There is no covariate at that index")
            return None
        
    def get_covariate_names(self):
        return [cov.name for cov in self.dataset.covariates]

    def rename_covariate(self, old_cov_name, new_cov_name):
        old_cov_obj = self.dataset.get_cov_obj_from_name(old_cov_name)
        self.dataset.change_covariate_name(old_cov_obj, new_cov_name)
        self.reset()

    def _get_col_count(self):
        '''
        Calculate how many columns to display; this is contingent on the data type,
        amongst other things (e.g., number of covariates).
        '''
        num_cols = 3 # we always show study name and year (and include studies)
        if len(self.dataset.get_outcome_names()) > 0:
            num_effect_size_fields = 3 # point estimate, low, high
            outcome_type = self.dataset.get_outcome_type(self.current_outcome)
            outcome_subtype = self.dataset.get_outcome_subtype(self.current_outcome)
            if outcome_subtype == "generic_effect":
                num_effect_size_fields = 2 # point estimate, se
            if outcome_type == DIAGNOSTIC:
                # we have two for diagnostic; sensitivity and specifity.
                # we will display the est, lower, and upper for both of these.
                num_effect_size_fields = 6
            
            num_cols += num_effect_size_fields + self.num_data_cols_for_current_unit()
        # now add the covariates (if any)
        num_cols += len(self.dataset.covariates)
        return num_cols
        
    def get_ordered_study_ids(self):
        return [study.id for study in self.dataset.studies]

    def add_new_outcome(self, name, data_type, sub_type=None):
        data_type = STR_TO_TYPE_DICT[data_type.lower()]
        self.dataset.add_outcome(Outcome(name, data_type, sub_type=sub_type))

    def remove_outcome(self, outcome_name):
        self.dataset.remove_outcome(outcome_name)
        
    def add_new_group(self, name):
        self.dataset.add_group(name, self.current_outcome)
        
    def remove_group(self, group_name):
        self.dataset.remove_group(group_name)
    
    def rename_group(self, old_group_name, new_group_name):
        self.dataset.change_group_name(old_group_name, new_group_name)
        if old_group_name in self.current_txs:
            group_index = self.current_txs.index(old_group_name)
            # now remove the old group from the list of current groups
            self.current_txs.pop(group_index)
            self.current_txs.insert(group_index, new_group_name)
        self.reset()

    def add_follow_up_to_current_outcome(self, follow_up_name):
        self.dataset.add_follow_up_to_outcome(self.current_outcome, follow_up_name)
        
    def remove_follow_up_from_outcome(self, follow_up_name, outcome_name):
        self.dataset.remove_follow_up_from_outcome(follow_up_name, outcome_name)
        
    def add_covariate(self, covariate_name, covariate_type, cov_values=None):
        self.dataset.add_covariate(Covariate(covariate_name, covariate_type),
                                   cov_values=cov_values)
        self.reset()
    
    def remove_covariate(self, covariate_name):
        self.dataset.remove_covariate(covariate_name)
        self.reset()
        
    def remove_study(self, an_id):
        self.dataset.studies.pop(an_id)
        self.reset()

    def get_name(self):
        return self.dataset.title

    def get_next_outcome_name(self):
        outcomes = self.dataset.get_outcome_names()
        cur_index = outcomes.index(self.current_outcome)
        next_outcome = outcomes[0] if cur_index == len(outcomes)-1\
                                                        else outcomes[cur_index+1]
        return next_outcome

    def get_prev_outcome_name(self):
        outcomes = self.dataset.get_outcome_names()
        cur_index = outcomes.index(self.current_outcome)
        prev_outcome = outcomes[-1] if cur_index == 0 \
                                                        else outcomes[cur_index-1]
        return prev_outcome

    def get_next_follow_up(self):
        print "\nfollow ups for outcome:"
        print self.dataset.outcome_names_to_follow_ups[self.current_outcome]
        t_point = self.current_time_point
        if self.current_time_point >= max(self.dataset.outcome_names_to_follow_ups[self.current_outcome].keys()):
            t_point = 0
        else:
            # WARNING if we delete a time point things might get screwed up here
            # as we're actually using the MAX when we insert new follow ups
            # TODO change this to look for the next greatest time point rather than
            # assuming the current + 1 exists
            t_point += 1
        follow_up_name = self.get_follow_up_name_for_t_point(t_point)
        print "\nt_point; name: %s, %s" % (t_point, follow_up_name)
        return (t_point, follow_up_name)
        
    def get_previous_follow_up(self):
        t_point = self.current_time_point
        if self.current_time_point <= min(self.dataset.outcome_names_to_follow_ups[self.current_outcome].keys()):
            t_point = max(self.dataset.outcome_names_to_follow_ups[self.current_outcome].keys())
        else:
            # WARNING if we delete a time point things might get screwed up here
            # as we're actually using the MAX when we insert new follow ups
            # TODO change this to look for the next greatest time point rather than
            # assuming the current - 1 exists
            t_point -= 1
        return (t_point, self.get_follow_up_name_for_t_point(t_point))
        
    def set_current_time_point(self, time_point):
        self.current_time_point = time_point
        self.emit(SIGNAL("followUpChanged()"))
        self.reset()
        
    def set_current_follow_up(self, follow_up_name):
        t_point = self.dataset.outcome_names_to_follow_ups[self.current_outcome].get_key(follow_up_name)
        self.set_current_time_point(t_point)

    def get_current_follow_up_name(self):
        if len(self.dataset.outcome_names_to_follow_ups) > 0:
            try:
                return self.dataset.outcome_names_to_follow_ups[self.current_outcome][self.current_time_point]
            except:
                return None
            
    def get_follow_up_name_for_t_point(self, t_point):
        return self.dataset.outcome_names_to_follow_ups[self.current_outcome][t_point]
        
    def get_t_point_for_follow_up_name(self, follow_up):
        return self.dataset.outcome_names_to_follow_ups[self.current_outcome].get_key(follow_up)
        
    def get_current_groups(self):
        return self.current_txs
        
    def get_previous_groups(self):
        return self.previous_txs
        
    def next_groups(self):
        ''' Returns a tuple with the next two group names (we just iterate round-robin) '''
        if len(self.dataset.get_group_names()) == 0:
            return []
        
        ## notice that we only retrieve the group names that belong
        # to the current outcome/follow-up tuple
        group_names = self.dataset.get_group_names_for_outcome_fu(self.current_outcome, self.get_current_follow_up_name())
    
        self._next_group_indices(group_names)
        
        if not self.is_diag():
            # shuffle over groups
            while self.tx_index_a == self.tx_index_b:
                self._next_group_indices(group_names)
        else:
            self._next_group_index(group_names)
            
        next_txs = [group_names[self.tx_index_a], group_names[self.tx_index_b]]
        print "new tx group indices a, b: %s, %s" % (self.tx_index_a, self.tx_index_b)
        return next_txs
        

    def _next_group_indices(self, group_names):
        print "\ngroup names: %s" % group_names
        if self.tx_index_b < len(group_names)-1:
            self.tx_index_b += 1
        else:
            # bump the a index
            if self.tx_index_a < len(group_names)-1:
                self.tx_index_a += 1
            else:
                self.tx_index_a = 0
            self.tx_index_b = 0


    def _next_group_index(self, group_names):
        # increments tx A; ignores B
        if self.tx_index_a < len(group_names)-1:
            self.tx_index_a += 1
        else:
            self.tx_index_a = 0
            
    def outcome_has_follow_up(self, outcome, follow_up):
        if outcome is None:
            print("Tried to reference None outcome")
            return None
        outcome_d = self.dataset.outcome_names_to_follow_ups[outcome]

        return follow_up in outcome_d.keys()
        
    def outcome_fu_has_group(self, outcome, follow_up, group):
        ## we just pull the outcome from the first study; we tacitly
        # assume that all studies have the same outcomes/follow-ups.
        # 
        outcome_d = self.dataset.studies[0].outcomes_to_follow_ups[outcome]

        ## we _assume_ that the follow_up is in this outcome!
        return group in outcome_d[follow_up].tx_groups.keys()
    
    def set_current_groups(self, group_names):
        self.previous_txs = self.current_txs
        self.current_txs = group_names
        self.tx_index_a = self.dataset.get_group_names().index(group_names[0])
        self.tx_index_b = self.dataset.get_group_names().index(group_names[1])
        print "\ncurrent tx group index a, b: %s, %s" % (self.tx_index_a, self.tx_index_b)

    def sort_studies(self, col, reverse):
        if col == self.NAME:
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(compare_by="name", reverse=reverse), reverse=reverse)
        elif col == self.YEAR:
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(compare_by="year", reverse=reverse), reverse=reverse)
        elif col in self.RAW_DATA:
            #data_type = self.dataset.get_outcome_type(self.current_outcome)
            # need this to dig down to find right ma_unit and data we're looking for to compare against
            ma_unit_reference_info = {'outcome_name': self.current_outcome, 
                                      'follow_up': self.get_follow_up_name_for_t_point(self.current_time_point),
                                      'current_groups': self.get_current_groups(),
                                      'data_index': col - min(self.RAW_DATA)}
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(compare_by="raw_data", 
                reverse=reverse, directions_to_ma_unit=ma_unit_reference_info), reverse=reverse)
        elif col in self.OUTCOMES:
            # need this to dig down to find right ma_unit and data we're looking for to compare against
            ma_unit_reference_info = {
                'outcome_type': self.dataset.get_outcome_type(self.current_outcome),
                'outcome_name': self.current_outcome, 
                'follow_up': self.get_follow_up_name_for_t_point(self.current_time_point),
                'current_groups': self.get_current_groups(),
                'current_effect': self.current_effect,
                'group_str': self.get_cur_group_str(),
                'data_index': col - min(self.OUTCOMES)
            }
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(compare_by="outcomes", 
                reverse=reverse, directions_to_ma_unit=ma_unit_reference_info), reverse=reverse)
        
            
        # covariates -- note that we assume anything to the right of the outcomes
        # is a covariate
        elif col > self.OUTCOMES[-1]:
            cov = self.get_cov(col)
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(\
                                        compare_by=cov.name, reverse=reverse), reverse=reverse)

        self.reset()

    def order_studies(self, ids):
        ''' Shuffles studies vector to the order specified by ids'''
        ordered_studies = []
        for an_id in ids:
            for study in self.dataset.studies:
                if study.id == an_id:
                    ordered_studies.append(study)
                    break
        self.dataset.studies = ordered_studies
        self.reset()

    def set_current_outcome(self, outcome_name):
        self.current_outcome = outcome_name
        self.update_column_indices()
        self.update_cur_tx_effect()
        self.emit(SIGNAL("outcomeChanged()"))
        self.reset()
        
    def update_cur_tx_effect(self):
        outcome_type = self.dataset.get_outcome_type(self.current_outcome)
        if outcome_type == BINARY:
            self.current_effect = "OR"
        elif outcome_type == CONTINUOUS:
            self.current_effect = "MD"
        else:
            # diagnostic -- what should we do here? we show
            # sensitivity/specificity; I don't think there's a
            # notion of a `current effect'...
            self.current_effect = None
        
    def max_study_id(self):
        return self.dataset.max_study_id()

    def num_data_cols_for_current_unit(self):
        '''
        Returns the number of columns needed to display the raw data
        given the current data type (binary, etc.)
        
        Note again that outcome names are necessarily unique!
        '''
        data_type = self.dataset.get_outcome_type(self.current_outcome)
        sub_type  = self.dataset.get_outcome_subtype(self.current_outcome)
        if data_type is None:
            return 0
        elif data_type in [BINARY, DIAGNOSTIC, OTHER]:
            return 4
        elif data_type == CONTINUOUS:
            if sub_type == "generic_effect":
                return 0 # no raw data for generic effect
            else:
                return 6
            

    def get_current_outcome_type(self, get_str=True):
        ''' Returns the type of the currently displayed (or 'active') outcome (e.g., binary).  '''
        return self.dataset.get_outcome_type(self.current_outcome, get_string=get_str)
    def get_current_outcome_subtype(self):
        return self.dataset.get_outcome_subtype(self.current_outcome)

    def _set_standard_cols(self, d):
        ''' these are immutable '''
        # column indices
        d["NAME"] = self.NAME
        d["YEAR"] = self.YEAR
        d["RAW_DATA"] = self.RAW_DATA
        d["OUTCOMES"] = self.OUTCOMES
        d["HEADERS"] = self.headers
        return d

    def make_reasonable_stateful_dict(self, data_model):
        d = {}
        d = self._set_standard_cols(d)

        # now take guesses/pick randomly for the remaining
        # fields
        d["current_outcome"] = data_model.get_outcome_names()[0] 
        d["current_time_point"] = data_model.get_follow_up_names()[0]


        # just pick a reasonable current effect,
        # given the outcome data type
        data_type = data_model.get_outcome_type(d["current_outcome"])
        
        print("data_type: ", data_type)

        all_txs = data_model.get_group_names()

        if data_type == DIAGNOSTIC:
            d["current_txs"] = [all_txs[0]]
        else:
            d["current_txs"] = [all_txs[0], all_txs[1]]

        effect = None # this is ignored for diagnostic
        if data_type == BINARY:
            effect = "OR"
        elif data_type == CONTINUOUS:
            effect = "SMD"
        # make sure you call change_metric_if_appropriate
        # after setting this as the state_dict
        d["current_effect"] = effect
        d["study_auto_added"] = False # hmm ?
        return d


    def get_stateful_dict(self):
        '''
        This captures the state of the model view; things like the current outcome
        and column indices that are on the QT side of the data table model.
        '''
        d = {}
        d = self._set_standard_cols(d)

        # currently displayed outcome, etc
        d["current_outcome"] = self.current_outcome
        d["current_time_point"] = self.current_time_point
        d["current_txs"] = self.current_txs
        d["current_effect"] = self.current_effect
        d["study_auto_added"] = self.study_auto_added
        
        return d

    def is_diag(self):
        ''' Convenience method -- just returns if the ma_dataset is a diagnostic dataset or not '''
        return self.dataset.is_diag
        
    def set_state(self, state_dict):
        for key, val in state_dict.items():
            exec("self.%s = val" % key)

        self.reset()

    def raw_data_is_complete_for_study(self, study_index, first_arm_only=False):         
        raw_data = self._get_raw_data_according_to_arms(study_index, first_arm_only)
        
        raw_data_is_complete = not "" in raw_data and not None in raw_data
        return raw_data_is_complete
    
    def _raw_data_is_not_empty_for_study(self, study_index, first_arm_only=False):
        raw_data = self._get_raw_data_according_to_arms(study_index, first_arm_only)
    
        empty = True
        for x in raw_data:
            if x not in meta_globals.EMPTY_VALS:
                empty = False

        return not empty      

    def _get_raw_data_according_to_arms(self, study_index, first_arm_only = False):
        if self.current_outcome is None or self.current_time_point is None:
            return False

        raw_data = self.get_cur_raw_data_for_study(study_index)
        data_type = self.get_current_outcome_type(get_str=False)
        # if first_arm_only is true, we are only concerned with whether
        # or not there is sufficient raw data for the first arm of the study
        
        if first_arm_only:
            if data_type == BINARY:
                raw_data = raw_data[:2]
            elif data_type == CONTINUOUS:
                raw_data = raw_data[:3]
        return raw_data

    def data_for_only_one_arm(self):
        '''
        really this should read 'data for one *and only one* arm.
        '''
        data_for_arm_one, data_for_arm_two = False, False

        data_type = self.get_current_outcome_type(get_str=False)
        per_group_raw_data_size = 2 if data_type == BINARY else 3

        for study_index in range(len(self.dataset.studies)):
            cur_raw_data = self.get_cur_raw_data_for_study(study_index)
            
            if len([x for x in cur_raw_data[:per_group_raw_data_size] if x is not None and x!='']) > 0:
                data_for_arm_one = True
            if len([x for x in cur_raw_data[per_group_raw_data_size:] if x is not None and x!='']) > 0:
                data_for_arm_two = True

        return (data_for_arm_one and not data_for_arm_two) or\
                 (data_for_arm_two and not data_for_arm_one)

    @DebugHelper
    def try_to_update_outcomes(self):
        for study_index in range(len(self.dataset.studies)):
            self.update_outcome_if_possible(study_index)

        
    def blank_all_studies(self, include_them):
        # note that we do *not* change the status of the
        # last study, because this is assumed to be an
        # auto-added (blank) study. formerly, when
        # 'include all' was used, this was being flipped
        # to true for the empty studies, causing issues.
        # this is a fix for issue #178
        for study in self.dataset.studies[:-1]:
            study.include=include_them
    
    ###
    # syntactic high-fructose corn syrup
    def include_all_studies(self):
        self.blank_all_studies(True)
    
    def exclude_all_studies(self):
        self.blank_all_studies(False)

    def all_studies_are_included(self):
        return all([study.include for study in self.dataset.studies])
    
    def all_studies_are_excluded(self):
        return all([not study.include for study in self.dataset.studies])


    def update_outcome_if_possible(self, study_index):
        '''
        Rules:
            Checks the parametric study to ascertain if enough raw data has been
            entered to compute the outcome. If so, the outcome is computed and
            displayed.
            
            If the raw data is not empty, the outcome should be blanked out.
            If the raw data is empty, the outcome should not be effected
        '''
        est_and_ci_d = None
        # to index into the effect belonging to the currently displayed groups
        group_str = self.get_cur_group_str() 
        data_type = self.get_current_outcome_type(get_str=False) 
        one_arm_effect = self.current_effect in BINARY_ONE_ARM_METRICS + CONTINUOUS_ONE_ARM_METRICS  
        ma_unit = self.get_current_ma_unit_for_study(study_index)

        ####
        # previously we were always setting this to false here,
        # but below we check only for raw data. in fact,
        # we only want to force an exclude if there is no
        # raw data *and* no manually entered point estimate/CI
        if data_type == DIAGNOSTIC or not self.study_has_point_est(study_index):
            self.dataset.studies[study_index].include = False

        # we try to compute outcomes if either all raw data is there, or, if we have a one-arm
        # metric then if sufficient raw data exists to compute this
        if self.raw_data_is_complete_for_study(study_index) or \
                (one_arm_effect and self.raw_data_is_complete_for_study(study_index, first_arm_only=True)):
            
            if not self.dataset.studies[study_index].manually_excluded:
                # include the study -- note that if the user excluded the study, then
                # edited the raw data, this will re-include it automatically
                self.dataset.studies[study_index].include = True

            if data_type == BINARY:
                e1, n1, e2, n2 = self.get_cur_raw_data_for_study(study_index)
                if self.current_effect in BINARY_TWO_ARM_METRICS:
                    est_and_ci_d = meta_py_r.effect_for_study(e1, n1, e2, n2,
                                                              metric=self.current_effect,
                                                              conf_level=meta_globals.get_global_conf_level())
                else:
                    # binary, one-arm
                    est_and_ci_d = meta_py_r.effect_for_study(e1, n1, 
                                                              two_arm=False,
                                                              metric=self.current_effect,
                                                              conf_level=meta_globals.get_global_conf_level())
            elif data_type == CONTINUOUS:
                n1, m1, sd1, n2, m2, sd2 = self.get_cur_raw_data_for_study(study_index)
                if self.current_effect in CONTINUOUS_TWO_ARM_METRICS:
                    est_and_ci_d = meta_py_r.continuous_effect_for_study(n1, m1, sd1,
                                        n2=n2, m2=m2, sd2=sd2, metric=self.current_effect, conf_level=meta_globals.get_global_conf_level())
                else:
                    # continuous, one-arm metric
                    est_and_ci_d = meta_py_r.continuous_effect_for_study(n1, m1, sd1,
                                          two_arm=False, metric=self.current_effect, conf_level=meta_globals.get_global_conf_level())
                
            elif data_type == DIAGNOSTIC: 
                # diagnostic data
                tp, fn, fp, tn = self.get_cur_raw_data_for_study(study_index)

                # sensitivity and specificity
                ests_and_cis = meta_py_r.diagnostic_effects_for_study(
                                                  tp, fn, fp, tn,
                                                  metrics=DIAGNOSTIC_METRICS,
                                                  conf_level=meta_globals.get_global_conf_level())
                
                ###
                # now we're going to set the effect estimate/CI on the MA object.
                
                for metric in DIAGNOSTIC_METRICS:
                    est, lower, upper = ests_and_cis[metric]["calc_scale"]
                    ma_unit.set_effect_and_ci(metric, group_str, est, lower, upper)
                    conv_to_disp_scale = self._get_conv_to_display_scale(data_type, effect=metric)
                    ma_unit.calculate_display_effect_and_ci(metric, group_str, conv_to_disp_scale)
                
            ####
            # if we're dealing with continuous or binary data, here
            # is where we update the point estimates -- we do this 
            # above in the case of diagnostic data, which needs to be
            # handled differently, because we're updating two
            # outcomes, in that case
            if data_type != DIAGNOSTIC:
                est, lower, upper = None, None, None
                if est_and_ci_d is not None:
                    est, lower, upper = est_and_ci_d["calc_scale"] # calculation scale
                # now set the effect size & CIs
                # note that we keep two versions around; a version on the 'calculation' scale
                # (e.g., log) and a version on the continuous/display scale to present to the
                # user via the UI.
                ma_unit.set_effect_and_ci(self.current_effect, group_str, est, lower, upper)
                conv_to_disp_scale = self._get_conv_to_display_scale(data_type, effect=self.current_effect)
                ma_unit.calculate_display_effect_and_ci(self.current_effect, group_str, conv_to_disp_scale)
        elif self._raw_data_is_not_empty_for_study(study_index) or (one_arm_effect and self._raw_data_is_not_empty_for_study(study_index, first_arm_only=True)):
            if data_type in [BINARY, CONTINUOUS]: # raw data is not blank but not full so clear outcome
                est, lower, upper, se = None, None, None, None
                ma_unit.set_effect_and_ci(self.current_effect, group_str, est, lower, upper)
                ma_unit.set_SE(self.current_effect, group_str, se)
                conv_to_disp_scale = self._get_conv_to_display_scale(data_type, effect=self.current_effect)
                ma_unit.calculate_display_effect_and_ci(self.current_effect, group_str, conv_to_disp_scale)
        else: # raw data is all blank, do nothing
            pass
                
    def get_cur_raw_data(self, only_if_included=True, only_these_studies=None):
        raw_data = []
        
        for study_index in range(len(self.dataset.studies)):
            if not only_if_included or self.dataset.studies[study_index].include:
                if only_these_studies is None or self.dataset.studies[study_index].id in only_these_studies:
                    raw_data.append(self.get_cur_raw_data_for_study(study_index))

        return raw_data
                
    def included_studies_have_raw_data(self):
        ''' 
        True iff all _included_ studies have all raw data (e.g., 2x2 for binary) for the currently
        selected outcome and tx groups.

        Note that if the current metric is a *one-arm* metric, we only check the first
        arm; i.e., a study is considered to have raw data in this case if the active arm
        has data.
        '''

        one_arm_data = self.current_effect in ONE_ARM_METRICS

        # the -1 is again accounting for the last (empty) appended study
        for study_index in range(len(self.dataset.studies)-1):
            if self.dataset.studies[study_index].include:
                if not self.raw_data_is_complete_for_study(study_index,\
                                                            first_arm_only=one_arm_data):
                    return False
        return True


    def study_has_point_est(self, study_index, effect=None):
        group_str = self.get_cur_group_str()
        effect = effect or self.current_effect
        cur_ma_unit = self.get_current_ma_unit_for_study(study_index)
        
        if None in cur_ma_unit.get_effect_and_se(effect, group_str):
            print "study %s does not have a point estimate" % study_index
            return False
            
        return "ok -- has all point estimates"
        return True
        
    def cur_point_est_and_SE_for_study(self, study_index, effect=None):
        group_str = self.get_cur_group_str()
        cur_ma_unit = self.get_current_ma_unit_for_study(study_index)
        effect = effect or self.current_effect
        
        est = cur_ma_unit.get_estimate(effect, group_str)
        se = cur_ma_unit.get_se(effect, group_str)
        return (est, se)
        
    def get_cur_ests_and_SEs(self, only_if_included=True, only_these_studies=None, effect=None):
        ests, SEs = [], []
        effect = effect or self.current_effect
        for study_index in xrange(len(self.dataset.studies)):
            if only_these_studies is None or self.dataset.studies[study_index].id in only_these_studies:
                # issue #171 -- blank studies are *wrongly* set to be included after paste
                if not only_if_included or self.dataset.studies[study_index].include:
                    est, SE = self.cur_point_est_and_SE_for_study(study_index, effect=effect)
                    ests.append(est)
                    SEs.append(SE)
        return (ests, SEs)
        
    def included_studies_have_point_estimates(self, effect=None):
        ''' 
        True iff all included studies have all point estiamtes (and CIs) for the
        the 'effect' outcome and currently displayed tx groups. (If effect is None,
        this sets the 'effect' to the currently selected effect).
        '''
        for study_index in range(len(self.dataset.studies)-1):
            if self.dataset.studies[study_index].include:
                if not self.study_has_point_est(study_index, effect=effect):
                    return False
        return True    
        
    def get_studies(self, only_if_included=True):
        included_studies = []

        for study in self.dataset.studies:
            if not only_if_included or study.include:
                included_studies.append(study)
        # we lop off the last entry because it is always a blank line/study
        # 11/18/11 -- arg! previously we were explicitly lopping off
        # the last study (presumed to be blank). this is not necessary! 
        # we already check if it's included...
        return list(included_studies)      

    def get_cur_raw_data_for_study(self, study_index):
        return self.get_current_ma_unit_for_study(study_index).get_raw_data_for_groups(self.current_txs)

    def set_current_ma_unit_for_study(self, study_index, new_ma_unit):
        # note that we just assume this exists.
        self.dataset.studies[study_index].outcomes_to_follow_ups[self.current_outcome][self.get_current_follow_up_name()]=new_ma_unit
        
    def get_current_ma_unit_for_study(self, study_index):
        '''
        Returns the MetaAnalytic unit for the study @ study_index. If no such Unit exists,
        it will be added. Thus when a new study is added to a dataset, there is no need
        to initially populate this study with empty MetaAnalytic units reflecting the known
        outcomes, time points & tx groups, as they will be added 'on-demand' here.
        '''
        study = self.dataset.studies[study_index]
        
        # first check to see that the current outcome is contained in this study
        if not self.current_outcome in study.outcomes_to_follow_ups:
            ###
            # Issue 7 (RESOLVED) http://github.com/bwallace/OpenMeta-analyst-/issues/#issue/7
            study.add_outcome(self.dataset.get_outcome_obj(self.current_outcome),
                              group_names=self.dataset.get_group_names())
        
        # we must also make sure the time point exists. note that we use the *name* rather than the 
        # index of the current time/follow up
        if not self.get_current_follow_up_name() in study.outcomes_to_follow_ups[self.current_outcome]:
            study.add_outcome_at_follow_up(self.dataset.get_outcome_obj(self.current_outcome),
                                           self.get_current_follow_up_name())
        
        # finally, make sure the studies contain the currently selected tx groups; if not, add them
        ma_unit = study.outcomes_to_follow_ups[self.current_outcome][self.get_current_follow_up_name()]
        for tx_group in self.current_txs:
            if not tx_group in ma_unit.get_group_names():
                ma_unit.add_group(tx_group)
        
        return ma_unit


    def get_ma_unit(self, study_index, outcome, follow_up):
        try:
            return self.dataset.studies[study_index].outcomes_to_follow_ups[outcome][follow_up]
        except:
            raise Exception, "whoops -- you're attempting to access raw data for a study, outcome \
                                        or time point that doesn't exist."
        
    def max_raw_data_cols_for_current_unit(self):
        '''
        Returns the length of the biggest raw data list for the parametric ma_unit. e.g.,
        if a two group, binary outcome is the current ma_unit, then the studies should
        raw data vectors that contain, at most, 4 elements.
        '''
        return \
          max([len(\
            study.outcomes_to_follow_ups[self.current_outcome][self.current_time_point].get_raw_data_for_groups(self.current_txs)\
          ) for study in self.dataset.studies if self.current_outcome in study.outcomes_to_follow_ups])

    def recalculate_display_scale(self):
        effect = self.current_effect
        group_str = self.get_cur_group_str()
        current_data_type = self.dataset.get_outcome_type(self.current_outcome)
        
        ma_units = []
        # Gather ma_units for spreadsheet
        for study_index in range(len(self.dataset.studies)-1): #-1 is because last study is always blank
            ma_units.append(self.get_current_ma_unit_for_study(study_index))
            
        binary_display_scale = lambda x: meta_py_r.binary_convert_scale(x, self.current_effect, convert_to="display.scale")
        continuous_display_scale = lambda x: meta_py_r.continuous_convert_scale(x, self.current_effect, convert_to="display.scale")
                            
        def get_diagnostic_display_scale(m_str):
            return lambda x: meta_py_r.diagnostic_convert_scale(x, m_str, convert_to="display.scale") 
        
        for index, x in enumerate(ma_units):
            print("Recalculating display scale for ma_unit %d" % index)

            if current_data_type in [BINARY,CONTINUOUS]:
                if current_data_type == BINARY:
                    convert_to_display_scale = binary_display_scale
                elif current_data_type == CONTINUOUS:
                    convert_to_display_scale = continuous_display_scale
                x.calculate_display_effect_and_ci(effect, group_str,
                                                  convert_to_display_scale,
                                                  check_if_necessary=True)
            elif current_data_type == DIAGNOSTIC:
                for m_str in ["Sens","Spec"]:
                    x.calculate_display_effect_and_ci(m_str, group_str,
                                                      convert_to_display_scale=get_diagnostic_display_scale(m_str),
                                                      check_if_necessary=True)
        print("Finished calculating display effect and cis")

    def _get_conv_to_display_scale(self, data_type, effect):
        ''' Returns appropriate conv_to_display_scale function '''
        
        if None in [data_type, effect]:
            print("_get_conv_to_display_scale got None for either data_type, or effect")

        if data_type == BINARY:
            conv_to_disp_scale = lambda x: meta_py_r.binary_convert_scale(x, effect, convert_to="display.scale")
        elif data_type == CONTINUOUS:
            conv_to_disp_scale = lambda x: meta_py_r.continuous_convert_scale(x, effect, convert_to="display.scale")
        elif data_type == DIAGNOSTIC:
            conv_to_disp_scale = lambda x: meta_py_r.diagnostic_convert_scale(x, effect, convert_to="display.scale")
        else:
            raise Exception("_get_conv_to_display_scale: data type not recognized!")
        
        return conv_to_disp_scale
    
    def _get_calc_scale_value(self, display_scale_val=None, data_type=None, effect=None):
        ''' Gets the calc-scale value of the given display_scale value'''
        
        if None in [display_scale_val, data_type, effect]:
            print("_get_calc_scale_value got None for either display_scale_val, data_type, or effect")
        
        calc_scale_val = None
        if data_type == BINARY:
            calc_scale_val = meta_py_r.binary_convert_scale(display_scale_val, effect, convert_to="calc.scale")
        elif data_type == CONTINUOUS:
            calc_scale_val = meta_py_r.continuous_convert_scale(display_scale_val, effect, convert_to="calc.scale")
        elif data_type == DIAGNOSTIC:
            calc_scale_val = meta_py_r.diagnostic_convert_scale(display_scale_val, effect, convert_to="calc.scale")   
        else:
                raise Exception("_get_calc_scale_value: data type not recognized!")
            
        return calc_scale_val
            
