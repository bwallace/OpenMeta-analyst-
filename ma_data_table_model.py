#################################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Proxy class, interfaces between the underlying representation (in ma_dataset.py)
#  and the DataTableView UI. Basically deals with keeping track of which outcomes/
#  follow-ups/treatments are being viewed. 
################################################################

import PyQt4
from PyQt4 import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import ma_dataset 
from ma_dataset import *
import meta_py_r

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
    def __init__(self, filename=QString(), dataset=None):
        super(DatasetModel, self).__init__()

        self.dataset = dataset or Dataset()
        # include an extra blank study to begin with
        self.dataset.studies.append(Study(self.max_study_id() +1))
        
        # these variables track which meta-analytic unit, 
        # i.e., outcome and time period, are being viewed
        self.current_outcome = None 
        self.current_time_point = 0
        self.current_txs = ["treated", "control"]
        # @TODO parameterize; make variable
        self.current_effect = "OR" 
        
        #
        # column indices; these are a core component of this class, 
        # as these indices are what maps the UI to the model. They
        # are also variable, contingent on the type data being displayed,
        # the number of covariates, etc. Thus it is extremely important
        # these are set and maintained correctly
        self.NAME, self.YEAR = range(2)
        self.RAW_DATA = [col+2 for col in range(4)]
        self.OUTCOMES = [6, 7, 8]
        # @TODO presumably the COVARIATES will contain the column
        # indices and the currently_displayed... will contain the names
        # of the covariates being displayed in said columns, in order
        self.COVARIATES = None
        self.currently_displayed_covariates = []
        
        # @TODO
        self.LABELS = None
        self.headers = ["study name", "year"]
        
        self.NUM_DIGITS = 3
        self.study_auto_added = None
        
    def data(self, index, role=Qt.DisplayRole):
        if not index.isValid() or not (0 <= index.row() < len(self.dataset)):
            return QVariant()
        study = self.dataset.studies[index.row()]
        column = index.column()
        if role == Qt.DisplayRole:
            if column == self.NAME:
                return QVariant(study.name)
            elif column == self.YEAR:
                if study.year == 0:
                    return QVariant("")
                else:
                    return QVariant(study.year)
            elif column in self.RAW_DATA:
                adjusted_index = column - 2
                if self.current_outcome in study.outcomes_to_follow_ups:
                    cur_raw_data = self.get_current_ma_unit_for_study(index.row()).\
                                                        get_raw_data_for_groups(self.current_txs)
                    if len(cur_raw_data) > adjusted_index:
                        return QVariant(cur_raw_data[adjusted_index])
                    else:
                        return QVariant("")
                else:
                    return QVariant("")
            elif column in self.OUTCOMES:
                # either the point estimate, or the lower/upper
                # confidence interval
                outcome_index = column - self.OUTCOMES[0]
                est_and_ci = self.get_current_ma_unit_for_study(index.row()).\
                                                get_effect_and_ci(self.current_effect)
                outcome_val = est_and_ci[outcome_index]  
                if outcome_val is None:
                    return QVariant("")
                return QVariant(round(outcome_val, self.NUM_DIGITS))
            
        elif role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        elif role == Qt.BackgroundColorRole:
            if column in self.OUTCOMES:
                return QVariant(QColor(Qt.yellow))
            else:
                return QVariant(QColor(Qt.white))
        return QVariant()
       
             
    def setData(self, index, value, role=Qt.EditRole):
        '''
        Implementation of the AbstractDataTable method. The view uses this method
        to request data to display. Thus we here return values to render in the table
        based on the index (row, column). 
        
        For more, see: http://doc.trolltech.com/4.5/qabstracttablemodel.html
        '''
        if index.isValid() and 0 <= index.row() < len(self.dataset):
            column = index.column()
            old_val = self.data(index)
            if column in (self.NAME, self.YEAR):
                study = self.dataset.studies[index.row()]
                if column == self.NAME:
                    study.name = value.toString()
                    if study.name!="" and index.row() == self.rowCount()-1:
                        # if the last study was just edited, append a 
                        # new, blank study 
                        new_study = Study(self.max_study_id()+1)
                        self.dataset.add_study(new_study) 
                        self.study_auto_added = new_study.id
                        self.reset()
                else:
                    study.year = value.toInt()[0]
            elif column in self.RAW_DATA:
                adjust_by = 2 # study name, year columns
                ma_unit = self.get_current_ma_unit_for_study(index.row())
                group_name = self.current_txs[0]
                if column in self.RAW_DATA[2:]:
                    # @TODO this (the 2) is assuming binary data!
                    # second group
                    adjust_by += 2 # @TODO again, assuming binary here
                    group_name = self.current_txs[1]
                
                adjusted_index = column-adjust_by
                val = value.toDouble()[0] if value.toDouble()[1] else ""
                ma_unit.tx_groups[group_name].raw_data[adjusted_index] = val
                # If a raw data column value is being edit, attempt to
                # update the corresponding outcome (if data permits)
                self.update_outcome_if_possible(index.row())
            elif column in self.OUTCOMES:
                # @TODO what to do if the entered estimate contradicts the raw data?
                if column == self.OUTCOMES[0]:
                    ma_unit = self.get_current_ma_unit_for_study(index.row())
                    # the user can also explicitly set the effect size
                    if value.toDouble()[1]:
                        ma_unit.set_effect(self.current_effect, value.toDouble()[0])

                                    
            self.emit(SIGNAL("dataChanged(QModelIndex, QModelIndex)"), index, index)
            
            # tell the view that an entry in the table has changed, and what the old 
            # and new values were. this for undo/redo purposes. 
            new_val = self.data(index)
            self.emit(SIGNAL("cellContentChanged(QModelIndex, QVariant, QVariant)"), index, old_val, new_val)
            
            return True
        return False
        
        
    def headerData(self, section, orientation, role=Qt.DisplayRole):
        '''
        Implementation of the abstract method inherited from the base table
        model class. This is responsible for providing header data for the
        respective columns.
        '''
        if role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        if role != Qt.DisplayRole:
            return QVariant()
        if orientation == Qt.Horizontal:
            if section == self.NAME:
                return QVariant(self.headers[self.NAME])
            elif section == self.YEAR:
                return QVariant(self.headers[self.YEAR])
            # note: we're assuming here that raw data 
            # always shows only two tx groups at once.
            elif section in self.RAW_DATA[:2]:
                # i.e., the first group
                if section == self.RAW_DATA[0]:
                    return QVariant(self.current_txs[0] + " n")
                else:
                    return QVariant(self.current_txs[0] + " N")
            elif section in self.RAW_DATA[2:]:
                # second group
                if section == self.RAW_DATA[2]:
                    return QVariant(self.current_txs[1] + " n")
                else:
                    return QVariant(self.current_txs[1] + " N")
            elif section in self.OUTCOMES:
                if section == self.OUTCOMES[0]: 
                    # effect size
                    return QVariant(self.current_effect)
                elif section == self.OUTCOMES[1]:
                    return QVariant("lower")
                else:
                    return QVariant("upper")
                    
        return QVariant(int(section+1))
        

    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)
        
    
    def rowCount(self, index=QModelIndex()):
        return self.dataset.num_studies()
    
    def columnCount(self, index=QModelIndex()):
        return self._get_col_count()
       
    def _get_col_count(self):
        '''
        Calculate how many columns to display; this is contingent on the data type,
        amongst other things (e.g., number of covariates).
        '''
        num_cols = 2 # we always show study name and year
        if self.current_outcome is None:
            return num_cols
        else:
            num_effect_size_fields = 3 # point estimate, low, high
            num_cols += num_effect_size_fields + self.num_data_cols_for_current_unit()
            return num_cols
            
    def get_ordered_study_ids(self):
        return [study.id for study in self.dataset.studies]
        
    def add_new_outcome(self, name, data_type):
        data_type = STR_TO_TYPE_DICT[data_type.lower()]
        self.dataset.add_outcome(Outcome(name, data_type))
        
    def remove_study(self, id):
        self.dataset.studies.pop(id)
        self.reset()
        
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
        
    def sort_studies(self, col, reverse):
        if col == self.NAME:
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(compare_by="name", reverse=reverse), reverse=reverse)
        elif col == self.YEAR:
            self.dataset.studies.sort(cmp = self.dataset.cmp_studies(compare_by="year", reverse=reverse), reverse=reverse)
        self.reset()
        
    def order_studies(self, ids):
        ''' Shuffles studies vector to the order specified by ids'''
        ordered_studies = []
        for id in ids:
            for study in self.dataset.studies:
                if study.id == id:
                    ordered_studies.append(study)
                    break
        self.dataset.studies = ordered_studies
        self.reset()
    
    def set_current_outcome(self, outcome_name):
        self.current_outcome = outcome_name
        self.emit(SIGNAL("outcomeChanged()"))
        self.reset()
        
    def set_current_time_point(self, time_point):
        self.current_time_point = time_point
        self.emit(SIGNAL("followUpChanged()"))
        self.reset()
        
    def max_study_id(self):
        if len(self.dataset.studies) == 0:
            return -1
        return max([study.id for study in self.dataset.studies])
        
    def num_data_cols_for_current_unit(self):
        ''' 
        Returns the number of columns needed to display the raw data
        given the current data type (binary, etc.)
        '''
        data_type = self.dataset.get_outcome_type(self.current_outcome)
        if data_type is None:
            return 0
        elif data_type in [BINARY, DIAGNOSTIC, OTHER]:
            return 4
        else:
            # continuous
            return 6
            
    def get_stateful_dict(self):
        '''
        This captures the state of the model view; things like the current outcome 
        and column indices that are on the QT side of the data table model.
        
        @TODO we're going to need to handle covariates here eventually
        '''
        d = {}
        
        #
        # column indices
        #
        d["NAME"] = self.NAME
        d["YEAR"] = self.YEAR
        d["RAW_DATA"] = self.RAW_DATA
        d["OUTCOMES"] = self.OUTCOMES
        d["HEADERS"] = self.headers
        
        #
        # currently displayed outcome, etc
        #
        d["current_outcome"] = self.current_outcome
        d["current_time_point"] = self.current_time_point
        d["current_txs"] = self.current_txs
        d["current_effect"] = self.current_effect
        
        d["study_auto_added"] = self.study_auto_added
        return d
        
    def set_state(self, state_dict):
        for key, val in state_dict.items():
            exec("self.%s = val" % key)

        self.reset()

    def raw_data_is_complete_for_study(self, study_index):
        if self.current_outcome is None or self.current_time_point is None:
            return False
        raw_data = self.get_current_ma_unit_for_study(study_index).\
                            get_raw_data_for_groups(self.current_txs)
        return not "" in raw_data
                            
    def try_to_update_outcomes(self):
        for study_index in range(len(self.dataset.studies)):
            self.update_outcome_if_possible(study_index)
    
    def update_outcome_if_possible(self, study_index):
        ''' 
        Checks the parametric study to ascertain if enough raw data has been
        entered to compute the outcome. If so, the outcome is computed and
        displayed.
        '''
        est, lower, upper = None, None, None
        if self.raw_data_is_complete_for_study(study_index):
            e1, n1, e2, n2 = self.get_cur_raw_data_for_study(study_index)
            est, lower, upper = meta_py_r.effect_for_study(e1, n1, e2, n2)
        ma_unit = self.get_current_ma_unit_for_study(study_index)
        # now set the effect size & CIs
        ma_unit.set_effect_and_ci(self.current_effect, est, lower, upper)
        
            
    def get_cur_raw_data_for_study(self, study_index):
        return self.get_current_ma_unit_for_study(study_index).get_raw_data_for_groups(self.current_txs)
        
    def get_current_ma_unit_for_study(self, study_index):
        ''' 
        Returns the MetaAnalytic unit for the study @ study_index. If no such Unit exists, 
        it will be added. Thus when a new study is added to a dataset, there is no need
        to initially populate this study with empty MetaAnalytic units reflecting the known
        outcomes and time points, as they will be added 'on-demand' here.
         '''
        if not self.current_outcome in self.dataset.studies[study_index].outcomes_to_follow_ups:
            self.dataset.studies[study_index].add_outcome(self.dataset.get_outcome_obj(self.current_outcome))
        # we must also make sure the time point exists
        if not self.current_time_point in self.dataset.studies[study_index].outcomes_to_follow_ups[self.current_outcome]:
            self.dataset.studies[study_index].add_outcome_at_follow_up(
                                self.dataset.get_outcome(self.current_outcome), self.current_time_point)
            
        return self.dataset.studies[study_index].outcomes_to_follow_ups[self.current_outcome][self.current_time_point]
        
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

            

        