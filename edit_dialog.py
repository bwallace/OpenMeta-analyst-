##################################
#                                           
#  Byron C. Wallace                   
#  Tufts Medical Center               
#  OpenMeta[analyst]                  
#                                     
#  This form is for 'batch' editing a dataset. Note that any 
#  edits apply to *all* MetaAnalyticUnit objects known. So
#  e.g., if a group name is changed, it will be changed 
# *everywhere*.
#
#  Note also that this form doesn't itself provide any 
#  undo/redo functionality. Rather, the strategy is to
#  treat *all* editing done via this form as one
#  undoable action.
#                                     
##################################

import pdb

from PyQt4.Qt import *

import ui_edit_dialog
import edit_list_models
import meta_py_r
import add_new_dialogs
import ma_dataset
from ma_dataset import *

class EditDialog(QDialog, ui_edit_dialog.Ui_edit_dialog):

    def __init__(self, dataset, parent=None):
        super(EditDialog, self).__init__(parent)
        self.setupUi(self)
        
        ### outcomes
        self.outcomes_model = edit_list_models.OutcomesModel(dataset = dataset)
        self.outcome_list.setModel(self.outcomes_model)
        try:
            index_of_outcome_to_select = self.outcomes_model.outcome_list.index(parent.model.current_outcome)
            outcome_index = self.outcomes_model.createIndex(index_of_outcome_to_select, 0)
            self.outcome_list.setCurrentIndex(outcome_index)
            self.selected_outcome = parent.model.current_outcome
            self.remove_outcome_btn.setEnabled(True)
        except:
            # no outcomes.
            self.selected_outcome = None
            
        ### follow-ups
        # notice that we pass the follow ups model the current outcome, because it will display only
        # those follow-ups included for this outcome
        self.follow_ups_model = edit_list_models.FollowUpsModel(dataset = dataset, outcome = self.selected_outcome)
        self.follow_up_list.setModel(self.follow_ups_model)
        if self.selected_outcome is not None:
            self.selected_follow_up = parent.model.get_current_follow_up_name()
            index_of_follow_up_to_select = self.follow_ups_model.follow_up_list.index(self.selected_follow_up)
            follow_up_index = self.follow_ups_model.createIndex(index_of_follow_up_to_select, 0)
            self.follow_up_list.setCurrentIndex(follow_up_index)
        else:
            self.selected_follow_up = None
            
        
        ### groups
        self.groups_model = edit_list_models.TXGroupsModel(dataset = dataset,\
                                                outcome = self.selected_outcome, follow_up = self.selected_follow_up)
        self.group_list.setModel(self.groups_model)
        
        
        ### studies
        self.studies_model = edit_list_models.StudiesModel(dataset = dataset)
        self.study_list.setModel(self.studies_model)
        
        self._setup_connections()
        self.dataset = dataset
        
        
    def _setup_connections(self):
        ###
        # groups
        QObject.connect(self.add_group_btn, SIGNAL("pressed()"),
                                    self.add_group)
        QObject.connect(self.remove_group_btn, SIGNAL("pressed()"),
                                    self.remove_group)
        QObject.connect(self.group_list, SIGNAL("clicked(QModelIndex)"),
                                    self.group_selected)
                 
        ###
        # outcomes   
        QObject.connect(self.add_outcome_btn, SIGNAL("pressed()"),
                                    self.add_outcome)
        QObject.connect(self.remove_outcome_btn, SIGNAL("pressed()"),
                                    self.remove_outcome)                          
        QObject.connect(self.outcome_list, SIGNAL("clicked(QModelIndex)"),
                                    self.outcome_selected)


        ###
        # follow-ups
        QObject.connect(self.add_follow_up_btn, SIGNAL("pressed()"),
                                    self.add_follow_up)
        QObject.connect(self.remove_follow_up_btn, SIGNAL("pressed()"),
                                    self.remove_follow_up)                          
        QObject.connect(self.follow_up_list, SIGNAL("clicked(QModelIndex)"),
                                    self.follow_up_selected)
                                    
        ###
        # studies
        QObject.connect(self.add_study_btn, SIGNAL("pressed()"),
                                    self.add_study)
        QObject.connect(self.remove_study_btn, SIGNAL("pressed()"),
                                    self.remove_study)                          

                                  
    def add_group(self):
        form = add_new_dialogs.AddNewGroupForm(self)
        form.group_name_le.setFocus()        
        if form.exec_():
            new_group_name = unicode(form.group_name_le.text().toUtf8(), "utf-8")
            self.group_list.model().dataset.add_group(new_group_name)
            self.group_list.model().refresh_group_list()
            
    def remove_group(self):
        index = self.group_list.currentIndex()
        selected_group = self.group_list.model().group_list[index.row()]
        self.group_list.model().dataset.delete_group(selected_group)
        self.group_list.model().refresh_group_list(self.selected_outcome, self.selected_follow_up)
        self.group_list.model().reset()
        
    def group_selected(self, index):
        self.disable_remove_buttons()
        self.remove_group_btn.setEnabled(True)
        
    def add_outcome(self):
        form =  add_new_dialogs.AddNewOutcomeForm(self)
        form.outcome_name_le.setFocus()
        if form.exec_():
            # then the user clicked ok and has added a new outcome.
            # here we want to add the outcome to the dataset, and then
            # display it
            new_outcome_name = unicode(form.outcome_name_le.text().toUtf8(), "utf-8")
            # the outcome type is one of the enumerated types; we don't worry about
            # unicode encoding
            data_type = str(form.datatype_cbo_box.currentText())
            data_type = STR_TO_TYPE_DICT[data_type.lower()]
            self.outcome_list.model().dataset.add_outcome(Outcome(new_outcome_name, data_type))
            self.outcome_list.model().refresh_outcome_list()
            self.outcome_list.model().current_outcome = new_outcome_name
            
    def get_selected_outcome(self):
        index = self.outcome_list.currentIndex()
        if index.row() < 0 or index.row() > len(self.outcome_list.model().outcome_list):
            return None
        return self.outcome_list.model().outcome_list[index.row()]
        
    def remove_outcome(self):
        self.selected_outcome = self.get_selected_outcome()
        self.outcome_list.model().dataset.remove_outcome(self.selected_outcome)
        self.outcome_list.model().refresh_outcome_list()
        self.outcome_list.model().reset()
        # now update the selected outcome
        self.selected_outcome = self.get_selected_outcome()
        # update the follow-ups list as appropriate
        if self.selected_outcome is not None:
            self.follow_up_list.model().current_outcome = self.selected_outcome
            print "\ncurrent outcome updated. is now: %s" % self.selected_outcome
            self.follow_up_list.model().refresh_follow_up_list()
            self.selected_follow_up = self.get_selected_follow_up()
            ## also update the groups and follow-up lists
            self.group_list.model().refresh_group_list(self.selected_outcome, self.selected_follow_up)
        else:
            ## the assumption in this case is that all outcomes have been deleted
            # so we clear the follow up and group lists.
            self.follow_up_list.model().follow_up_list = []
            self.follow_up_list.model().reset()
            self.group_list.model().group_list = []
            self.group_list.model().reset()

    def outcome_selected(self, index):
        self.selected_outcome = self.get_selected_outcome()
        self.follow_up_list.model().current_outcome = self.selected_outcome
        self.follow_up_list.model().refresh_follow_up_list()
        self.group_list.model().refresh_group_list(self.selected_outcome, self.selected_follow_up)
        ## update
        self.disable_remove_buttons()
        self.remove_outcome_btn.setEnabled(True)
        
    def add_follow_up(self):
        form = add_new_dialogs.AddNewFollowUpForm(self)
        form.follow_up_name_le.setFocus()
        if form.exec_():
            follow_up_lbl = unicode(form.follow_up_name_le.text().toUtf8(), "utf-8")
            self.follow_up_list.model().dataset.add_follow_up(follow_up_lbl)
            self.follow_up_list.model().current_outcome =self.selected_outcome
            self.follow_up_list.model().refresh_follow_up_list()
            
    def get_selected_follow_up(self):
        index = self.follow_up_list.currentIndex()
        print "index is: %s" % index.row()
        print "here is the current follow-up list: %s" % self.follow_up_list.model().follow_up_list
        return self.follow_up_list.model().follow_up_list[index.row()]
        
    def remove_follow_up(self):
        self.selected_follow_up = self.get_selected_follow_up()
        self.follow_up_list.model().dataset.remove_follow_up(self.selected_follow_up)
        self.follow_up_list.model().current_outcome =self.selected_outcome
        self.follow_up_list.model().refresh_follow_up_list()
        
    def follow_up_selected(self, index):
        self.disable_remove_buttons()
        # we want to disallow the user from removing *all* 
        # follow-ups for a given outcome, since this would be meaningless.
        # thus we check if there is only follow-up; if so, disable 
        # (or rather, don't enable) the remove button
        if len(self.follow_up_list.model().follow_up_list) > 1:
            self.remove_follow_up_btn.setEnabled(True)
        self.selected_follow_up = self.get_selected_follow_up()
        self.group_list.model().refresh_group_list(self.selected_outcome, self.selected_follow_up)
        
    def disable_remove_buttons(self):
        self.remove_group_btn.setEnabled(False)
        self.remove_follow_up_btn.setEnabled(False)
        self.remove_outcome_btn.setEnabled(False)
        
    def add_study(self):
        pass
    
    def remove_study(self):
        pass