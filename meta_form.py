######################################
#                                    #       
#  Byron C. Wallace                  #
#  Tufts Medical Center              # 
#  OpenMeta[analyst]                 # 
#                                    # 
#  Container form for UI. Handles    # 
#  user interaction.                 # 
#                                    # 
######################################

import sys
import os
import pdb
import pickle
print "about to import PyQt4..."
from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
print "success!"
#from os import urandom as _urandom
#import nose # for unit tests
import copy

#
# hand-rolled modules
#
import ui_meta
from ma_data_table_view import StudyDelegate
import ma_data_table_view
from ma_data_table_model import *
#import meta_globals
from meta_globals import *
import ma_dataset
from ma_dataset import *
import meta_py_r

#
# additional forms
#
import add_new_dialogs
import results_window
import ma_specs 
import diag_metrics
import meta_reg_form
import meta_subgroup_form
import edit_dialog
import edit_group_name_form
import change_cov_type_form
import network_view
import start_up_dialog
import create_new_project_dialog

# for the help
import webbrowser

class MetaForm(QtGui.QMainWindow, ui_meta.Ui_MainWindow):

    def __init__(self, parent=None):
        #
        # We follow the advice given by Mark Summerfield in his Python QT book: 
        # Namely, we use multiple inheritence to gain access to the ui. We take
        # this approach throughout OpenMeta.
        #
        super(MetaForm, self).__init__(parent)
        self.setupUi(self)
        

        # TODO should also allow a (path to a) dataset
        # to be given on the console.
        self.model = None
        self.new_dataset()

        # flag maintaining whether the current dataset
        # has been saved
        self.current_data_unsaved = False

        self.tableView.setModel(self.model)
        # attach a delegate for editing
        self.tableView.setItemDelegate(StudyDelegate(self))

        # the nav_lbl text corresponds to the currently selected
        # 'dimension', e.g., outcome or treatment. New points
        # can then be added tot his dimension, or it can be travelled
        # along using the horizontal nav arrows (the vertical arrows
        # navigate along the *dimensions*)
        self.dimensions =["outcome", "follow-up", "group"]
        self.cur_dimension_index = 0
        self.cur_dimension = "outcome"
        self.update_dimension()
        self._setup_connections()
        self.tableView.setSelectionMode(QTableView.ContiguousSelection)
        self.model.reset()
        ## 
        # we hand off a reference of the main gui to the table view
        # so that it can do things like pass suitable events 'up'
        # to the main form 
        self.tableView.main_gui = self
        self.tableView.resizeColumnsToContents()
    

        self.out_path = None
        self.metric_menu_is_set_for = None
        self.raise_()

        # by default, disable meta-regression (until we
        # have covariates)
        self.action_meta_regression.setEnabled(False)

        ####
        # this (toy-data) is almost certainly antiquated 
        # and should be removed or updated
        ####
        if len(sys.argv)>1 and sys.argv[-1]=="--toy-data":
            # toy data for now
            data_model = _gen_some_data()
            self.model = DatasetModel(dataset=data_model)
            self.display_outcome("death")
            self.model.set_current_time_point(0)
            self.model.current_effect = "OR"
            self.model.try_to_update_outcomes()
            self.model.reset()
            self.tableView.resizeColumnsToContents()
        else:
            ###
            # show the welcome dialog 
            self.load_user_prefs()
            if self.user_prefs["splash"]:
                start_up_window =  start_up_dialog.StartUp(parent=self, \
                            recent_datasets=self.user_prefs['recent datasets'])
                
                ###
                # fix for issue #158
                # formerly .show()
                start_up_window.exec_()

                ## arg -- this won't work!
                start_up_window.setFocus()
                #start_up_window.dataset_name_le.setFocus()  ######## DELETE LATER? OBSOLETE?
                start_up_window.raise_()
                start_up_window.activateWindow()
      
            self.populate_open_recent_menu()
            

    def closeEvent(self, event):
        if self.current_data_unsaved:
            if not self.user_is_going_to_lose_data():
                # then they canceled!
                event.ignore()
        print "*** goodbye, dear analyst. ***"

    ###    Should ask if user wants to save before making the new dataset.. GD
    def create_new_dataset(self):
#        new_data_window = start_up_dialog.StartUp(parent=self, \
#                    recent_datasets=self.user_prefs['recent datasets'],
#                    start_up=False)
#        new_data_window.show()
#        ## arg -- this won't work!
#        new_data_window.setFocus()
#        new_data_window.dataset_name_le.setFocus()
        
        name = unicode("untitled_dataset", "utf-8")
        
        new_project_dialog = create_new_project_dialog.CreateNewProjectDlg(parent=self)
        if new_project_dialog.exec_():
            dataset_info = new_project_dialog.get_summary()
            is_diag = dataset_info['data_type'] == "Diagnostic"
        
            self.new_dataset(name=name, is_diag=is_diag)
            tmp = self.cur_dimension
            self.cur_dimension = "outcome"
            #self.hide()
            self.add_new(dataset_info)        
            self.cur_dimension = tmp
            #self.close()
            
            # SET DEFAULT METRICS
            if dataset_info['data_type'] == "Binary":
                if dataset_info['arms'] == "one":
                    self.model.current_effect = meta_globals.DEFAULT_BINARY_ONE_ARM
                elif dataset_info['arms'] == "two":
                    self.model.current_effect = meta_globals.DEFAULT_BINARY_TWO_ARM
            if dataset_info['data_type'] == "Continuous":
                if dataset_info['arms'] == "one":
                    self.model.current_effect = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
                elif dataset_info['arms'] == "two":
                    self.model.current_effect = meta_globals.DEFAULT_CONTINUOUS_TWO_ARM
            # Put this stuff in an if just in case...
            if dataset_info['data_type'] == "Binary" or dataset_info['data_type'] == "Continuous":
                self.populate_metrics_menu(metric_to_check=self.model.current_effect)
                self.model.try_to_update_outcomes()
                self.model.reset()
        
        
    def new_dataset(self, name=None, is_diag=False):
        data_model = Dataset(title=name, is_diag=is_diag)
        if self.model is not None:
            original_dataset = copy.deepcopy(self.model.dataset)
            old_state_dict = self.tableView.model().get_stateful_dict()
            undo_f = lambda : self.set_model(original_dataset, old_state_dict) 
            redo_f = lambda : self.set_model(data_model)
            edit_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(edit_command)
        else:
            self.model = DatasetModel(dataset=data_model)
            # no dataset; disable saving, editing, etc.
            self.disable_menu_options_that_require_dataset()
        # set the out_path to None; this (new) dataset is unsaved.
        self.out_path = None


    def _notify_user_that_data_is_unsaved(self):
        if self.out_path is None:
            self.dataset_file_lbl.setText("<font color='red'>careful! your data isn't saved yet</font>")
        else:
            self.dataset_file_lbl.setText("open file: <font color='red'>%s</font>" % self.out_path)

    def toggle_menu_options_that_require_dataset(self, enable):
        self.action_go.setEnabled(enable)
        if self.model.get_current_outcome_type() != "diagnostic":
            self.action_cum_ma.setEnabled(enable)
        self.action_loo_ma.setEnabled(enable)
        self.action_meta_regression.setEnabled(enable)
        self.action_subgroup_ma.setEnabled(enable)
        
    def disable_menu_options_that_require_dataset(self):
        self.toggle_menu_options_that_require_dataset(False)

    def enable_menu_options_that_require_dataset(self):
        self.toggle_menu_options_that_require_dataset(True)

    def keyPressEvent(self, event):
        if (event.modifiers() & QtCore.Qt.ControlModifier):
            if event.key() == QtCore.Qt.Key_S:
                # ctrl + s = save
                print "saving.."
                self.save()
            elif event.key() == QtCore.Qt.Key_O:
                # ctrl + o = open
                self.open()
            elif event.key() == QtCore.Qt.Key_A:
                self.analysis()

    def _disconnections(self):
        ''' 
        disconnects model-related signs/slots. this should be called prior to swapping
        in a new model, e.g., when a dataset is loaded, to tear down the relevant connections. 
        _setup_connections (with menu_actiosn set to False) should subsequently be invoked. 
        '''
        QObject.disconnect(self.tableView.model(), SIGNAL("pyCellContentChanged(PyQt_PyObject, PyQt_PyObject, PyQt_PyObject, PyQt_PyObject)"),
                                                                self.tableView.cell_content_changed)

        QObject.disconnect(self.tableView.model(), SIGNAL("outcomeChanged()"),
                                                                self.tableView.displayed_ma_changed)
        QObject.disconnect(self.tableView.model(), SIGNAL("followUpChanged()"),
                                                                self.tableView.displayed_ma_changed)
                                                                 
        QObject.disconnect(self.tableView, SIGNAL("dataDirtied()"), self.data_dirtied)


    def data_error(self, msg):
        QMessageBox.warning(self.parent(), "whoops", msg)

    def set_edit_focus(self, index):
        ''' sets edit focus to the row,col specified by index.'''
        self.tableView.setCurrentIndex(index)
        self.tableView.edit(index)
         
    def populate_open_recent_menu(self):
        recent_datasets = self.user_prefs['recent datasets']
        recent_datasets.reverse() # most-recent first
        # qt designer inexplicably forcing the _2. not sure why; 
        # gave up struggling with it. grr.
        self.action_open_recent_2.clear()
        for dataset in recent_datasets:
            action_item = QAction(QString(dataset), self.action_open_recent_2)
            self.action_open_recent_2.addAction(action_item)
            QObject.connect(action_item, SIGNAL("triggered()"), self.dataset_selected) 
                
        
        

    def dataset_selected(self):
        dataset_path = QObject.sender(self).text()
        self.open(file_path=dataset_path)

    def _setup_connections(self, menu_actions=True):
        ''' Signals & slots '''
        QObject.connect(self.tableView.model(), SIGNAL("pyCellContentChanged(PyQt_PyObject, PyQt_PyObject, PyQt_PyObject, PyQt_PyObject)"),
                                                                self.tableView.cell_content_changed)


        QObject.connect(self.tableView.model(), SIGNAL("outcomeChanged()"),
                                                                self.tableView.displayed_ma_changed)
        QObject.connect(self.tableView.model(), SIGNAL("followUpChanged()"),
                                                                self.tableView.displayed_ma_changed)
                                                                
        ###
        # this is not ideal, but I couldn't get the rowsInserted methods working. 
        # basically, the modelReset (which is custom to this app; not a QT thing, per se)
        # is emitted when a model reset was called but the edit focus should be set back to 
        # where it was before this reset() call (reset clears the current editor).
        # this index is the QModelIndex. this is used, e.g., when a new study is added.
        # this fixes bug #20.
        QObject.connect(self.tableView.model(), SIGNAL("modelReset(QModelIndex)"),
                                                                self.set_edit_focus) 
           
        ###
        # this listens to the model regarding errors in data entry -- 
        # such data will be rejected (e.g., strings for counts, or whatever),
        # and this hook allows the model to pass along error messages to the
        # user. the data checking happens in ma_dataset (specifically, in the
        # setData method)                                     
        QObject.connect(self.tableView.model(), SIGNAL("dataError(QString)"), 
                                                                self.data_error)

        QObject.connect(self.tableView, SIGNAL("dataDirtied()"),
                                                self.data_dirtied)                                                       
        if menu_actions:                
            QObject.connect(self.nav_add_btn, SIGNAL("pressed()"), self.add_new)
            QObject.connect(self.nav_right_btn, SIGNAL("pressed()"), self.next)
            QObject.connect(self.nav_left_btn, SIGNAL("pressed()"), self.previous)
            QObject.connect(self.nav_up_btn, SIGNAL("pressed()"), self.next_dimension)
            QObject.connect(self.nav_down_btn, SIGNAL("pressed()"), self.previous_dimension)
    
            QObject.connect(self.action_save, SIGNAL("triggered()"), self.save)
            QObject.connect(self.action_save_as, SIGNAL("triggered()"), self.save_as)
            QObject.connect(self.action_open, SIGNAL("triggered()"), self.open)
            QObject.connect(self.action_new_dataset, SIGNAL("triggered()"), self.create_new_dataset)
            QObject.connect(self.action_quit, SIGNAL("triggered()"), self.quit)
            QObject.connect(self.action_go, SIGNAL("triggered()"), self.go)
            QObject.connect(self.action_cum_ma, SIGNAL("triggered()"), self.cum_ma)
            QObject.connect(self.action_loo_ma, SIGNAL("triggered()"), self.loo_ma)
            
            QObject.connect(self.action_undo, SIGNAL("triggered()"), self.undo)
            QObject.connect(self.action_redo, SIGNAL("triggered()"), self.redo)
            QObject.connect(self.action_copy, SIGNAL("triggered()"), self.tableView.copy)
            QObject.connect(self.action_paste, SIGNAL("triggered()"), self.tableView.paste)
            
            QObject.connect(self.action_edit, SIGNAL("triggered()"), self.edit_dataset)
            QObject.connect(self.action_view_network, SIGNAL("triggered()"), self.view_network)
            QObject.connect(self.action_add_covariate, SIGNAL("triggered()"), self.add_covariate)
            
            QObject.connect(self.action_meta_regression, SIGNAL("triggered()"), self.meta_reg)
            QObject.connect(self.action_subgroup_ma, SIGNAL("triggered()"), self.meta_subgroup_get_cov)

            QObject.connect(self.action_open_help, SIGNAL("triggered()"), self.show_help)

    def go(self):
        form = None
        if self.model.get_current_outcome_type() != "diagnostic":
            # in the binary and continuous case, we go straight 
            # to selecting the metric/parameters here.
            #
            # note that the spec form gets *this* form as a parameter.
            # this allows the spec form to callback to this
            # module when specifications have been provided.
            form =  ma_specs.MA_Specs(self.model, parent=self)
        else:
            # diagnostic data; we first have the user select metric(s),
            # and only then the model, &etc.
            form = diag_metrics.Diag_Metrics(self.model, parent=self)
        form.show()
    
    def meta_reg(self):
        form = meta_reg_form.MetaRegForm(self.model, parent=self)
        form.show()
        
    def data_dirtied(self):
        self._notify_user_that_data_is_unsaved()
        self.current_data_unsaved = True

    def meta_subgroup_get_cov(self):
        form = meta_subgroup_form.MetaSubgroupForm(self.model, parent=self)
        form.show()
  
    ####
    # Here are the calls to ma_specs with so-called `meta-methods`
    # which operate over the output of meta-analytic methods. Note
    # that we don't care what sort of data we're operating over here;
    # ma_specs takes care of that. The convention is that each meta
    # method, for example `cum.ma`, has .binary and .continuous 
    # implementation.
    ### TODO pull out meta methods auto-magically via introspection.
    def cum_ma(self):
        # NOTE that we do not allow cumulative meta-analysis on
        # diagnostic data -- this method should never be invoked
        # if we're dealing with diag data.
        form = None
        # note that the spec form gets *this* form as a parameter.
        # this allows the spec form to callback to this
        # module when specifications have been provided.
        if self.model.get_current_outcome_type() != "diagnostic":
            form =  ma_specs.MA_Specs(self.model, meta_f_str="cum.ma", parent=self)
        else:
            # diagnostic data; we first have the user select metric(s),
            # and only then the model, &etc.
            '''
            @@ TODO this is not actually implemented! i.e., we do not have
            a cumulative diagnostic MA method. for now this method should
            *never* be called with diagnostic data.
            '''
            form = diag_metrics.Diag_Metrics(self.model, meta_f_str="cum.ma", \
                                                parent=self) 

        form.show()
        
    def loo_ma(self):
        form = None
        if self.model.get_current_outcome_type() != "diagnostic":
            # in the binary and continuous case, we go straight 
            # to selecting the metric/parameters here.
            #
            # note that the spec form gets *this* form as a parameter.
            # this allows the spec form to callback to this
            # module when specifications have been provided.
            form =  ma_specs.MA_Specs(self.model, meta_f_str="loo.ma", parent=self)
        else:
            # diagnostic data; we first have the user select metric(s),
            # and only then the model, &etc.
            form = diag_metrics.Diag_Metrics(self.model, meta_f_str="loo.ma", \
                                                parent=self)

        form.show()

    def show_help(self):
        webbrowser.open(PATH_TO_HELP)

    def meta_subgroup(self, selected_cov):
        form = None
        if self.model.get_current_outcome_type() != "diagnostic":
            # in the binary and continuous case, we go straight 
            # to selecting the metric/parameters here.
            #
            # note that the spec form gets *this* form as a parameter.
            # this allows the spec form to callback to this
            # module when specifications have been provided.
            form = ma_specs.MA_Specs(self.model, meta_f_str="subgroup.ma", 
                                  parent=self, \
                                  external_params={"cov_name":selected_cov})
        else:
            # diagnostic data; we first have the user select metric(s),
            # and only then the model, &etc.
            form = diag_metrics.Diag_Metrics(self.model, meta_f_str="subgroup.ma", \
                                                parent=self,\
                                                external_params={"cov_name":selected_cov})

        form.show()
    
    def undo(self):
        self.tableView.undoStack.undo()
        
    def redo(self):
        self.tableView.undoStack.redo()
        
    def edit_dataset(self):
        cur_dataset = copy.deepcopy(self.model.dataset)
        edit_window =  edit_dialog.EditDialog(cur_dataset, parent=self)
    
        if edit_window.exec_():
            # if we edited the current dataset when there was no
            # outcome yet, then we want to default to an outcome
            # that was added.

            ### get stateful dictionary here, update, pass to 
            old_state_dict = self.tableView.model().get_stateful_dict()
            new_state_dict = copy.deepcopy(old_state_dict)

            # update the new state dict to reflect the currently selected
            # outcomes, etc.
            new_state_dict["current_outcome"] = old_state_dict["current_outcome"]

            if edit_window.outcome_list.model().current_outcome is not None:
                new_state_dict["current_outcome"] = edit_window.outcome_list.model().current_outcome
            # fix for issue #130: if the current outcome no longer exists, pick a different one.
            elif new_state_dict["current_outcome"] not in \
                            edit_window.outcome_list.model().outcome_list:
                # then just show a random outcome
                new_state_dict["current_outcome"] = edit_window.outcome_list.model().outcome_list[0]

            new_state_dict["current_time_point"] =  max(edit_window.follow_up_list.currentIndex().row(), 0)
            grp_list = edit_window.group_list.model().group_list

            if len(grp_list) >= 2:
                new_state_dict["current_txs"] = grp_list[:2]
            else:
                new_state_dict["current_txs"] = ["tx A", "tx B"]
            modified_dataset = edit_window.dataset
            
            redo_f = lambda : self.set_model(modified_dataset, new_state_dict)
            original_dataset = copy.deepcopy(self.model.dataset)
            undo_f = lambda : self.set_model(original_dataset, old_state_dict) 
            edit_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(edit_command)
            
    
    def populate_metrics_menu(self, metric_to_check=None):
        '''
        Populates the `metric` sub-menu with available metrics for the
        current datatype.
        '''
        self.menuMetric.clear()
        self.menuMetric.setDisabled(False)

        if self.model.get_current_outcome_type()=="binary":
            self.add_binary_metrics(metric_to_check=metric_to_check)
            self.metric_menu_is_set_for = BINARY

        elif self.model.get_current_outcome_type()=="continuous":
            self.add_continuous_metrics(metric_to_check=metric_to_check)
            self.metric_menu_is_set_for = CONTINUOUS
        
        else:
            # diagnostic data; deactive metrics option
            # we always show sens. + spec. for diag. data.
            self.menuMetric.setDisabled(True)
            self.metric_menu_is_set_for = DIAGNOSTIC

                
    def add_binary_metrics(self, metric_to_check=None):
        self.add_metrics(meta_globals.BINARY_ONE_ARM_METRICS,\
                         meta_globals.BINARY_TWO_ARM_METRICS,
                         metric_to_check=metric_to_check)
        
    def add_continuous_metrics(self, metric_to_check=None):
        self.add_metrics(meta_globals.CONTINUOUS_ONE_ARM_METRICS,\
                         meta_globals.CONTINUOUS_TWO_ARM_METRICS,
                         metric_to_check=metric_to_check)
        
    def add_metrics(self, one_arm_metrics, two_arm_metrics, \
                        metric_to_check=None):
        # we'll add sub-menus for two-arm and one-arm metrics
        self.twoArmMetricMenu = self.add_sub_metric_menu("two-arm")
        self.oneArmMetricMenu = self.add_sub_metric_menu("one-arm")

        for i,metric in enumerate(two_arm_metrics):
            metric_action = self.add_metric_action(metric, self.twoArmMetricMenu)
            if metric == metric_to_check or (metric_to_check is None and i == 0):
                # arbitrarily check the first metric in the case that none
                # is specificied 
                metric_action.setChecked(True)
                
        # now add the one-arm metrics
        for metric in one_arm_metrics:
            metric_action = self.add_metric_action(metric, self.oneArmMetricMenu)    
            if metric == metric_to_check:
                metric_action.setChecked(True)
      

    def add_sub_metric_menu(self, name):
        sub_menu = QtGui.QMenu(QString(name), self.menuMetric)
        self.menuMetric.addAction(sub_menu.menuAction())
        return sub_menu
           
    def add_metric_action(self, metric, menu):
        metric_action = QAction(QString(metric), self)
        metric_action.setCheckable(True)
        QObject.connect(metric_action, \
                        SIGNAL("toggled(bool)"),\
                        lambda: self.metric_selected(metric, menu))
        menu.addAction(metric_action)     
        return metric_action
    
    
    def deselect_all_metrics(self):
        # de-selects all metrics
        # it doesn't appear that there is a more
        # straight forward way of doing this, 
        # unfortunately.
        data_type = self.tableView.model().get_current_outcome_type(get_str=False)
        if data_type in (BINARY, CONTINUOUS):
            # then there are sub-menus (one-group, two-group)
            for sub_menu in self.menuMetric.actions():
                sub_menu = sub_menu.menu()
                for action in sub_menu.actions():
                    action.blockSignals(True)
                    action.setChecked(False)
                    action.blockSignals(False)

    def metric_selected(self, metric_name, menu):
        # first deselect the previous metric
        self.deselect_all_metrics()
        
        # now select the newly chosen one.
        prev_metric_name = self.tableView.model().current_effect
        for action in menu.actions():
            action_text = action.text()
            if action_text == metric_name:
                action.blockSignals(True)
                action.setChecked(True)
                action.blockSignals(False)
        
        self.tableView.model().set_current_metric(metric_name)
        self.model.try_to_update_outcomes()    
        self.model.reset()
        self.tableView.resizeColumnsToContents()
        
    def view_network(self):
        view_window =  network_view.ViewDialog(self.model, parent=self)
        view_window.show()
        
    def analysis(self, results):
        form = results_window.ResultsWindow(results, parent=self)
        form.show()


    def edit_group_name(self, cur_group_name):
        orig_group_name = copy.copy(cur_group_name)
        edit_group_form = edit_group_name_form.EditGroupName(cur_group_name, parent=self)
        if edit_group_form.exec_():
            new_group_name = unicode(edit_group_form.group_name_le.text().toUtf8(), "utf-8")
            
            # make sure the group name doesn't already exist
            if new_group_name in self.model.dataset.get_group_names():
                QMessageBox.warning(self,
                            "whoops.",
                            "%s is already a group name -- pick something else, please" % new_group_name)
            
            else:
                redo_f = lambda: self.model.rename_group(orig_group_name, new_group_name)
                undo_f = lambda: self.model.rename_group(new_group_name, orig_group_name)

                rename_group_command = CommandGenericDo(redo_f, undo_f)
                self.tableView.undoStack.push(rename_group_command)
            
    def add_covariate(self):
        form = add_new_dialogs.AddNewCovariateForm(self)
        form.covariate_name_le.setFocus()
        if form.exec_():
            # then the user clicked 'ok'.
            new_covariate_name = unicode(form.covariate_name_le.text().toUtf8(), "utf-8")

            # fix for issue #59; do not allow the user to create two covariates with
            # the same name!
            new_covariate_type = str(form.datatype_cbo_box.currentText())
            if new_covariate_name in self.model.get_covariate_names():
                QMessageBox.warning(self,
                            "whoops.",
                            "you've already entered a covariate with the name %s; please pick another name." % \
                                    new_covariate_name)
            else:
                redo_f = lambda: self._add_new_covariate(new_covariate_name, new_covariate_type)
                undo_f = lambda: self._undo_add_new_covariate(new_covariate_name)
                
                add_cov_command = CommandGenericDo(redo_f, undo_f)
                self.tableView.undoStack.push(add_cov_command)
     

    def _add_new_covariate(self, cov_name, cov_type):
        self.model.add_covariate(cov_name, cov_type)
        print "new covariate name: %s with type %s" % (cov_name, cov_type)
        self.tableView.resizeColumnsToContents()
        self.action_meta_regression.setEnabled(True)
        
    def _undo_add_new_covariate(self, cov_name):
        self.model.remove_covariate(cov_name)
        self.tableView.resizeColumnsToContents()
        if len(self.model.covariates) == 0:
            self.action_meta_regression.setEnabled(False)
        
    def add_new(self, startup_outcome = None):
        redo_f, undo_f = None, None
        if self.cur_dimension == "outcome" and not startup_outcome:
            form = add_new_dialogs.AddNewOutcomeForm(parent=self, is_diag=self.model.is_diag())
            form.outcome_name_le.setFocus()
            if form.exec_():
                # then the user clicked ok and has added a new outcome.
                # here we want to add the outcome to the dataset, and then
                # display it
                new_outcome_name = unicode(form.outcome_name_le.text().toUtf8(), "utf-8")
                # the outcome type is one of the enumerated types; we don't worry about
                # unicode encoding
                new_outcome_type = str(form.datatype_cbo_box.currentText())
                redo_f = lambda: self._add_new_outcome(new_outcome_name, new_outcome_type)
                prev_outcome = str(self.model.current_outcome)
                undo_f = lambda: self._undo_add_new_outcome(new_outcome_name, prev_outcome)
        elif self.cur_dimension == "outcome" and startup_outcome: # For dealing with outcomes from the startup form
            new_outcome_name = unicode(startup_outcome['name'].toUtf8(), "utf-8")
            new_outcome_type = str(startup_outcome['data_type'])
            print 'Startup Outcome',startup_outcome
            redo_f = lambda: self._add_new_outcome(new_outcome_name, new_outcome_type)
            prev_outcome = str(self.model.current_outcome)
            undo_f = lambda: self._undo_add_new_outcome(new_outcome_name, prev_outcome)
        elif self.cur_dimension == "group":
            form = add_new_dialogs.AddNewGroupForm(self)
            form.group_name_le.setFocus()        
            if form.exec_():
                new_group_name = unicode(form.group_name_le.text().toUtf8(), "utf-8")
                cur_groups = list(self.model.get_current_groups())
                redo_f = lambda: self._add_new_group(new_group_name)
                undo_f = lambda: self._undo_add_new_group(new_group_name, cur_groups)
        else:
            # then the dimension is follow-up
            form = add_new_dialogs.AddNewFollowUpForm(self)
            form.follow_up_name_le.setFocus()
            if form.exec_():
                follow_up_lbl = unicode(form.follow_up_name_le.text().toUtf8(), "utf-8")
                redo_f = lambda: self._add_new_follow_up_for_cur_outcome(follow_up_lbl)
                previous_follow_up = self.model.get_current_follow_up_name()
                undo_f = lambda: self._undo_add_follow_up_for_cur_outcome(\
                                                    previous_follow_up, follow_up_lbl)

        if redo_f is not None:
            next_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(next_command)
                
    def _add_new_group(self, new_group_name):
        self.model.add_new_group(new_group_name)
        print "\nok. added new group: %s" % new_group_name
        cur_groups = list(self.model.get_current_groups())
        cur_groups[1] = new_group_name
        self.model.set_current_groups(cur_groups)
        # @TODO probably need to tell the table model we changed 
        # the group being displayed...
        self.display_groups(cur_groups)
        
    def _undo_add_new_group(self, added_group, previously_displayed_groups):
        self.model.remove_group(added_group)
        print "\nremoved group %s" % added_group
        print "attempting to display groups: %s" % previously_displayed_groups
        self.model.set_current_groups(previously_displayed_groups)
        self.display_groups(previously_displayed_groups)
    
    def _undo_add_new_outcome(self, added_outcome, previously_displayed_outcome):
        print "removing added outcome: %s" % added_outcome
        self.model.remove_outcome(added_outcome)
        print "trying to display: %s" % previously_displayed_outcome
        ##
        # RESOLVED previously, if previous outcome was None, this threw up
        # (see Issue 4: http://github.com/bwallace/OpenMeta-analyst-/issues#issue/4)
        self.display_outcome(previously_displayed_outcome)
    
    def _add_new_outcome(self, outcome_name, outcome_type):
        self.model.add_new_outcome(outcome_name, outcome_type)
        self.display_outcome(outcome_name)
        
    def _add_new_follow_up_for_cur_outcome(self, follow_up_lbl):
        self.model.add_follow_up_to_current_outcome(follow_up_lbl)
        self.display_follow_up(self.model.get_t_point_for_follow_up_name(follow_up_lbl))
        
    def _undo_add_follow_up_for_cur_outcome(self, prev_follow_up, follow_up_to_del):
        self.model.remove_follow_up_from_outcome(follow_up_to_del, \
                                                                                str(self.model.current_outcome))
        self.display_follow_up(self.model.get_t_point_for_follow_up_name(prev_follow_up))
        
    def next(self):
        # probably you should disable next for the current dimension
        # if there is only one point (e.g., outcome). otherwise you end
        # up enqueueing a bunch of pointless undo/redos.
        redo_f, undo_f = None, None
        if self.cur_dimension == "outcome":
            old_outcome = self.model.current_outcome
            ## 
            # note that we have to cache the currently displayed
            # groups, as well. these groups may or may not be available
            # on the next outcome; the next_outcome call may therefore
            # default to displaying some other group(s). however, this
            # would cause problems when the 'next' action is undone, as in
            # such a case the previous (current) outcome will be displayed,
            # but the groups being displayed may be other than what they 
            # should be (i.e., than what they are currently)
            previous_groups = self.model.get_current_groups()
            next_outcome = self.model.get_next_outcome_name()
            redo_f = lambda: self.display_outcome(next_outcome)
            previous_follow_up = self.model.get_current_follow_up_name()
            undo_f = lambda: self.display_outcome(old_outcome, \
                                            follow_up_name=previous_follow_up, group_names=previous_groups)
        elif self.cur_dimension == "group":
            previous_groups = self.model.get_current_groups()
            new_groups = self.model.next_groups()
            redo_f = lambda: self.display_groups(new_groups)
            undo_f = lambda: self.display_groups(previous_groups)
        elif self.cur_dimension == "follow-up":
            old_follow_up_t_point = self.model.current_time_point
            next_follow_up_t_point = self.model.get_next_follow_up()[0]
            redo_f = lambda: self.display_follow_up(next_follow_up_t_point) 
            undo_f = lambda: self.display_follow_up(old_follow_up_t_point)
            
        if redo_f is not None and undo_f is not None:
            next_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(next_command)
            
    def previous(self):
        redo_f, undo_f = None, None
        if self.cur_dimension == "outcome":
            old_outcome = self.model.current_outcome
            next_outcome = self.model.get_prev_outcome_name()
            redo_f = lambda: self.display_outcome(next_outcome)
            undo_f = lambda: self.display_outcome(old_outcome)
        elif self.cur_dimension == "group":
            cur_groups = self.model.get_current_groups()
            prev_groups = self.model.get_previous_groups()
            redo_f = lambda: self.display_groups(prev_groups)
            undo_f = lambda: self.display_groups(cur_groups)
        elif self.cur_dimension == "follow-up":
            old_follow_up_t_point = self.model.current_time_point
            previous_follow_up_t_point = self.model.get_previous_follow_up()[0]
            redo_f = lambda: self.display_follow_up(previous_follow_up_t_point) 
            undo_f = lambda: self.display_follow_up(old_follow_up_t_point)
            
        if redo_f is not None and undo_f is not None:
            prev_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(prev_command)

    def next_dimension(self):
        '''
        In keeping with the dimensions metaphor, wherein the various
        components that can comprise a dataset are 'dimensions' (e.g.,
        outcomes), this function iterates over the dimensions. So if you call
        this method, then 'next()', the next method will step forward in the
        dimension made active here.
        '''
        if self.cur_dimension_index == len(self.dimensions)-1:
            self.cur_dimension_index = 0
        else:
            self.cur_dimension_index+=1
        self.update_dimension()

    def previous_dimension(self):
        if self.cur_dimension_index == 0:
            self.cur_dimension_index = len(self.dimensions)-1
        else:
            self.cur_dimension_index-=1
        self.update_dimension()

    def update_dimension(self):
        self.cur_dimension = self.dimensions[self.cur_dimension_index]
        self.nav_lbl.setText(self.cur_dimension)

    def display_groups(self, groups):
        print "displaying groups: %s" % groups
        self.model.set_current_groups(groups)
        self.model.try_to_update_outcomes()
        self.model.reset()
        self.tableView.resizeColumnsToContents()
        
    def display_outcome(self, outcome_name, group_names=None, follow_up_name=None):
        print "displaying outcome: %s" % outcome_name
        ###
        # We need to update which groups & follow-ups are current
        # in order to avoid attempting to display a group/fu that
        # do not belong to the outcome_name. 
        self.model.set_current_outcome(outcome_name)
        self.populate_metrics_menu()
        
        # first ascertain if the currently displayed follow up is
        # available for this outcome
        if follow_up_name is not None:
            self.model.set_current_follow_up(follow_up_name)
        else:
            # If a follow up isn't explicitly passed in, attempt to use
            # the current follow up. If this does not exist for the outcome
            # to be displayed, then display a different follow up.
            cur_follow_up = self.model.get_current_follow_up_name()
            if not self.model.outcome_has_follow_up(outcome_name, cur_follow_up):
                # then the outcome does not have this follow up and we have to 
                # step on to the next one.
                next_follow_up = self.model.get_next_follow_up()[1]
                self.model.set_current_follow_up(next_follow_up)
        
        # now we check the groups.
        if group_names is not None:
            self.model.set_current_groups(group_names)
        else:
            # then no group names were explicitly passed in; ascertain
            # that the outcome/fu contains the current groups; if not,
            # set them to something else.
            cur_groups = self.model.get_current_groups()
            if not all([self.model.outcome_fu_has_group(\
                            outcome_name, self.model.get_current_follow_up_name(), group) for group in cur_groups]):
                self.model.set_current_groups(self.model.next_groups())
            
        self.cur_outcome_lbl.setText(u"<font color='Blue'>%s</font>" % outcome_name)
        self.cur_time_lbl.setText(u"<font color='Blue'>%s</font>" % self.model.get_current_follow_up_name())
        self.model.reset()
        self.tableView.resizeColumnsToContents()

    def display_follow_up(self, time_point):
        print "follow up"
        self.model.current_time_point = time_point
        self.update_follow_up_label()
        self.model.reset()
        self.tableView.resizeColumnsToContents()
        
    def update_follow_up_label(self):
        self.cur_time_lbl.setText(u"<font color='Blue'>%s</font>" % self.model.get_current_follow_up_name())
        
    def open(self, file_path=None):
        '''
        This gets called when the user opts to open an existing dataset. Note that we make use
        of the pickled dataset itself (.oma) and we also look for a corresponding `state`
        dictionary, which contains things like which outcome was currently displayed, etc.
        Also note that, as in Excel, the open operation is undoable. 
        '''
        
        if self.current_data_unsaved:
            if not self.user_is_going_to_lose_data():
                # then they canceled!
                return

        # if no file path is provided, prompt the user.
        if file_path is None:
            file_path = unicode(QFileDialog.getOpenFileName(self, "OpenMeta[analyst] - Open File",
                                                              ".", "open meta files (*.oma)"))
            # if the user didn't select anything, we return false.                                  
            if file_path == "":
                return False                                               

        if file_path in self.user_prefs['recent datasets']:
            # delete it; we'll re-insert it
            self.user_prefs['recent datasets'].remove(file_path)
        
        self.user_prefs['recent datasets'].append(file_path)
        self._save_user_prefs()
        
        
        data_model = None
        print "loading %s..." % file_path
        try:
            data_model = pickle.load(open(file_path, 'r'))
            print "successfully loaded data"
        except:
            return None
        
        ## cache current state for undo.
        prev_out_path = copy.copy(self.out_path)
        prev_state_dict = copy.copy(self.model.get_stateful_dict())
        
        self.out_path = file_path
        
        state_dict = None
        check_for_appropriate_metric = False # only if we guess at the current state
        try:
            state_dict = pickle.load(open(file_path + ".state"))
            print "found state dictionary: \n%s" % state_dict
        except:
            print "no state dictionary found -- using 'reasonable' defaults"
            state_dict = self.tableView.model().make_reasonable_stateful_dict(data_model)
            print "made state dictionary: \n%s" % state_dict
            check_for_appropriate_metric = True

        prev_dataset = self.model.dataset.copy()
        
        undo_f = lambda : self.undo_set_model(prev_out_path, prev_state_dict, \
                                                prev_dataset)
        redo_f = lambda : self.set_model(data_model, state_dict, \
                                            check_for_appropriate_metric=True)
        
        open_command = CommandGenericDo(redo_f, undo_f)
        self.tableView.undoStack.push(open_command)
        self.dataset_file_lbl.setText("open file: %s" % file_path)
        
        # we just opened it, so it's 'saved'
        self.current_data_unsaved = False

        return True

        
    def delete_study(self, study, study_index=None):
        undo_f = lambda : self._add_study(study, study_index=study_index)
        redo_f = lambda : self._remove_study(study)
        delete_command = CommandGenericDo(redo_f, undo_f)
        self.tableView.undoStack.push(delete_command)
    

    def change_cov_type(self, covariate):
        cur_dataset = copy.deepcopy(self.model.dataset)
        # keep the current study order, because we're going to sort the studies
        # on the change_cov_form but we want to revert to the ordering
        # they came in with when we're done.
        original_study_order = \
                [study.name for study in self.model.dataset.studies]

        change_type_form = \
            change_cov_type_form.ChangeCovTypeForm(cur_dataset, covariate, parent=self)
        
        if change_type_form.exec_():
            modified_dataset = change_type_form.dataset
            # revert to original study ordering
            modified_dataset.studies.sort(\
                    cmp=modified_dataset.cmp_studies(compare_by="ordered_list",\
                                        ordered_list=original_study_order))
            

            ### use the same state dict as before.
            old_state_dict = self.tableView.model().get_stateful_dict()
            new_state_dict = copy.deepcopy(old_state_dict)

            redo_f = lambda : self.set_model(modified_dataset, new_state_dict)
            original_dataset = copy.deepcopy(self.model.dataset)
            undo_f = lambda : self.set_model(original_dataset, old_state_dict) 
            edit_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(edit_command)
            

    def rename_covariate(self, covariate):
        orig_cov_name = copy.copy(covariate.name)
        # TODO need to rename edit_group_name_form to something more general...
        edit_cov_form = edit_group_name_form.EditCovariateName(orig_cov_name, parent=self)
        if edit_cov_form.exec_():
            # the field names are also poorly named, in this case. here we mean the 
            # **covariate name**, of course.
            new_cov = unicode(edit_cov_form.group_name_le.text().toUtf8(), "utf-8")
            
            # make sure the group name doesn't already exist
            if new_cov in self.model.dataset.get_cov_names():
                QMessageBox.warning(self,
                            "whoops.",
                            "%s is already a covariate name -- pick something else, please" % new_cov)
            
            else:
                ###
                # TODO implement rename_covariate!
                redo_f = lambda: self.model.rename_covariate(orig_cov_name, new_cov)
                undo_f = lambda: self.model.rename_covariate(new_cov, orig_cov_name)

                rename_cov_command = CommandGenericDo(redo_f, undo_f)
                self.tableView.undoStack.push(rename_cov_command)


    def delete_covariate(self, covariate):
        cov_vals_d = self.model.dataset.get_values_for_cov(covariate.name)
        undo_f = lambda : \
                    self.model.add_covariate(covariate.name, \
                                COV_INTS_TO_STRS[covariate.data_type], \
                                cov_values=cov_vals_d)
        redo_f = lambda : self.model.remove_covariate(covariate)
        delete_command = CommandGenericDo(redo_f, undo_f)
        self.tableView.undoStack.push(delete_command)  

    def _add_study(self, study, study_index=None):
        print "adding study: %s" % study.name
        self.model.dataset.add_study(study, study_index=study_index)
        self.model.reset()
        self.data_dirtied()
        
    def _remove_study(self, study):
        print "deleting study: %s" % study.name
        self.model.dataset.studies.remove(study)
        self.model.reset()
        self.data_dirtied()

    def set_model(self, data_model, state_dict=None, check_for_appropriate_metric=False):
        ##
        # we explicitly append a blank study to the
        # dataset iff there is fewer than 1 study 
        # in the dataset. in this case, the only 
        # row is essentially a blank study. 
        add_blank_study = len(data_model) < 1
        self.model = DatasetModel(dataset=data_model, 
                                    add_blank_study=add_blank_study)

        self._disconnections()
        if len(data_model) >= 2:
            self.enable_menu_options_that_require_dataset()
        else:
            self.disable_menu_options_that_require_dataset()
        
        # covariates?
        if len(data_model.covariates) > 0:
            self.action_meta_regression.setEnabled(True)
        else:
            self.action_meta_regression.setEnabled(False)

        self.tableView.setModel(self.model)

        ## moving the statefulendess 
        # update below the model swap-out
        # to fix issue #62
        if state_dict is not None:
            self.model.set_state(state_dict)

        self.tableView.model().update_column_indices()
        self.tableView.resizeColumnsToContents()
  
        if check_for_appropriate_metric:
            self.tableView.change_metric_if_appropriate()

        if self.model.get_current_outcome_type() == "diagnostic":
            # no cumulative MA for diagnostic data
            self.action_cum_ma.setEnabled(False)
        else:
            self.action_cum_ma.setEnabled(True)

        self.model_updated()
        self.data_dirtied()
        print "ok -- model set."
        
        
    def model_updated(self):
        ''' Call me when the model is changed. '''
        self.model.update_current_group_names()
        self.model.update_current_outcome()
        self.model.update_current_time_points()

        if self.model.current_outcome is not None:
            self.model.try_to_update_outcomes()
            
        # This is kind of subtle. We have to reconnect
        # our signals and slots when the underlying model 
        # changes, because otherwise the antiquated/replaced
        # model (which was connected to the slots of interest)
        # remains, which is useless. However, we do not
        # reconnect the menu_action options; this will cause those
        # methods to be called x times! (x being the number of times
        # _setup_connections is invoked)
        self._setup_connections(menu_actions=False)
        self.tableView.resizeColumnsToContents()
        self.update_outcome_lbl()
        self.update_follow_up_label()
        
        ####
        # adding check to ascertain that the menu
        # isn't already ready for the current kind of data
        cur_data_type = self.tableView.model().get_current_outcome_type(get_str=False)
        if self.metric_menu_is_set_for != cur_data_type:
            self.populate_metrics_menu(\
                metric_to_check=self.tableView.model().current_effect)

        self.model.reset()
        
        
    def undo_set_model(self, out_path, state_dict, dataset):
        self.model = DatasetModel(dataset)
        self.model.set_state(state_dict)
        self.out_path = out_path
        self._disconnections()
        self.tableView.setModel(self.model)
        self.model_updated()
        self.dataset_file_lbl.setText("open file: %s" % self.out_path)

        
    def update_outcome_lbl(self):
        self.cur_outcome_lbl.setText(\
                u"<font color='Blue'>%s</font>" % self.model.current_outcome)
        
    def quit(self):
        if self.current_data_unsaved:
            if self.user_is_going_to_lose_data():
                QApplication.quit()
        else:
            # otherwise, just shutdown
            QApplication.quit()

    def user_is_going_to_lose_data(self):
        save_first = QMessageBox.warning(self,
                        "Warning",
                        "you've made unsaved changes to your data. what do you want to do with them?",
                        QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel)
        
        if save_first == QMessageBox.Save:
            self.save()        

        # return true so long as the user didn't *cancel*
        return save_first != QMessageBox.Cancel

    def save_as(self):
        return self.save(save_as=True)

    def save(self, save_as=False):
        if self.out_path is None or save_as:
            # fix for issue #58,1. -- use current dataset name in save path
            out_f = os.path.join("." , self.model.get_name())
            out_f = unicode(QFileDialog.getSaveFileName(self, "OpenMeta[analyst] - Save File",
                                                         out_f, "open meta files: (.oma)"))
            if out_f == "" or out_f == None:
                return None
            else:
                self.out_path = out_f
        try:
            print "trying to write data out to: %s" % self.out_path
            f = open(self.out_path, 'wb')
            pickle.dump(self.model.dataset, f)
            f.close()
            # also write out the 'state', which contains things
            # pertaining to the view
            d = self.model.get_stateful_dict()
            f = open(self.out_path + ".state", 'wb')
            pickle.dump(d, f)
            f.close()
            ###
            # update user preferences to save the location of 
            # this dataset
            self.user_prefs['recent datasets'].append(self.out_path)
            self._save_user_prefs()
            self.dataset_file_lbl.setText("open file: %s" % self.out_path)
            self.current_data_unsaved = False
        except Exception, e:
            # @TODO handle this elegantly?
            print e
            raise Exception, "whoops. exception thrown attempting to save."

    def load_user_prefs(self):
        '''
        Attempts to read a local dictionary of preferences
        ('user_prefs.dict'). If not such file exists, this file
        is created with defaults.
        '''
        
        self.user_prefs = {}
        if os.path.exists(PREFS_PATH):
            try:
                self.user_prefs = pickle.load(open(PREFS_PATH))
            except:
                print "preferences dictionary is corrupt! using defaults"
                self.user_prefs = self._default_user_prefs()
        else:
            self.user_prefs = self._default_user_prefs()

        # for backwards-compatibility
        if not "method_params" in self.user_prefs:
            self.user_prefs["method_params"] = {}

        self._save_user_prefs()
        print "loaded user preferences: %s" % self.user_prefs


    def update_user_prefs(self, field, value):
        self.user_prefs[field] = value
        self._save_user_prefs()
            
    def get_user_method_params_d(self):
        return self.user_prefs["method_params"]

    def _save_user_prefs(self):
        try:
            fout = open(PREFS_PATH, 'wb')
            pickle.dump(self.user_prefs, fout)
            fout.close()
        except:
            print "failed to write preferences data!"
        
    def _default_user_prefs(self):       
        return {"splash":True, "digits":3, "recent datasets":[], 
                    "method_params":{}}
        
class CommandGenericDo(QUndoCommand):
    '''
   This is a generic undo/redo command that takes two unevaluated lambdas --
   thunks, if you will -- one for doing and one for undoing.
    '''
    def __init__(self, redo_f, undo_f, description=""):
        super(CommandGenericDo, self).__init__(description)
        self.redo_f = redo_f
        self.undo_f = undo_f
        
    def redo(self):
        self.redo_f()
        
    def undo(self):
        self.undo_f()
    
class CommandNext(QUndoCommand):
    '''
   This is an undo command for user navigation
    '''
    def __init__(self, redo_f, undo_f, description="command:: next dimension"):
        super(CommandNext, self).__init__(description)
        self.redo_f = redo_f
        self.undo_f = undo_f
        
    def redo(self):
        self.redo_f()
        
    def undo(self):
        self.undo_f()
        

#############################################################################
#  Unit tests! Use nose
#           [http://somethingaboutorange.com/mrl/projects/nose] or just
#           > easy_install nose
#
#   e.g., while in this directory:
#           > nosetests meta_form
#
##############################################################################
def _gen_some_data():
    ''' For testing purposes. Generates a toy dataset.'''
    dataset = Dataset()
    studies = [Study(i, name=study, year=y) for i, study, y in zip(range(3),
                        ["trik", "wallace", "lau"], [1984, 1990, 2000])]
    raw_data = [
                                [ [10, 100] , [15, 100] ], [ [20, 200] , [25, 200] ],
                                [ [30, 300] , [35, 300] ]
                      ]
  
    outcome = Outcome("death", BINARY)
    dataset.add_outcome(outcome)

    for study in studies:
        dataset.add_study(study)
    
    for study,data in zip(dataset.studies, raw_data):
        study.add_ma_unit(MetaAnalyticUnit(outcome, raw_data=data), "baseline")
    
    return dataset


def _setup_app():
    app = QtGui.QApplication(sys.argv)
    meta = MetaForm()
    meta.tableView.setSelectionMode(QTableView.ContiguousSelection)
    meta.show()
    return (meta, app)

def _tear_down_app(app):
    sys.exit(app.exec_())


def binary_meta_analysis_test():
    meta, app = _setup_app()
    meta.open(os.path.join("test_data", "amino.oma"))

    ####
    # TODO -- run through all metrics here

def copy_paste_test():
    meta, app = _setup_app()

    # generate some faux data, set up the
    # tableview model
    #data_model = _gen_some_data()
    meta.open(os.path.join("test_data", "amino.oma"))

    #test_model = DatasetModel(dataset=data_model)
    #meta.tableView.setModel(test_model)

    upper_left_index = meta.tableView.model().createIndex(0, 1)
    lower_right_index = meta.tableView.model().createIndex(1, 2)
    copied = meta.tableView.copy_contents_in_range(upper_left_index, lower_right_index,
                                                                                    to_clipboard=False)

    tester = "trik\t1984\nwallace\t1990"

    assert(copied == tester)
    print "ok.. copied correctly"
    
    # now ascertain that we can paste it. first, copy (the same string) to the clipboard
    copied = meta.tableView.copy_contents_in_range(upper_left_index, lower_right_index,
                                                                                to_clipboard=True)
    upper_left_index = meta.tableView.model().createIndex(1, 1)

    # originally, the second row is wallace
    assert(str(meta.tableView.model().data(upper_left_index).toString()) == "wallace")
    meta.tableView.paste_from_clipboard(upper_left_index)
    # now, the 2nd row (index:1) should contain trik
    assert(str(meta.tableView.model().data(upper_left_index).toString()) == "trik")


def test_add_new_outcome():
    meta, app = _setup_app()
    data_model = _gen_some_data()
    test_model = DatasetModel(dataset=data_model)
    meta.tableView.setModel(test_model)
    new_outcome_name = u"test outcome"
    new_outcome_type = "binary"
    meta._add_new_outcome(new_outcome_name, new_outcome_type)
    outcome_names = meta.model.dataset.get_outcome_names()
    assert(new_outcome_name in outcome_names)
    
def test_remove_outcome():
    meta, app = _setup_app()
    data_model = _gen_some_data()
    test_model = DatasetModel(dataset=data_model)
    meta.tableView.setModel(test_model)
    new_outcome_name = u"test outcome"
    new_outcome_type = "binary"
    meta._add_new_outcome(new_outcome_name, new_outcome_type)
    # note that we test the adding functionality elsewhere
    meta.model.dataset.remove_outcome(new_outcome_name)
    outcome_names = meta.model.dataset.get_outcome_names()
    assert (new_outcome_name not in outcome_names)
    
def paste_from_excel_test():
    meta, app = _setup_app()

    #set up the tableview model with a blank model
    test_model = DatasetModel()
    meta.tableView.setModel(test_model)
    upper_left_index = meta.tableView.model().createIndex(0, 1)
    # copied from an Excel spreadsheet
    copied_str = """a	1993
b	1785
"""
    clipboard = QApplication.clipboard()
    clipboard.setText(QString(copied_str))
    meta.tableView.paste_from_clipboard(upper_left_index)

    #
    # now make sure the content is there
    content = [["a", "1993"], ["b", "1785"]]
    for row in range(len(content)):
        for col in range(len(content[row])):
            # the plus one offsets the first column, which is the include/
            # exclude checkbox
            cur_index = meta.tableView.model().createIndex(row, col+1)
            cur_val = str(meta.tableView.model().data(cur_index).toString())
            should_be = content[row][col]
            print "cur val is %s; it should be %s" % (cur_val, should_be)
            assert(cur_val == should_be)



def start():
    welcome_str = "** welcome to OpenMeta; version %s **" % meta_globals.VERSION
    print "".join(["*" for x in range(len(welcome_str))])
    print welcome_str
    print "".join(["*" for x in range(len(welcome_str))])

    app = QtGui.QApplication(sys.argv)
    meta = MetaForm()
    meta.show()
    sys.exit(app.exec_())
    
#
# to launch:
#   >python meta_form.py
#
if __name__ == "__main__":
    start()

