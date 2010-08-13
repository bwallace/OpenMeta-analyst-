###########################
#                                           
#  Byron C. Wallace                   
#  Tufts Medical Center               
#  OpenMeta[analyst]                  
#                                     
#  Container form for UI. Handles     
#  user interaction.                  
#                                     
###########################

import sys
import pdb
import pickle
from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
import nose # for unit tests
import copy

#
# hand-rolled modules
#
import ui_meta
from ma_data_table_view import StudyDelegate
import ma_data_table_view
from ma_data_table_model import *
import ma_dataset
from ma_dataset import *
import meta_py_r

#
# additional forms
#
import add_new_dialogs
import results_window
import ma_specs
import edit_dialog
import network_view

VERSION = .003
NUM_DIGITS = 4

class MetaForm(QtGui.QMainWindow, ui_meta.Ui_MainWindow):

    def __init__(self, parent=None):
        #
        # We follow the advice given by Mark Summerfield in his Python QT book: 
        # Namely, we use multiple inheritence to gain access to the ui. We take
        # this approach throughout OpenMeta.
        #
        super(MetaForm, self).__init__(parent)
        self.setupUi(self)

        # this is just for debugging purposes; if a
        # switch is passed in, display fake data
        if len(sys.argv)>1 and sys.argv[-1]=="--toy-data":
            # toy data for now
            data_model = _gen_some_data()
            self.model = DatasetModel(dataset=data_model)
            self.display_outcome("death")
            self.model.set_current_time_point(0)
            self.model.try_to_update_outcomes()
        else:
            data_model = Dataset()
            self.model = DatasetModel(dataset=data_model)

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


    def keyPressEvent(self, event):
        if (event.modifiers() & QtCore.Qt.ControlModifier):
            if event.key() == QtCore.Qt.Key_S:
                # ctrl + s = save
                print "saving?"
                self.save()
            elif event.key() == QtCore.Qt.Key_O:
                # ctrl + o = open
                self.open()
            elif event.key() == QtCore.Qt.Key_A:
                self.analysis()

    def _setup_connections(self):
        ''' Signals & slots '''
        QObject.connect(self.tableView.model(), SIGNAL("cellContentChanged(QModelIndex, QVariant, QVariant)"),
                                                                                    self.tableView.cell_content_changed)
        QObject.connect(self.tableView.model(), SIGNAL("outcomeChanged()"),
                                                                                    self.tableView.displayed_ma_changed)
        QObject.connect(self.tableView.model(), SIGNAL("followUpChanged()"),
                                                                                    self.tableView.displayed_ma_changed)
        QObject.connect(self.nav_add_btn, SIGNAL("pressed()"), self.add_new)
        QObject.connect(self.nav_right_btn, SIGNAL("pressed()"), self.next)
        QObject.connect(self.nav_left_btn, SIGNAL("pressed()"), self.previous)
        QObject.connect(self.nav_up_btn, SIGNAL("pressed()"), self.next_dimension)
        QObject.connect(self.nav_down_btn, SIGNAL("pressed()"), self.previous_dimension)

        QObject.connect(self.action_save, SIGNAL("triggered()"), self.save)
        QObject.connect(self.action_open, SIGNAL("triggered()"), self.open)
        QObject.connect(self.action_quit, SIGNAL("triggered()"), self.quit)
        QObject.connect(self.action_go, SIGNAL("triggered()"), self.go)
        
        QObject.connect(self.action_edit, SIGNAL("triggered()"), self.edit_dataset)
        QObject.connect(self.action_view_network, SIGNAL("triggered()"), self.view_network)

    def go(self):
        # the spec form gets *this* form as a parameter.
        # this allows the spec form to callback to this
        # module when specifications have been provided.
        form =  ma_specs.MA_Specs(self.model, parent=self)
        form.show()

    def edit_dataset(self):
        cur_dataset = copy.deepcopy(self.model.dataset)
        edit_window =  edit_dialog.EditDialog(cur_dataset, parent=self)
    
        if edit_window.exec_():
            self.display_outcome(self.model.current_outcome)
            print "current outcome is %s" % self.model.current_outcome
            print edit_window.dataset.studies[0].outcomes_to_follow_ups
            modified_dataset = edit_window.dataset
            redo_f = lambda : self.set_model(modified_dataset)
            original_dataset = copy.deepcopy(self.model.dataset)
            undo_f = lambda : self.set_model(original_dataset) 
            print original_dataset.outcome_names_to_follow_ups
            edit_command = CommandGenericDo(redo_f, undo_f)
            self.tableView.undoStack.push(edit_command)
            
        
    def set_model(self, dataset):
        self.model.dataset = dataset
        self.model.update_current_group_names()
        self.model.update_current_outcome()
        self.model.update_current_time_points()
        self.model.try_to_update_outcomes()
        self.update_follow_up_label()
        self.display_outcome(self.model.current_outcome)
        self.model.reset()
        self.tableView.resizeColumnsToContents()
        print "ok -- model set" 
        
    def view_network(self):
        view_window =  network_view.ViewDialog(self.model, parent=self)
        view_window.show()
        
    def analysis(self, results):
        form = results_window.ResultsWindow(results, parent=self)
        form.show()

    def add_new(self):
        redo_f, undo_f = None, None
        if self.cur_dimension == "outcome":
            form =  add_new_dialogs.AddNewOutcomeForm(self)
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
            
        #self.model.set_current_time_point(0)
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
        
    def open(self):
        file_path = unicode(QFileDialog.getOpenFileName(self, "OpenMeta[analyst] - Open File",
                                                                                        ".", "open meta files (*.oma)"))
        data_model = None
        print "loading %s..." % file_path
        try:
            data_model = pickle.load(open(file_path, 'r'))
        except:
            return None
        
        self.out_path = file_path
        # this is questionable; we explicitly remove the last study, because
        # there is *always* a blank study appended to the current dataset.
        # thus when the dataset was dumped (via pickle) it included this study,
        # but the model will append *another* blank study to the dataset
        # when it is opened. this was the easiest way to resolve this issue.
        data_model.studies = data_model.studies[:-1]
        self.model = DatasetModel(dataset=data_model)

        state_dict = None
        try:
            state_dict = pickle.load(open(file_path + ".state"))
            self.model.set_state(state_dict)
            print "found state dictionary: \n%s" % state_dict
        except:
            print "no state dictionary found!"

        self.tableView.setModel(self.model)
        self.tableView.resizeColumnsToContents()
        self.cur_outcome_lbl.setText(u"<font color='Blue'>%s</font>" % self.model.current_outcome)
        self.update_follow_up_label()
        print "successfully loaded data"


    def quit(self):
        QApplication.quit()

    def save(self):
        if self.out_path is None:
            out_f = "."
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
        except Exception, e:
            # @TODO handle this elegantly?
            print e
            raise Exception, "whoops. exception thrown attempting to save."


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
        

########################################################
#  Unit tests! Use nose
#           [http://somethingaboutorange.com/mrl/projects/nose] or just
#           > easy_install nose
#
#   e.g., while in this directory:
#           > nosetests meta_form
#
########################################################
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
    
    # self.get_current_ma_unit_for_study(index.row()).get_raw_data_for_groups(self.current_txs)
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

def copy_paste_test():
    meta, app = _setup_app()

    # generate some faux data, set up the
    # tableview model
    data_model = _gen_some_data()
    test_model = DatasetModel(dataset=data_model)
    meta.tableView.setModel(test_model)

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
    
#def test_add
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



#
# to launch:
#   >python meta_form.py
#
if __name__ == "__main__":
    welcome_str = "** welcome to OpenMeta; version %s **" % VERSION
    print "".join(["*" for x in range(len(welcome_str))])
    print welcome_str
    print "".join(["*" for x in range(len(welcome_str))])

    app = QtGui.QApplication(sys.argv)
    meta = MetaForm()
    meta.show()
    sys.exit(app.exec_())
