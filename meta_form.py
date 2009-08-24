#!/usr/bin/env python

#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##
##  Container form for UI
##  
#############################################################################

import sys
import pdb
from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
import nose # for unit tests
import ui_meta
import ma_data_table_model
from ma_data_table_model import *
import ma_dataset
from ma_dataset import *
import meta_form
import new_outcome_form

class MetaForm(QtGui.QMainWindow, ui_meta.Ui_MainWindow):
    
    def __init__(self, parent=None):
        #
        # We follow the advice given by Mark Summerfield in his Python QT book: Namely, we
        # use multiple inheritence to gain access to the ui. 
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
        else:
            data_model = Dataset()
            self.model = DatasetModel(dataset=data_model)

        self.tableView.setModel(self.model)
        
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
    
    def _setup_connections(self):
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
        
    def add_new(self):
        if self.cur_dimension == "outcome":
            form =  new_outcome_form.AddNewOutcomeForm(self) 
            form.outcome_name_le.setFocus()
            if form.exec_():
                # then the user clicked ok and has added a new outcome.
                # here we want to add the outcome to the dataset, and then
                # display it
                name = form.outcome_name_le.text()
                type = str(form.datatype_cbo_box.currentText())
                self.model.add_new_outcome(name, type)
                self.display_outcome(name)
                
                
    def next(self):
        if self.cur_dimension == "outcome":
            next_outcome = self.model.get_next_outcome_name()
            self.display_outcome(next_outcome)
            
        if self.cur_dimension in ["outcome, folow-up"]:
            self.update_undo_stack()
            
    def previous(self):
        if self.cur_dimension == "outcome":
            next_outcome = self.model.get_prev_outcome_name()
            self.display_outcome(next_outcome)     
        
    def next_dimension(self):
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
    
        
    def display_outcome(self, outcome_name):
        self.model.set_current_outcome(outcome_name)        
        self.model.set_current_time_point(0)
        self.cur_outcome_lbl.setText(u"<font color='Blue'>%s</font>" % outcome_name)
        self.cur_time_lbl.setText(u"<font color='Blue'>%s</font>" % self.model.current_time_point)
        self.model.reset()
        
################################################################
#  Unit tests! Use nose 
#           [http://somethingaboutorange.com/mrl/projects/nose] or just
#           > easy_install nose
#
#   e.g., while in this directory:
#           > nosetests meta_form
#
################################################################

def _gen_some_data():
    ''' For testing purposes. Generates a toy dataset.'''
    dataset = Dataset()
    studies = [Study(i, name=study, year=y) for i, study, y in zip(range(3), 
                        ["trik", "wallace", "lau"], [1984, 1990, 2000])]
    raw_data = [
                                [ [10, 100] , [15, 100] ], [ [20, 200] , [25, 200] ], 
                                [ [30, 300] , [35, 300] ]
                      ]
    dataset.studies = studies
    outcome = Outcome("death", BINARY)
    for study,data in zip(dataset.studies, raw_data):
        study.add_ma_unit(MetaAnalyticUnit(outcome, raw_data=data), 0)
    
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

    upper_left_index = meta.tableView.model().createIndex(0, 0)
    lower_right_index = meta.tableView.model().createIndex(1, 1)
    copied = meta.tableView.copy_contents_in_range(upper_left_index, lower_right_index, 
                                                                                    to_clipboard=False)

    tester = "trik\t1984\nwallace\t1990"
    assert(copied == tester)

    # now ascertain that we can paste it. first, copy (the same string) to the clipboard
    copied = meta.tableView.copy_contents_in_range(upper_left_index, lower_right_index, 
                                                                                to_clipboard=True)
    upper_left_index = meta.tableView.model().createIndex(1, 0)
 
    # originally, the second row is wallace
    assert(str(meta.tableView.model().data(upper_left_index).toString()) == "wallace")
    meta.tableView.paste_from_clipboard(upper_left_index)
    # now, the 2nd row (index:1) should contain trik
    assert(str(meta.tableView.model().data(upper_left_index).toString()) == "trik")

def undo_redo_test():
    meta, app = _setup_app()
    test_model = DatasetModel()
    meta.tableView.setModel(test_model)
    assert(True)
    
def paste_from_excel_test():
    meta, app = _setup_app()
    
    #set up the tableview model with a blank model
    test_model = DatasetModel()
    meta.tableView.setModel(test_model)
    upper_left_index = meta.tableView.model().createIndex(0, 0)
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
            cur_index = meta.tableView.model().createIndex(row, col)
            cur_val = str(meta.tableView.model().data(cur_index).toString())
            should_be = content[row][col]
            print "cur val is %s; it should be %s" % (cur_val, should_be)
            assert(cur_val == should_be)


#
# to launch:
#   >python meta_form.py
#
if __name__ == "__main__":
    welcome_str = "** welcome to OpenMeta; version %s **" % .01
    print "".join(["*" for x in range(len(welcome_str))])
    print welcome_str
    print "".join(["*" for x in range(len(welcome_str))])
    
    app = QtGui.QApplication(sys.argv)
    meta = MetaForm()
    meta.show()
    sys.exit(app.exec_())
