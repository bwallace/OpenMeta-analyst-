#!/usr/bin/env python

#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMetaanalyst
##
##  Container form for UI
##  
#############################################################################

import sys
import pdb
from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
from PyQt4.QtSql import *
import nose # for unit tests
import ui_meta
import ma_data_table_model
from ma_data_table_model import *
import ma_dataset
from ma_dataset import *

class MetaForm(QtGui.QMainWindow, ui_meta.Ui_MainWindow):
    
    def __init__(self, parent=None):
        #
        # We follow the advice given by Mark Summerfield in his Python QT book: Namely, we
        # use multiple inheritence to gain access to the ui. 
        #
        super(MetaForm, self).__init__(parent)
        self.setupUi(self)
        # toy data for now
        # data_model = _gen_some_data()
        data_model = Dataset()
        self.model = DatasetModel(dataset=data_model)
        self.tableView.setModel(self.model)
        self.tableView.setSelectionMode(QTableView.ContiguousSelection)
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
    raw_data = [[10, 100, 15, 100], [20, 200, 25, 200], 
                                [30, 300, 35, 300]]
    dataset.studies = studies
    for study,data in zip(dataset.studies, raw_data):
        study.raw_data = data
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


#
# to launch:
#   >python meta_form.py to 
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
