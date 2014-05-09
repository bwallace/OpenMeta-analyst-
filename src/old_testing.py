#############################################################################
#  Unit tests! Use nose
#           [http://somethingaboutorange.com/mrl/projects/nose] or just
#           > easy_install nose
#
#   e.g., while in this directory:
#           > nosetests meta_form
#
##############################################################################



import sys
import os
from PyQt4 import QtCore, QtGui
from PyQt4.Qt import *


import ma_data_table_model
import meta_globals
import ma_dataset
from meta_form import MetaForm

#         ####################################################
#         # this (toy-data) is almost certainly antiquated   #
#         # and should be removed or updated                 #
#         ####################################################
#         if len(sys.argv)>1 and sys.argv[-1]=="--toy-data":
#             # toy data for now
#             data_model = _gen_some_data()
#             self.model = ma_data_table_model.DatasetModel(dataset=data_model)
#             self.display_outcome("death")
#             self.model.set_current_time_point(0)
#             self.model.current_effect = "OR"
#             self.model.try_to_update_outcomes()
#             self.model.reset()
#             self.tableView.resizeColumnsToContents()
#         else:

def _gen_some_data():
    ''' For testing purposes. Generates a toy dataset.'''
    dataset = ma_dataset.Dataset()
    studies = [ma_dataset.Study(i, name=study, year=y) for i, study, y in zip(range(3),
                        ["trik", "wallace", "lau"], [1984, 1990, 2000])]
    raw_data = [
                                [ [10, 100] , [15, 100] ], [ [20, 200] , [25, 200] ],
                                [ [30, 300] , [35, 300] ]
                      ]
  
    outcome = ma_dataset.Outcome("death", meta_globals.BINARY)
    dataset.add_outcome(outcome)

    for study in studies:
        dataset.add_study(study)
    
    for study,data in zip(dataset.studies, raw_data):
        study.add_ma_unit(ma_dataset.MetaAnalyticUnit(outcome, raw_data=data), "baseline")
    
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
    test_model = ma_data_table_model.DatasetModel(dataset=data_model)
    meta.tableView.setModel(test_model)
    new_outcome_name = u"test outcome"
    new_outcome_type = "binary"
    meta._add_new_outcome(new_outcome_name, new_outcome_type)
    outcome_names = meta.model.dataset.get_outcome_names()
    assert(new_outcome_name in outcome_names)
    
def test_remove_outcome():
    meta, app = _setup_app()
    data_model = _gen_some_data()
    test_model = ma_data_table_model.DatasetModel(dataset=data_model)
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
    test_model = ma_data_table_model.DatasetModel()
    meta.tableView.setModel(test_model)
    upper_left_index = meta.tableView.model().createIndex(0, 1)
    # copied from an Excel spreadsheet
    copied_str = """a    1993
b    1785
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