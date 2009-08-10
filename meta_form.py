#!/usr/bin/env python

#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  MetaAnalyst 2
##
##  Container form for UI
##  
#############################################################################

import sys
import pdb
from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
from PyQt4.QtSql import *
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
        data_model = gen_some_data()
        
        self.model = DatasetModel(dataset=data_model)
        
        #self.model.select()

        self.tableView.setModel(self.model)
        self.tableView.setSelectionMode(QTableView.ContiguousSelection)
        #self.tableView.setSelectionBehavior(QTableView.SelectRows)
        #self.tableView.setItemDelegate(QSqlRelationalDelegate(self))

        self.model.reset()
        #self.model = QSqlTableModel(self)
        #self.model.select()
        query = QSqlQuery()



def gen_some_data():
    dataset = Dataset()
    studies = [Study(i, name=study, year=y) for i, study, y in zip(range(3), ["trik", "wallace", "lau"], [1984, 1990, 2000])]
    raw_data = [[10, 100, 15, 100], [20, 200, 25, 200], [30, 300, 35, 300]]
    dataset.studies = studies
    for study,data in zip(dataset.studies, raw_data):
        study.raw_data = data
    return dataset
    
welcome_str = "** welcome to OpenMeta; version %s **" % .01
print "".join(["*" for x in range(len(welcome_str))])
print welcome_str
print "".join(["*" for x in range(len(welcome_str))])

app = QtGui.QApplication(sys.argv)
meta = MetaForm()
meta.show()
sys.exit(app.exec_())

'''
if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    meta = Ui_MainWindow
    meta.show()
    sys.exit(app.exec_())
'''