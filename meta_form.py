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
import model
import ma_data_table

class MetaForm(QtGui.QMainWindow, ui_meta.Ui_MainWindow):
    
    def __init__(self, parent=None):
        #
        # We follow the advice given by Mark Summerfield in his Python QT book: Namely, we
        # use multiple inheritence to gain access to the ui. 
        #
        super(MetaForm, self).__init__(parent)
        self.setupUi(self)
        self.model = QSqlRelationalTableModel(self)
        
        
        self.model.setTable("test")
        self.model.select()

        self.tableView.setModel(self.model)
        self.tableView.setSelectionMode(QTableView.SingleSelection)
        self.tableView.setSelectionBehavior(QTableView.SelectRows)
        self.tableView.setItemDelegate(QSqlRelationalDelegate(self))

        #self.model = QSqlTableModel(self)
        self.model.select()
        query = QSqlQuery()



def init_tables():
    query = QSqlQuery()
    query.exec_("""CREATE TABLE test (
                            id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                            name VARCHAR(40),
                            tx_events INTEGER,
                            tx_N INTEGER,
                            ctrl_events INTEGER,
                            ctrl_N INTEGER)""")
    QSqlDatabase.database().commit()
    
def add_some_data():
    #QSqlDatabase.database().transaction()
    query = QSqlQuery()
    query.exec_("INSERT INTO test (id, name, tx_events, tx_N, ctrl_events, ctrl_N) VALUES (null, 'arg', 10 , 10, 10, 10)")
    QSqlDatabase.database().commit()
    
welcome_str = "** welcome to OpenMeta; version %s **" % .01
print "".join(["*" for x in range(len(welcome_str))])
print welcome_str
print "".join(["*" for x in range(len(welcome_str))])

app = QtGui.QApplication(sys.argv)
db = QSqlDatabase.addDatabase("QSQLITE")
db.setDatabaseName(":memory:")
db.open()
init_tables()
add_some_data()
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