import PyQt4
from PyQt4 import *
from PyQt4.QtCore import *
from PyQt4.QtSql import *

db = None

def init_db():
    pass
    
def init_tables():
    query = QSqlQuery()
    query.exec_("""CREATE TABLE test (
                            id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                            name VARCHAR(40),
                            tx_events INTEGER,
                            tx_N INTEGER,
                            ctrl_events INTEGER,
                            ctrl_N INTEGER)""")
    
def add_some_data():
    query = QSqlQuery()
    query.prepare("INSERT INTO test (name, tx_events, tx_N, ctrl_events, ctrl_N) VALUES (?, ?, ?, ?, ?)")
    query.addBindValue(QVariant(QString("hi")))
    query.addBindValue(QVariant(QString(10)))
    query.addBindValue(QVariant(QString(40)))
    query.addBindValue(QVariant(QString(7)))
    query.addBindValue(QVariant(QString(40)))
    query.exec_()