# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'results_window.ui'
#
# Created: Thu Nov 19 12:05:38 2009
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_results_window(object):
    def setupUi(self, results_window):
        results_window.setObjectName("results_window")
        results_window.resize(800, 600)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        results_window.setFont(font)
        self.centralwidget = QtGui.QWidget(results_window)
        self.centralwidget.setObjectName("centralwidget")
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName("verticalLayout")
        self.graphicsView = QtGui.QGraphicsView(self.centralwidget)
        self.graphicsView.setObjectName("graphicsView")
        self.verticalLayout.addWidget(self.graphicsView)
        self.lineEdit = QtGui.QLineEdit(self.centralwidget)
        self.lineEdit.setObjectName("lineEdit")
        self.verticalLayout.addWidget(self.lineEdit)
        results_window.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(results_window)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 21))
        self.menubar.setObjectName("menubar")
        results_window.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(results_window)
        self.statusbar.setObjectName("statusbar")
        results_window.setStatusBar(self.statusbar)

        self.retranslateUi(results_window)
        QtCore.QMetaObject.connectSlotsByName(results_window)

    def retranslateUi(self, results_window):
        results_window.setWindowTitle(QtGui.QApplication.translate("results_window", "Results / Analysis", None, QtGui.QApplication.UnicodeUTF8))

