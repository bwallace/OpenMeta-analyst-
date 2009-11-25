# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'meta.ui'
#
# Created: Wed Aug 05 12:03:56 2009
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui
import ma_data_table_view

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName("MainWindow")
        MainWindow.resize(981, 629)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap("images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        MainWindow.setWindowIcon(icon)
        self.centralwidget = QtGui.QWidget(MainWindow)
        self.centralwidget.setObjectName("centralwidget")
        self.horizontalLayout = QtGui.QHBoxLayout(self.centralwidget)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.tabWidget = QtGui.QTabWidget(self.centralwidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.tabWidget.setFont(font)
        self.tabWidget.setObjectName("tabWidget")
        self.dataTab = QtGui.QWidget()
        self.dataTab.setObjectName("dataTab")
        self.horizontalLayout_2 = QtGui.QHBoxLayout(self.dataTab)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        #self.tableView = QtGui.QTableView(self.dataTab)
        self.tableView = ma_data_table_view.MADataTable(self.dataTab)
        self.tableView.setObjectName("tableView")
        self.horizontalLayout_2.addWidget(self.tableView)
        self.tabWidget.addTab(self.dataTab, "")
        self.resultsTab = QtGui.QWidget()
        self.resultsTab.setObjectName("resultsTab")
        self.tabWidget.addTab(self.resultsTab, "")
        self.horizontalLayout.addWidget(self.tabWidget)
        MainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(MainWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 981, 21))
        self.menubar.setObjectName("menubar")
        self.menuFile = QtGui.QMenu(self.menubar)
        self.menuFile.setObjectName("menuFile")
        self.menuNew_Dataset = QtGui.QMenu(self.menuFile)
        self.menuNew_Dataset.setObjectName("menuNew_Dataset")
        self.menuAnalysis = QtGui.QMenu(self.menubar)
        self.menuAnalysis.setObjectName("menuAnalysis")
        MainWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(MainWindow)
        self.statusbar.setObjectName("statusbar")
        MainWindow.setStatusBar(self.statusbar)
        self.actionBinary = QtGui.QAction(MainWindow)
        self.actionBinary.setObjectName("actionBinary")
        self.actionContinuous = QtGui.QAction(MainWindow)
        self.actionContinuous.setObjectName("actionContinuous")
        self.menuNew_Dataset.addAction(self.actionContinuous)
        self.menuFile.addAction(self.menuNew_Dataset.menuAction())
        self.menubar.addAction(self.menuFile.menuAction())
        self.menubar.addAction(self.menuAnalysis.menuAction())

        self.retranslateUi(MainWindow)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(QtGui.QApplication.translate("MainWindow", "OpenMeta", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.dataTab), QtGui.QApplication.translate("MainWindow", "Data", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.resultsTab), QtGui.QApplication.translate("MainWindow", "Results/Analysis", None, QtGui.QApplication.UnicodeUTF8))
        self.menuFile.setTitle(QtGui.QApplication.translate("MainWindow", "File", None, QtGui.QApplication.UnicodeUTF8))
        self.menuNew_Dataset.setTitle(QtGui.QApplication.translate("MainWindow", "New Dataset...", None, QtGui.QApplication.UnicodeUTF8))
        self.menuAnalysis.setTitle(QtGui.QApplication.translate("MainWindow", "Analysis", None, QtGui.QApplication.UnicodeUTF8))
        self.actionBinary.setText(QtGui.QApplication.translate("MainWindow", "Binary", None, QtGui.QApplication.UnicodeUTF8))
        self.actionContinuous.setText(QtGui.QApplication.translate("MainWindow", "Continuous", None, QtGui.QApplication.UnicodeUTF8))

