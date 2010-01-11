# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'results_window.ui'
#
# Created: Mon Nov 23 13:59:45 2009
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui
import ma_text_edit

class Ui_ResultsWindow(object):
    def setupUi(self, ResultsWindow):
        ResultsWindow.setObjectName("ResultsWindow")
        ResultsWindow.resize(823, 522)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        ResultsWindow.setFont(font)
        self.centralwidget = QtGui.QWidget(ResultsWindow)
        self.centralwidget.setObjectName("centralwidget")
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.nav_tree = QtGui.QTreeWidget(self.centralwidget)
        self.nav_tree.setMaximumSize(QtCore.QSize(140, 16777215))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.nav_tree.setFont(font)
        self.nav_tree.setAnimated(True)
        self.nav_tree.setColumnCount(1)
        self.nav_tree.setObjectName("nav_tree")
        self.horizontalLayout.addWidget(self.nav_tree)
        self.graphics_view = QtGui.QGraphicsView(self.centralwidget)
        self.graphics_view.setObjectName("graphics_view")
        self.horizontalLayout.addWidget(self.graphics_view)
        self.verticalLayout.addLayout(self.horizontalLayout)
        #self.psuedo_console = QtGui.QLineEdit(self.centralwidget)
        self.psuedo_console = ma_text_edit.MATextEdit(self.centralwidget)
        #self.psuedo_console.setMaximumSize(QtCore.QSize(16777215, 40))
        font = QtGui.QFont()
        font.setFamily("Terminal")
        self.psuedo_console.setFont(font)
        self.psuedo_console.setAutoFillBackground(False)
        self.psuedo_console.setStyleSheet("""background-color: rgb(0, 0, 0);
color: rgb(0, 255, 0);""")
        self.psuedo_console.setLineWrapMode(QtGui.QTextEdit.NoWrap)
        self.psuedo_console.setAcceptRichText(False)
        self.psuedo_console.setObjectName("psuedo_console")
        self.verticalLayout.addWidget(self.psuedo_console)
        ResultsWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(ResultsWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 823, 21))
        self.menubar.setObjectName("menubar")
        ResultsWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(ResultsWindow)
        self.statusbar.setObjectName("statusbar")
        ResultsWindow.setStatusBar(self.statusbar)

        self.retranslateUi(ResultsWindow)
        QtCore.QMetaObject.connectSlotsByName(ResultsWindow)

    def retranslateUi(self, ResultsWindow):
        ResultsWindow.setWindowTitle(QtGui.QApplication.translate("ResultsWindow", "Results / Analysis", None, QtGui.QApplication.UnicodeUTF8))
        self.nav_tree.headerItem().setText(0, QtGui.QApplication.translate("ResultsWindow", "1", None, QtGui.QApplication.UnicodeUTF8))
        self.psuedo_console.setHtml(QtGui.QApplication.translate("ResultsWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Terminal\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:8pt;\">&gt;&gt; </p></body></html>", None, QtGui.QApplication.UnicodeUTF8))

