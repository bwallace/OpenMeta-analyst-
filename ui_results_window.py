# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'results_window.ui'
#
# Created: Mon Sep 26 09:08:25 2011
#      by: PyQt4 UI code generator 4.8.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui
import qconsole

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_ResultsWindow(object):
    def setupUi(self, ResultsWindow):
        ResultsWindow.setObjectName(_fromUtf8("ResultsWindow"))
        ResultsWindow.resize(832, 544)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        ResultsWindow.setFont(font)
        self.centralwidget = QtGui.QWidget(ResultsWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.splitter = QtGui.QSplitter(self.centralwidget)
        self.splitter.setOrientation(QtCore.Qt.Vertical)
        self.splitter.setObjectName(_fromUtf8("splitter"))
        self.frame = QtGui.QFrame(self.splitter)
        self.frame.setMinimumSize(QtCore.QSize(733, 0))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.frame.setFont(font)
        self.frame.setFrameShape(QtGui.QFrame.StyledPanel)
        self.frame.setFrameShadow(QtGui.QFrame.Raised)
        self.frame.setObjectName(_fromUtf8("frame"))
        self.gridLayout = QtGui.QGridLayout(self.frame)
        self.gridLayout.setMargin(1)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.nav_tree = QtGui.QTreeWidget(self.frame)
        self.nav_tree.setMaximumSize(QtCore.QSize(140, 16777215))
        self.nav_tree.setObjectName(_fromUtf8("nav_tree"))
        self.nav_tree.headerItem().setText(0, _fromUtf8("1"))
        self.gridLayout.addWidget(self.nav_tree, 0, 0, 1, 1)
        self.graphics_view = QtGui.QGraphicsView(self.frame)
        self.graphics_view.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOn)
        self.graphics_view.setObjectName(_fromUtf8("graphics_view"))
        self.gridLayout.addWidget(self.graphics_view, 0, 1, 1, 1)
        #self.psuedo_console = QtGui.QTextEdit(self.splitter)
        self.psuedo_console = qconsole.QConsole(self.splitter)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.psuedo_console.sizePolicy().hasHeightForWidth())
        self.psuedo_console.setSizePolicy(sizePolicy)
        self.psuedo_console.setMinimumSize(QtCore.QSize(733, 0))
        self.psuedo_console.setMaximumSize(QtCore.QSize(16777215, 16777215))
        self.psuedo_console.setBaseSize(QtCore.QSize(0, 0))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Terminal"))
        self.psuedo_console.setFont(font)
        self.psuedo_console.setAutoFillBackground(False)
        self.psuedo_console.setStyleSheet(_fromUtf8("background-color: rgb(0, 0, 0);\n"
"color: rgb(0, 255, 0);"))
        self.psuedo_console.setLineWrapMode(QtGui.QTextEdit.NoWrap)
        self.psuedo_console.setAcceptRichText(False)
        self.psuedo_console.setObjectName(_fromUtf8("psuedo_console"))
        self.verticalLayout.addWidget(self.splitter)
        ResultsWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(ResultsWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 832, 20))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        ResultsWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(ResultsWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        ResultsWindow.setStatusBar(self.statusbar)

        self.retranslateUi(ResultsWindow)
        QtCore.QMetaObject.connectSlotsByName(ResultsWindow)

    def retranslateUi(self, ResultsWindow):
        ResultsWindow.setWindowTitle(QtGui.QApplication.translate("ResultsWindow", "results / analysis", None, QtGui.QApplication.UnicodeUTF8))
        self.psuedo_console.setHtml(QtGui.QApplication.translate("ResultsWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Terminal\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">&gt;&gt; </span></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))

