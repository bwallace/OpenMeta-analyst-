# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'results_window.ui'
#
# Created: Tue Mar 19 15:55:48 2013
#      by: PyQt4 UI code generator 4.9.6
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_ResultsWindow(object):
    def setupUi(self, ResultsWindow):
        ResultsWindow.setObjectName(_fromUtf8("ResultsWindow"))
        ResultsWindow.resize(799, 544)
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
        self.horizontalLayout = QtGui.QHBoxLayout(self.frame)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.results_nav_splitter = QtGui.QSplitter(self.frame)
        self.results_nav_splitter.setOrientation(QtCore.Qt.Horizontal)
        self.results_nav_splitter.setObjectName(_fromUtf8("results_nav_splitter"))
        self.nav_tree = QtGui.QTreeWidget(self.results_nav_splitter)
        self.nav_tree.setMaximumSize(QtCore.QSize(16777215, 16777215))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.nav_tree.setFont(font)
        self.nav_tree.setObjectName(_fromUtf8("nav_tree"))
        self.nav_tree.headerItem().setText(0, _fromUtf8("1"))
        self.graphics_view = QtGui.QGraphicsView(self.results_nav_splitter)
        self.graphics_view.setToolTip(_fromUtf8(""))
        self.graphics_view.setObjectName(_fromUtf8("graphics_view"))
        self.horizontalLayout.addWidget(self.results_nav_splitter)
        self.psuedo_console = QtGui.QTextEdit(self.splitter)
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
        self.menubar.setGeometry(QtCore.QRect(0, 0, 799, 22))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        ResultsWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(ResultsWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        ResultsWindow.setStatusBar(self.statusbar)

        self.retranslateUi(ResultsWindow)
        QtCore.QMetaObject.connectSlotsByName(ResultsWindow)

    def retranslateUi(self, ResultsWindow):
        ResultsWindow.setWindowTitle(_translate("ResultsWindow", "results / analysis", None))
        self.psuedo_console.setHtml(_translate("ResultsWindow", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Terminal\'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">&gt;&gt; </span></p></body></html>", None))

