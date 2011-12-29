# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'diagnostic_explain_dlg.ui'
#
# Created: Thu Dec 29 10:13:35 2011
#      by: PyQt4 UI code generator 4.8.5
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_diag_explain_window(object):
    def setupUi(self, diag_explain_window):
        diag_explain_window.setObjectName(_fromUtf8("diag_explain_window"))
        diag_explain_window.resize(510, 239)
        diag_explain_window.setWindowTitle(QtGui.QApplication.translate("diag_explain_window", "Diagnostic MA with Multiple Metrics", None, QtGui.QApplication.UnicodeUTF8))
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        diag_explain_window.setWindowIcon(icon)
        diag_explain_window.setModal(True)
        self.verticalLayout_2 = QtGui.QVBoxLayout(diag_explain_window)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.textBrowser = QtGui.QTextBrowser(diag_explain_window)
        self.textBrowser.setHtml(QtGui.QApplication.translate("diag_explain_window", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'MS Shell Dlg 2\'; font-size:8.25pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:12pt; font-weight:600;\">A note on diagnostic meta-analysis for multiple metrics</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:8pt;\"></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">It looks like you\'re conducting meta-analyses for multiple diagnostic metrics. </span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-size:8pt;\"></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:8pt;\">Due to statistical properties of these metrics, different methods are available for sensitivity and specificity than are for likelihood and diagnostic odds ratios. So now we\'re going to ask you to pick a meta-analysis method to use for these two sets of metrics. </span></p></body></html>", None, QtGui.QApplication.UnicodeUTF8))
        self.textBrowser.setObjectName(_fromUtf8("textBrowser"))
        self.verticalLayout.addWidget(self.textBrowser)
        self.dont_show_again_chk_box = QtGui.QCheckBox(diag_explain_window)
        self.dont_show_again_chk_box.setText(QtGui.QApplication.translate("diag_explain_window", "don\'t show this message next time", None, QtGui.QApplication.UnicodeUTF8))
        self.dont_show_again_chk_box.setObjectName(_fromUtf8("dont_show_again_chk_box"))
        self.verticalLayout.addWidget(self.dont_show_again_chk_box)
        self.buttonBox = QtGui.QDialogButtonBox(diag_explain_window)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)
        self.verticalLayout_2.addLayout(self.verticalLayout)

        self.retranslateUi(diag_explain_window)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), diag_explain_window.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), diag_explain_window.reject)
        QtCore.QMetaObject.connectSlotsByName(diag_explain_window)

    def retranslateUi(self, diag_explain_window):
        pass

import icons_rc
