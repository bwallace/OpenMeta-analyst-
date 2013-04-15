# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'diag_explain_form.ui'
#
# Created: Fri Apr 12 15:38:24 2013
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

class Ui_diag_explain_window(object):
    def setupUi(self, diag_explain_window):
        diag_explain_window.setObjectName(_fromUtf8("diag_explain_window"))
        diag_explain_window.resize(510, 239)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        diag_explain_window.setWindowIcon(icon)
        diag_explain_window.setModal(True)
        self.verticalLayout_2 = QtGui.QVBoxLayout(diag_explain_window)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.textBrowser = QtGui.QTextBrowser(diag_explain_window)
        self.textBrowser.setObjectName(_fromUtf8("textBrowser"))
        self.verticalLayout.addWidget(self.textBrowser)
        self.dont_show_again_chk_box = QtGui.QCheckBox(diag_explain_window)
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
        diag_explain_window.setWindowTitle(_translate("diag_explain_window", "Diagnostic MA with Multiple Metrics", None))
        self.textBrowser.setHtml(_translate("diag_explain_window", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Lucida Grande\'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'MS Shell Dlg 2\'; font-size:14pt; font-weight:600;\">A note on diagnostic meta-analysis for multiple metrics</span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'MS Shell Dlg 2\'; font-size:14pt;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'MS Shell Dlg 2\'; font-size:12pt;\">It looks like you\'re conducting meta-analyses for multiple diagnostic metrics. </span></p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px; font-family:\'MS Shell Dlg 2\'; font-size:12pt;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'MS Shell Dlg 2\'; font-size:12pt;\">Due to statistical properties of these metrics, different methods are available for sensitivity and specificity than are for likelihood and diagnostic odds ratios. So now we\'re going to first ask you to pick a meta-analysis method (and parameters) to use for sensitivity/specificity, and then we\'re going to ask you to pick a method to use for likelihood and diagnostic odds ratios. </span></p></body></html>", None))
        self.dont_show_again_chk_box.setText(_translate("diag_explain_window", "don\'t show this message next time", None))

import icons_rc
