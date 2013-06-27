# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'outcome_name_page.ui'
#
# Created: Thu Jun 27 10:21:34 2013
#      by: PyQt4 UI code generator 4.10.1
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

class Ui_WizardPage(object):
    def setupUi(self, WizardPage):
        WizardPage.setObjectName(_fromUtf8("WizardPage"))
        WizardPage.resize(285, 46)
        WizardPage.setMinimumSize(QtCore.QSize(285, 45))
        WizardPage.setMaximumSize(QtCore.QSize(300, 50))
        WizardPage.setSubTitle(_fromUtf8(""))
        self.horizontalLayout = QtGui.QHBoxLayout(WizardPage)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.label = QtGui.QLabel(WizardPage)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label.sizePolicy().hasHeightForWidth())
        self.label.setSizePolicy(sizePolicy)
        self.label.setObjectName(_fromUtf8("label"))
        self.horizontalLayout.addWidget(self.label)
        self.outcome_name_LineEdit = QtGui.QLineEdit(WizardPage)
        self.outcome_name_LineEdit.setEnabled(True)
        self.outcome_name_LineEdit.setText(_fromUtf8(""))
        self.outcome_name_LineEdit.setObjectName(_fromUtf8("outcome_name_LineEdit"))
        self.horizontalLayout.addWidget(self.outcome_name_LineEdit)
        self.label.setBuddy(self.outcome_name_LineEdit)

        self.retranslateUi(WizardPage)
        QtCore.QMetaObject.connectSlotsByName(WizardPage)

    def retranslateUi(self, WizardPage):
        WizardPage.setWindowTitle(_translate("WizardPage", "WizardPage", None))
        WizardPage.setTitle(_translate("WizardPage", "What is the name of your outcome?", None))
        self.label.setText(_translate("WizardPage", "Outcome Name:", None))

