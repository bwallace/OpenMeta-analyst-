# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cov_subgroup_dlg.ui'
#
# Created: Tue Jun 14 12:21:35 2011
#      by: PyQt4 UI code generator 4.7.7
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_cov_subgroup_dialog(object):
    def setupUi(self, cov_subgroup_dialog):
        cov_subgroup_dialog.setObjectName(_fromUtf8("cov_subgroup_dialog"))
        cov_subgroup_dialog.resize(301, 132)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        cov_subgroup_dialog.setFont(font)
        self.buttonBox = QtGui.QDialogButtonBox(cov_subgroup_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 100, 281, 32))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.label = QtGui.QLabel(cov_subgroup_dialog)
        self.label.setGeometry(QtCore.QRect(20, 30, 121, 41))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        font.setPointSize(10)
        self.label.setFont(font)
        self.label.setObjectName(_fromUtf8("label"))
        self.cov_subgroup_cbo_box = QtGui.QComboBox(cov_subgroup_dialog)
        self.cov_subgroup_cbo_box.setGeometry(QtCore.QRect(130, 40, 152, 20))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.cov_subgroup_cbo_box.setFont(font)
        self.cov_subgroup_cbo_box.setObjectName(_fromUtf8("cov_subgroup_cbo_box"))

        self.retranslateUi(cov_subgroup_dialog)
        QtCore.QMetaObject.connectSlotsByName(cov_subgroup_dialog)

    def retranslateUi(self, cov_subgroup_dialog):
        cov_subgroup_dialog.setWindowTitle(QtGui.QApplication.translate("cov_subgroup_dialog", "select covariate", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("cov_subgroup_dialog", "covariate for \n"
"subgroups:", None, QtGui.QApplication.UnicodeUTF8))

