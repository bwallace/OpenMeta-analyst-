# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cov_reg_dlg.ui'
#
# Created: Tue Oct 12 16:02:31 2010
#      by: PyQt4 UI code generator 4.7.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_cov_reg_dialog(object):
    def setupUi(self, cov_reg_dialog):
        cov_reg_dialog.setObjectName("cov_reg_dialog")
        cov_reg_dialog.resize(301, 132)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        cov_reg_dialog.setFont(font)
        self.buttonBox = QtGui.QDialogButtonBox(cov_reg_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 100, 281, 32))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.label = QtGui.QLabel(cov_reg_dialog)
        self.label.setGeometry(QtCore.QRect(20, 30, 121, 41))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        font.setPointSize(10)
        self.label.setFont(font)
        self.label.setObjectName("label")
        self.cov_cbo_box = QtGui.QComboBox(cov_reg_dialog)
        self.cov_cbo_box.setGeometry(QtCore.QRect(130, 40, 152, 20))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.cov_cbo_box.setFont(font)
        self.cov_cbo_box.setObjectName("cov_cbo_box")

        self.retranslateUi(cov_reg_dialog)
        QtCore.QMetaObject.connectSlotsByName(cov_reg_dialog)

    def retranslateUi(self, cov_reg_dialog):
        cov_reg_dialog.setWindowTitle(QtGui.QApplication.translate("cov_reg_dialog", "select covariate", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("cov_reg_dialog", "covariate for \n"
"regression:", None, QtGui.QApplication.UnicodeUTF8))

