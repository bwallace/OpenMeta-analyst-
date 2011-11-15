# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cov_reg_dlg2.ui'
#
# Created: Thu Nov 10 13:24:44 2011
#      by: PyQt4 UI code generator 4.8.5
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_cov_reg_dialog(object):
    def setupUi(self, cov_reg_dialog):
        cov_reg_dialog.setObjectName(_fromUtf8("cov_reg_dialog"))
        cov_reg_dialog.resize(398, 201)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        cov_reg_dialog.setFont(font)
        cov_reg_dialog.setWindowTitle(QtGui.QApplication.translate("cov_reg_dialog", "select covariates", None, QtGui.QApplication.UnicodeUTF8))
        self.verticalLayout = QtGui.QVBoxLayout(cov_reg_dialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label = QtGui.QLabel(cov_reg_dialog)
        self.label.setText(QtGui.QApplication.translate("cov_reg_dialog", "select covariates for regression:", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        spacerItem = QtGui.QSpacerItem(20, 10, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        self.verticalLayout.addItem(spacerItem)
        self.cov_grp_box = QtGui.QGroupBox(cov_reg_dialog)
        self.cov_grp_box.setTitle(QtGui.QApplication.translate("cov_reg_dialog", "available covariates", None, QtGui.QApplication.UnicodeUTF8))
        self.cov_grp_box.setObjectName(_fromUtf8("cov_grp_box"))
        self.verticalLayout.addWidget(self.cov_grp_box)
        spacerItem1 = QtGui.QSpacerItem(20, 60, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem1)
        self.buttonBox = QtGui.QDialogButtonBox(cov_reg_dialog)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(cov_reg_dialog)
        QtCore.QMetaObject.connectSlotsByName(cov_reg_dialog)

    def retranslateUi(self, cov_reg_dialog):
        pass

