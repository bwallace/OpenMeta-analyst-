# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cov_reg_dlg2.ui'
#
# Created: Wed Feb 22 15:55:14 2012
#      by: PyQt4 UI code generator 4.7.7
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
        cov_reg_dialog.resize(401, 267)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        cov_reg_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        cov_reg_dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(cov_reg_dialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label = QtGui.QLabel(cov_reg_dialog)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        spacerItem = QtGui.QSpacerItem(20, 10, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        self.verticalLayout.addItem(spacerItem)
        self.cov_grp_box = QtGui.QGroupBox(cov_reg_dialog)
        self.cov_grp_box.setObjectName(_fromUtf8("cov_grp_box"))
        self.verticalLayout.addWidget(self.cov_grp_box)
        spacerItem1 = QtGui.QSpacerItem(20, 30, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem1)
        self.groupBox = QtGui.QGroupBox(cov_reg_dialog)
        self.groupBox.setMinimumSize(QtCore.QSize(0, 50))
        self.groupBox.setObjectName(_fromUtf8("groupBox"))
        self.verticalLayout_2 = QtGui.QVBoxLayout(self.groupBox)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.random_effects_radio = QtGui.QRadioButton(self.groupBox)
        self.random_effects_radio.setChecked(True)
        self.random_effects_radio.setObjectName(_fromUtf8("random_effects_radio"))
        self.verticalLayout_2.addWidget(self.random_effects_radio)
        self.fixed_effects_radio = QtGui.QRadioButton(self.groupBox)
        self.fixed_effects_radio.setObjectName(_fromUtf8("fixed_effects_radio"))
        self.verticalLayout_2.addWidget(self.fixed_effects_radio)
        self.verticalLayout.addWidget(self.groupBox)
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
        cov_reg_dialog.setWindowTitle(QtGui.QApplication.translate("cov_reg_dialog", "select covariates", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("cov_reg_dialog", "select covariates for regression:", None, QtGui.QApplication.UnicodeUTF8))
        self.cov_grp_box.setTitle(QtGui.QApplication.translate("cov_reg_dialog", "available covariates", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("cov_reg_dialog", "model type", None, QtGui.QApplication.UnicodeUTF8))
        self.random_effects_radio.setText(QtGui.QApplication.translate("cov_reg_dialog", "random effects", None, QtGui.QApplication.UnicodeUTF8))
        self.fixed_effects_radio.setText(QtGui.QApplication.translate("cov_reg_dialog", "fixed effect", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
