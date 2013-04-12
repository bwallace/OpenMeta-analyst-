# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'cov_reg_dlg2.ui'
#
# Created: Fri Apr 12 14:30:03 2013
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

class Ui_cov_reg_dialog(object):
    def setupUi(self, cov_reg_dialog):
        cov_reg_dialog.setObjectName(_fromUtf8("cov_reg_dialog"))
        cov_reg_dialog.resize(401, 323)
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
        self.diagnostic_group_box = QtGui.QGroupBox(cov_reg_dialog)
        self.diagnostic_group_box.setMinimumSize(QtCore.QSize(0, 50))
        self.diagnostic_group_box.setObjectName(_fromUtf8("diagnostic_group_box"))
        self.verticalLayout_3 = QtGui.QVBoxLayout(self.diagnostic_group_box)
        self.verticalLayout_3.setObjectName(_fromUtf8("verticalLayout_3"))
        self.dor_radio = QtGui.QRadioButton(self.diagnostic_group_box)
        self.dor_radio.setChecked(True)
        self.dor_radio.setObjectName(_fromUtf8("dor_radio"))
        self.verticalLayout_3.addWidget(self.dor_radio)
        self.sensitivity_radio = QtGui.QRadioButton(self.diagnostic_group_box)
        self.sensitivity_radio.setChecked(False)
        self.sensitivity_radio.setObjectName(_fromUtf8("sensitivity_radio"))
        self.verticalLayout_3.addWidget(self.sensitivity_radio)
        self.fixed_radio = QtGui.QRadioButton(self.diagnostic_group_box)
        self.fixed_radio.setObjectName(_fromUtf8("fixed_radio"))
        self.verticalLayout_3.addWidget(self.fixed_radio)
        self.verticalLayout.addWidget(self.diagnostic_group_box)
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
        cov_reg_dialog.setWindowTitle(_translate("cov_reg_dialog", "select covariates", None))
        self.label.setText(_translate("cov_reg_dialog", "select covariates for regression:", None))
        self.cov_grp_box.setTitle(_translate("cov_reg_dialog", "available covariates", None))
        self.diagnostic_group_box.setTitle(_translate("cov_reg_dialog", "metric", None))
        self.dor_radio.setText(_translate("cov_reg_dialog", "diagnostic odds ratio", None))
        self.sensitivity_radio.setText(_translate("cov_reg_dialog", "sensitivity", None))
        self.fixed_radio.setText(_translate("cov_reg_dialog", "specificity", None))
        self.groupBox.setTitle(_translate("cov_reg_dialog", "model type", None))
        self.random_effects_radio.setText(_translate("cov_reg_dialog", "random effects", None))
        self.fixed_effects_radio.setText(_translate("cov_reg_dialog", "fixed effect", None))

import icons_rc
