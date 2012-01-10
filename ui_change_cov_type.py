# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'change_cov_type_form.ui'
#
# Created: Tue Jan 10 13:05:53 2012
#      by: PyQt4 UI code generator 4.8.5
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_ChangeCovTypeForm(object):
    def setupUi(self, ChangeCovTypeForm):
        ChangeCovTypeForm.setObjectName(_fromUtf8("ChangeCovTypeForm"))
        ChangeCovTypeForm.resize(664, 555)
        ChangeCovTypeForm.setMinimumSize(QtCore.QSize(550, 0))
        ChangeCovTypeForm.setMaximumSize(QtCore.QSize(100000, 555))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        ChangeCovTypeForm.setFont(font)
        ChangeCovTypeForm.setWindowTitle(QtGui.QApplication.translate("ChangeCovTypeForm", "Change Covariate Type", None, QtGui.QApplication.UnicodeUTF8))
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        ChangeCovTypeForm.setWindowIcon(icon)
        self.verticalLayout_2 = QtGui.QVBoxLayout(ChangeCovTypeForm)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.grp_box_preview = QtGui.QGroupBox(ChangeCovTypeForm)
        self.grp_box_preview.setTitle(QtGui.QApplication.translate("ChangeCovTypeForm", "covariate assignment", None, QtGui.QApplication.UnicodeUTF8))
        self.grp_box_preview.setObjectName(_fromUtf8("grp_box_preview"))
        self.verticalLayout = QtGui.QVBoxLayout(self.grp_box_preview)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.cov_prev_table = QtGui.QTableView(self.grp_box_preview)
        self.cov_prev_table.setObjectName(_fromUtf8("cov_prev_table"))
        self.verticalLayout.addWidget(self.cov_prev_table)
        self.verticalLayout_2.addWidget(self.grp_box_preview)
        self.buttonBox = QtGui.QDialogButtonBox(ChangeCovTypeForm)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout_2.addWidget(self.buttonBox)

        self.retranslateUi(ChangeCovTypeForm)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), ChangeCovTypeForm.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), ChangeCovTypeForm.reject)
        QtCore.QMetaObject.connectSlotsByName(ChangeCovTypeForm)

    def retranslateUi(self, ChangeCovTypeForm):
        pass

import icons_rc
