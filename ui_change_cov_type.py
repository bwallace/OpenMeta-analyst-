# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'change_cov_type_form.ui'
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

class Ui_ChangeCovTypeForm(object):
    def setupUi(self, ChangeCovTypeForm):
        ChangeCovTypeForm.setObjectName(_fromUtf8("ChangeCovTypeForm"))
        ChangeCovTypeForm.resize(484, 428)
        ChangeCovTypeForm.setMinimumSize(QtCore.QSize(400, 0))
        ChangeCovTypeForm.setMaximumSize(QtCore.QSize(100000, 555))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        ChangeCovTypeForm.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        ChangeCovTypeForm.setWindowIcon(icon)
        self.verticalLayout_2 = QtGui.QVBoxLayout(ChangeCovTypeForm)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        self.grp_box_preview = QtGui.QGroupBox(ChangeCovTypeForm)
        self.grp_box_preview.setObjectName(_fromUtf8("grp_box_preview"))
        self.verticalLayout = QtGui.QVBoxLayout(self.grp_box_preview)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.cov_prev_table = QtGui.QTableView(self.grp_box_preview)
        self.cov_prev_table.setMaximumSize(QtCore.QSize(16777215, 16777215))
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
        ChangeCovTypeForm.setWindowTitle(_translate("ChangeCovTypeForm", "Change Covariate Type", None))
        self.grp_box_preview.setTitle(_translate("ChangeCovTypeForm", "values for new covariate", None))

import icons_rc
