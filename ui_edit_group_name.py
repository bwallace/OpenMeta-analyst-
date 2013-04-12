# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'change_group_name_dlg.ui'
#
# Created: Fri Apr 12 14:30:02 2013
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

class Ui_group_name_dialog(object):
    def setupUi(self, group_name_dialog):
        group_name_dialog.setObjectName(_fromUtf8("group_name_dialog"))
        group_name_dialog.setEnabled(True)
        group_name_dialog.resize(301, 100)
        group_name_dialog.setMinimumSize(QtCore.QSize(301, 100))
        group_name_dialog.setMaximumSize(QtCore.QSize(500, 100))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        group_name_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        group_name_dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(group_name_dialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.gridLayout = QtGui.QGridLayout()
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.field_lbl = QtGui.QLabel(group_name_dialog)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        font.setPointSize(10)
        self.field_lbl.setFont(font)
        self.field_lbl.setObjectName(_fromUtf8("field_lbl"))
        self.gridLayout.addWidget(self.field_lbl, 0, 0, 1, 1)
        self.group_name_le = QtGui.QLineEdit(group_name_dialog)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.group_name_le.setFont(font)
        self.group_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.group_name_le.setObjectName(_fromUtf8("group_name_le"))
        self.gridLayout.addWidget(self.group_name_le, 0, 1, 1, 1)
        self.verticalLayout.addLayout(self.gridLayout)
        self.buttonBox = QtGui.QDialogButtonBox(group_name_dialog)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(group_name_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), group_name_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), group_name_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(group_name_dialog)

    def retranslateUi(self, group_name_dialog):
        group_name_dialog.setWindowTitle(_translate("group_name_dialog", "edit group name", None))
        self.field_lbl.setText(_translate("group_name_dialog", "group name:", None))

import icons_rc
