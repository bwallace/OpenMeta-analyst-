# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_follow_up_dlg.ui'
#
# Created: Fri Apr 12 15:38:25 2013
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

class Ui_new_follow_up_dialog(object):
    def setupUi(self, new_follow_up_dialog):
        new_follow_up_dialog.setObjectName(_fromUtf8("new_follow_up_dialog"))
        new_follow_up_dialog.setEnabled(True)
        new_follow_up_dialog.resize(301, 132)
        new_follow_up_dialog.setMinimumSize(QtCore.QSize(301, 132))
        new_follow_up_dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        new_follow_up_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        new_follow_up_dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(new_follow_up_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.layoutWidget = QtGui.QWidget(new_follow_up_dialog)
        self.layoutWidget.setGeometry(QtCore.QRect(10, 10, 281, 71))
        self.layoutWidget.setObjectName(_fromUtf8("layoutWidget"))
        self.gridLayout = QtGui.QGridLayout(self.layoutWidget)
        self.gridLayout.setMargin(0)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label_2 = QtGui.QLabel(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        font.setPointSize(10)
        self.label_2.setFont(font)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 0, 0, 1, 1)
        self.follow_up_name_le = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.follow_up_name_le.setFont(font)
        self.follow_up_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.follow_up_name_le.setObjectName(_fromUtf8("follow_up_name_le"))
        self.gridLayout.addWidget(self.follow_up_name_le, 0, 1, 1, 1)

        self.retranslateUi(new_follow_up_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), new_follow_up_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), new_follow_up_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(new_follow_up_dialog)

    def retranslateUi(self, new_follow_up_dialog):
        new_follow_up_dialog.setWindowTitle(_translate("new_follow_up_dialog", "add new follow-up", None))
        self.label_2.setText(_translate("new_follow_up_dialog", "follow-up label", None))

import icons_rc
