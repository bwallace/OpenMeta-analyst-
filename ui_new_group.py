# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_group_dlg.ui'
#
# Created: Wed Feb 10 10:47:06 2010
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_new_group_dialog(object):
    def setupUi(self, new_group_dialog):
        new_group_dialog.setObjectName("new_group_dialog")
        new_group_dialog.setEnabled(True)
        new_group_dialog.resize(301, 132)
        new_group_dialog.setMinimumSize(QtCore.QSize(301, 132))
        new_group_dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        new_group_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        new_group_dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(new_group_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.layoutWidget = QtGui.QWidget(new_group_dialog)
        self.layoutWidget.setGeometry(QtCore.QRect(10, 10, 281, 71))
        self.layoutWidget.setObjectName("layoutWidget")
        self.gridLayout = QtGui.QGridLayout(self.layoutWidget)
        self.gridLayout.setObjectName("gridLayout")
        self.label_2 = QtGui.QLabel(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        font.setPointSize(10)
        self.label_2.setFont(font)
        self.label_2.setObjectName("label_2")
        self.gridLayout.addWidget(self.label_2, 0, 0, 1, 1)
        self.group_name_le = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.group_name_le.setFont(font)
        self.group_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.group_name_le.setObjectName("group_name_le")
        self.gridLayout.addWidget(self.group_name_le, 0, 1, 1, 1)

        self.retranslateUi(new_group_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), new_group_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), new_group_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(new_group_dialog)

    def retranslateUi(self, new_group_dialog):
        new_group_dialog.setWindowTitle(QtGui.QApplication.translate("new_group_dialog", "add new tx group (arm)", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("new_group_dialog", "tx group name:", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
