# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_follow_up_dlg.ui'
#
# Created: Wed Feb 17 10:51:17 2010
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_new_follow_up_dialog(object):
    def setupUi(self, new_follow_up_dialog):
        new_follow_up_dialog.setObjectName("new_follow_up_dialog")
        new_follow_up_dialog.setEnabled(True)
        new_follow_up_dialog.resize(301, 132)
        new_follow_up_dialog.setMinimumSize(QtCore.QSize(301, 132))
        new_follow_up_dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        new_follow_up_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        new_follow_up_dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(new_follow_up_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.layoutWidget = QtGui.QWidget(new_follow_up_dialog)
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
        self.follow_up_name_le = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.follow_up_name_le.setFont(font)
        self.follow_up_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.follow_up_name_le.setObjectName("follow_up_name_le")
        self.gridLayout.addWidget(self.follow_up_name_le, 0, 1, 1, 1)

        self.retranslateUi(new_follow_up_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), new_follow_up_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), new_follow_up_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(new_follow_up_dialog)

    def retranslateUi(self, new_follow_up_dialog):
        new_follow_up_dialog.setWindowTitle(QtGui.QApplication.translate("new_follow_up_dialog", "add new follow-up", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("new_follow_up_dialog", "follow-up label", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
