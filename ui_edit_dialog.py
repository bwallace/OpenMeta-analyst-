# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'edit_dialog.ui'
#
# Created: Wed Feb 17 10:28:21 2010
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_edit_dialog(object):
    def setupUi(self, edit_dialog):
        edit_dialog.setObjectName("edit_dialog")
        edit_dialog.resize(626, 292)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        edit_dialog.setFont(font)
        self.groupBox_2 = QtGui.QGroupBox(edit_dialog)
        self.groupBox_2.setGeometry(QtCore.QRect(170, 10, 161, 271))
        self.groupBox_2.setObjectName("groupBox_2")
        self.nav_add_btn_2 = QtGui.QToolButton(self.groupBox_2)
        self.nav_add_btn_2.setGeometry(QtCore.QRect(50, 230, 51, 41))
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/function_icon_set/add_48.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.nav_add_btn_2.setIcon(icon)
        self.nav_add_btn_2.setIconSize(QtCore.QSize(64, 64))
        self.nav_add_btn_2.setAutoRaise(True)
        self.nav_add_btn_2.setObjectName("nav_add_btn_2")
        self.outcome_list = QtGui.QListView(self.groupBox_2)
        self.outcome_list.setGeometry(QtCore.QRect(0, 20, 161, 209))
        self.outcome_list.setAlternatingRowColors(True)
        self.outcome_list.setObjectName("outcome_list")
        self.groupBox = QtGui.QGroupBox(edit_dialog)
        self.groupBox.setGeometry(QtCore.QRect(10, 10, 151, 271))
        self.groupBox.setFlat(False)
        self.groupBox.setObjectName("groupBox")
        self.group_list = QtGui.QListView(self.groupBox)
        self.group_list.setGeometry(QtCore.QRect(0, 20, 151, 211))
        self.group_list.setAlternatingRowColors(True)
        self.group_list.setObjectName("group_list")
        self.nav_add_btn = QtGui.QToolButton(self.groupBox)
        self.nav_add_btn.setGeometry(QtCore.QRect(50, 230, 51, 41))
        self.nav_add_btn.setIcon(icon)
        self.nav_add_btn.setIconSize(QtCore.QSize(64, 64))
        self.nav_add_btn.setAutoRaise(True)
        self.nav_add_btn.setObjectName("nav_add_btn")

        self.retranslateUi(edit_dialog)
        QtCore.QMetaObject.connectSlotsByName(edit_dialog)

    def retranslateUi(self, edit_dialog):
        edit_dialog.setWindowTitle(QtGui.QApplication.translate("edit_dialog", "Edit Dataset", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_2.setTitle(QtGui.QApplication.translate("edit_dialog", "outcomes", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("edit_dialog", "tx groups", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
