# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'start_up2.ui'
#
# Created: Wed Dec 26 15:34:56 2012
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_WelcomeDialog(object):
    def setupUi(self, WelcomeDialog):
        WelcomeDialog.setObjectName(_fromUtf8("WelcomeDialog"))
        WelcomeDialog.resize(674, 436)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        WelcomeDialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        WelcomeDialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(WelcomeDialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.logoGraphicsView = QtGui.QGraphicsView(WelcomeDialog)
        self.logoGraphicsView.setObjectName(_fromUtf8("logoGraphicsView"))
        self.verticalLayout.addWidget(self.logoGraphicsView)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.chk_show = QtGui.QCheckBox(WelcomeDialog)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.chk_show.setFont(font)
        self.chk_show.setChecked(True)
        self.chk_show.setObjectName(_fromUtf8("chk_show"))
        self.horizontalLayout_2.addWidget(self.chk_show)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.open_btn = QtGui.QPushButton(WelcomeDialog)
        self.open_btn.setMinimumSize(QtCore.QSize(100, 0))
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/function_icon_set/folder_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.open_btn.setIcon(icon1)
        self.open_btn.setObjectName(_fromUtf8("open_btn"))
        self.horizontalLayout.addWidget(self.open_btn)
        self.open_recent_btn = QtGui.QPushButton(WelcomeDialog)
        self.open_recent_btn.setMinimumSize(QtCore.QSize(120, 0))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.open_recent_btn.setFont(font)
        self.open_recent_btn.setIcon(icon1)
        self.open_recent_btn.setObjectName(_fromUtf8("open_recent_btn"))
        self.horizontalLayout.addWidget(self.open_recent_btn)
        self.create_new_btn = QtGui.QPushButton(WelcomeDialog)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/function_icon_set/add_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.create_new_btn.setIcon(icon2)
        self.create_new_btn.setObjectName(_fromUtf8("create_new_btn"))
        self.horizontalLayout.addWidget(self.create_new_btn)
        self.horizontalLayout_2.addLayout(self.horizontalLayout)
        self.verticalLayout.addLayout(self.horizontalLayout_2)

        self.retranslateUi(WelcomeDialog)
        QtCore.QMetaObject.connectSlotsByName(WelcomeDialog)
        WelcomeDialog.setTabOrder(self.create_new_btn, self.chk_show)
        WelcomeDialog.setTabOrder(self.chk_show, self.open_recent_btn)
        WelcomeDialog.setTabOrder(self.open_recent_btn, self.open_btn)

    def retranslateUi(self, WelcomeDialog):
        WelcomeDialog.setWindowTitle(QtGui.QApplication.translate("WelcomeDialog", "Welcome to OpenMeta", None, QtGui.QApplication.UnicodeUTF8))
        self.chk_show.setText(QtGui.QApplication.translate("WelcomeDialog", "show this window on start-up", None, QtGui.QApplication.UnicodeUTF8))
        self.open_btn.setText(QtGui.QApplication.translate("WelcomeDialog", "open ...", None, QtGui.QApplication.UnicodeUTF8))
        self.open_recent_btn.setText(QtGui.QApplication.translate("WelcomeDialog", "open recent", None, QtGui.QApplication.UnicodeUTF8))
        self.create_new_btn.setText(QtGui.QApplication.translate("WelcomeDialog", "create new", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
