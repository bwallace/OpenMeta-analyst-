# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'start_up.ui'
#
# Created: Thu Feb 10 13:00:43 2011
#      by: PyQt4 UI code generator 4.8.3
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
        WelcomeDialog.resize(564, 197)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        WelcomeDialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        WelcomeDialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(WelcomeDialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.groupBox = QtGui.QGroupBox(WelcomeDialog)
        self.groupBox.setObjectName(_fromUtf8("groupBox"))
        self.gridLayout_2 = QtGui.QGridLayout(self.groupBox)
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName(_fromUtf8("horizontalLayout_3"))
        self.label = QtGui.QLabel(self.groupBox)
        self.label.setObjectName(_fromUtf8("label"))
        self.horizontalLayout_3.addWidget(self.label)
        self.dataset_name_le = QtGui.QLineEdit(self.groupBox)
        self.dataset_name_le.setObjectName(_fromUtf8("dataset_name_le"))
        self.horizontalLayout_3.addWidget(self.dataset_name_le)
        self.gridLayout_2.addLayout(self.horizontalLayout_3, 0, 0, 1, 1)
        self.gridLayout = QtGui.QGridLayout()
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label_2 = QtGui.QLabel(self.groupBox)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 0, 0, 1, 2)
        self.bin_radio = QtGui.QRadioButton(self.groupBox)
        self.bin_radio.setChecked(True)
        self.bin_radio.setObjectName(_fromUtf8("bin_radio"))
        self.buttonGroup = QtGui.QButtonGroup(WelcomeDialog)
        self.buttonGroup.setObjectName(_fromUtf8("buttonGroup"))
        self.buttonGroup.addButton(self.bin_radio)
        self.gridLayout.addWidget(self.bin_radio, 1, 0, 1, 1)
        self.diag_radio = QtGui.QRadioButton(self.groupBox)
        self.diag_radio.setObjectName(_fromUtf8("diag_radio"))
        self.buttonGroup.addButton(self.diag_radio)
        self.gridLayout.addWidget(self.diag_radio, 1, 1, 1, 1)
        self.gridLayout_2.addLayout(self.gridLayout, 2, 0, 1, 1)
        spacerItem = QtGui.QSpacerItem(20, 20, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Fixed)
        self.gridLayout_2.addItem(spacerItem, 1, 0, 1, 1)
        self.verticalLayout.addWidget(self.groupBox)
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

    def retranslateUi(self, WelcomeDialog):
        WelcomeDialog.setWindowTitle(QtGui.QApplication.translate("WelcomeDialog", "Welcome to OpenMeta", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("WelcomeDialog", "new dataset", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("WelcomeDialog", "name:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("WelcomeDialog", "what sort of data will you be working with?", None, QtGui.QApplication.UnicodeUTF8))
        self.bin_radio.setText(QtGui.QApplication.translate("WelcomeDialog", "continuous and/or binary", None, QtGui.QApplication.UnicodeUTF8))
        self.diag_radio.setText(QtGui.QApplication.translate("WelcomeDialog", "diagnostic", None, QtGui.QApplication.UnicodeUTF8))
        self.chk_show.setText(QtGui.QApplication.translate("WelcomeDialog", "show this window on start-up", None, QtGui.QApplication.UnicodeUTF8))
        self.open_btn.setText(QtGui.QApplication.translate("WelcomeDialog", "open ...", None, QtGui.QApplication.UnicodeUTF8))
        self.open_recent_btn.setText(QtGui.QApplication.translate("WelcomeDialog", "open recent", None, QtGui.QApplication.UnicodeUTF8))
        self.create_new_btn.setText(QtGui.QApplication.translate("WelcomeDialog", "create new", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
import icons_rc
