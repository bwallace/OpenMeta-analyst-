# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'ma_specs.ui'
#
# Created: Tue Dec 08 11:02:48 2009
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.setWindowModality(QtCore.Qt.ApplicationModal)
        Dialog.resize(400, 421)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        Dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.verticalLayout = QtGui.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtGui.QLabel(Dialog)
        self.label.setMinimumSize(QtCore.QSize(100, 0))
        self.label.setMaximumSize(QtCore.QSize(150, 16777215))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.label.setFont(font)
        self.label.setAlignment(QtCore.Qt.AlignCenter)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        self.method_cbo_box = QtGui.QComboBox(Dialog)
        self.method_cbo_box.setObjectName("method_cbo_box")
        self.horizontalLayout.addWidget(self.method_cbo_box)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.parameter_grp_box = QtGui.QGroupBox(Dialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(40)
        sizePolicy.setHeightForWidth(self.parameter_grp_box.sizePolicy().hasHeightForWidth())
        self.parameter_grp_box.setSizePolicy(sizePolicy)
        self.parameter_grp_box.setMinimumSize(QtCore.QSize(0, 200))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.parameter_grp_box.setFont(font)
        self.parameter_grp_box.setObjectName("parameter_grp_box")
        self.verticalLayout.addWidget(self.parameter_grp_box)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "Method & Parameters", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Dialog", "analysis method:", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
