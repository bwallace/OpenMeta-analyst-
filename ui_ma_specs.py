# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'ma_specs.ui'
#
# Created: Thu Dec 03 13:24:43 2009
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(471, 469)
        self.verticalLayout = QtGui.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtGui.QLabel(Dialog)
        self.label.setMaximumSize(QtCore.QSize(90, 16777215))
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        self.comboBox = QtGui.QComboBox(Dialog)
        self.comboBox.setObjectName("comboBox")
        self.horizontalLayout.addWidget(self.comboBox)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.parameter_grp_box = QtGui.QGroupBox(Dialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(40)
        sizePolicy.setHeightForWidth(self.parameter_grp_box.sizePolicy().hasHeightForWidth())
        self.parameter_grp_box.setSizePolicy(sizePolicy)
        self.parameter_grp_box.setMinimumSize(QtCore.QSize(0, 400))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.parameter_grp_box.setFont(font)
        self.parameter_grp_box.setObjectName("parameter_grp_box")
        self.verticalLayout.addWidget(self.parameter_grp_box)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "Dialog", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Dialog", "analysis method:", None, QtGui.QApplication.UnicodeUTF8))
        self.parameter_grp_box.setTitle(QtGui.QApplication.translate("Dialog", "(method name)", None, QtGui.QApplication.UnicodeUTF8))

