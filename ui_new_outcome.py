# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_outcome_dlg.ui'
#
# Created: Wed Sep 02 11:55:00 2009
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.setEnabled(True)
        Dialog.resize(301, 132)
        Dialog.setMinimumSize(QtCore.QSize(301, 132))
        Dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        Dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(Dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.layoutWidget = QtGui.QWidget(Dialog)
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
        self.outcome_name_le = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.outcome_name_le.setFont(font)
        self.outcome_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.outcome_name_le.setObjectName("outcome_name_le")
        self.gridLayout.addWidget(self.outcome_name_le, 0, 1, 1, 1)
        self.label = QtGui.QLabel(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        font.setPointSize(10)
        self.label.setFont(font)
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 1, 0, 1, 1)
        self.datatype_cbo_box = QtGui.QComboBox(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.datatype_cbo_box.setFont(font)
        self.datatype_cbo_box.setObjectName("datatype_cbo_box")
        self.gridLayout.addWidget(self.datatype_cbo_box, 1, 1, 1, 1)

        self.retranslateUi(Dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), Dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), Dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        Dialog.setWindowTitle(QtGui.QApplication.translate("Dialog", "add new outcome", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("Dialog", "outcome name:", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Dialog", "type of outcome:", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
