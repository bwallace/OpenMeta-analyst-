# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_study_dlg.ui'
#
# Created: Tue Nov 02 09:38:43 2010
#      by: PyQt4 UI code generator 4.7.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_new_study_dialog(object):
    def setupUi(self, new_study_dialog):
        new_study_dialog.setObjectName("new_study_dialog")
        new_study_dialog.setEnabled(True)
        new_study_dialog.resize(301, 132)
        new_study_dialog.setMinimumSize(QtCore.QSize(301, 132))
        new_study_dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        new_study_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        new_study_dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(new_study_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.layoutWidget = QtGui.QWidget(new_study_dialog)
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
        self.study_lbl = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.study_lbl.setFont(font)
        self.study_lbl.setAlignment(QtCore.Qt.AlignCenter)
        self.study_lbl.setObjectName("study_lbl")
        self.gridLayout.addWidget(self.study_lbl, 0, 1, 1, 1)

        self.retranslateUi(new_study_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), new_study_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), new_study_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(new_study_dialog)

    def retranslateUi(self, new_study_dialog):
        new_study_dialog.setWindowTitle(QtGui.QApplication.translate("new_study_dialog", "add new study", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("new_study_dialog", "study ", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
