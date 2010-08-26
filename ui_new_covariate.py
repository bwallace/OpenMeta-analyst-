# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_covariate_dlg.ui'
#
# Created: Thu Aug 26 10:19:14 2010
#      by: PyQt4 UI code generator 4.7.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_new_covariate_dialog(object):
    def setupUi(self, new_covariate_dialog):
        new_covariate_dialog.setObjectName("new_covariate_dialog")
        new_covariate_dialog.setEnabled(True)
        new_covariate_dialog.resize(301, 132)
        new_covariate_dialog.setMinimumSize(QtCore.QSize(301, 132))
        new_covariate_dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        new_covariate_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/images/meta.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        new_covariate_dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(new_covariate_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.layoutWidget = QtGui.QWidget(new_covariate_dialog)
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
        self.covariate_name_le = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.covariate_name_le.setFont(font)
        self.covariate_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.covariate_name_le.setObjectName("covariate_name_le")
        self.gridLayout.addWidget(self.covariate_name_le, 0, 1, 1, 1)
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

        self.retranslateUi(new_covariate_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), new_covariate_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), new_covariate_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(new_covariate_dialog)

    def retranslateUi(self, new_covariate_dialog):
        new_covariate_dialog.setWindowTitle(QtGui.QApplication.translate("new_covariate_dialog", "add new covariate", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("new_covariate_dialog", "covariate name:", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("new_covariate_dialog", "type of covariate:", None, QtGui.QApplication.UnicodeUTF8))

import icons_rc
