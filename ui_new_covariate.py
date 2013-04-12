# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'new_covariate_dlg.ui'
#
# Created: Fri Apr 12 14:30:03 2013
#      by: PyQt4 UI code generator 4.9.6
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_new_covariate_dialog(object):
    def setupUi(self, new_covariate_dialog):
        new_covariate_dialog.setObjectName(_fromUtf8("new_covariate_dialog"))
        new_covariate_dialog.setEnabled(True)
        new_covariate_dialog.resize(301, 132)
        new_covariate_dialog.setMinimumSize(QtCore.QSize(301, 132))
        new_covariate_dialog.setMaximumSize(QtCore.QSize(301, 132))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        new_covariate_dialog.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        new_covariate_dialog.setWindowIcon(icon)
        self.buttonBox = QtGui.QDialogButtonBox(new_covariate_dialog)
        self.buttonBox.setGeometry(QtCore.QRect(10, 90, 281, 32))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.buttonBox.setFont(font)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.layoutWidget = QtGui.QWidget(new_covariate_dialog)
        self.layoutWidget.setGeometry(QtCore.QRect(10, 10, 281, 71))
        self.layoutWidget.setObjectName(_fromUtf8("layoutWidget"))
        self.gridLayout = QtGui.QGridLayout(self.layoutWidget)
        self.gridLayout.setMargin(0)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.label_2 = QtGui.QLabel(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        font.setPointSize(10)
        self.label_2.setFont(font)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 0, 0, 1, 1)
        self.covariate_name_le = QtGui.QLineEdit(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.covariate_name_le.setFont(font)
        self.covariate_name_le.setAlignment(QtCore.Qt.AlignCenter)
        self.covariate_name_le.setObjectName(_fromUtf8("covariate_name_le"))
        self.gridLayout.addWidget(self.covariate_name_le, 0, 1, 1, 1)
        self.label = QtGui.QLabel(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        font.setPointSize(10)
        self.label.setFont(font)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 1, 0, 1, 1)
        self.datatype_cbo_box = QtGui.QComboBox(self.layoutWidget)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        self.datatype_cbo_box.setFont(font)
        self.datatype_cbo_box.setObjectName(_fromUtf8("datatype_cbo_box"))
        self.gridLayout.addWidget(self.datatype_cbo_box, 1, 1, 1, 1)

        self.retranslateUi(new_covariate_dialog)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), new_covariate_dialog.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), new_covariate_dialog.reject)
        QtCore.QMetaObject.connectSlotsByName(new_covariate_dialog)

    def retranslateUi(self, new_covariate_dialog):
        new_covariate_dialog.setWindowTitle(_translate("new_covariate_dialog", "add new covariate", None))
        self.label_2.setText(_translate("new_covariate_dialog", "covariate name:", None))
        self.label.setText(_translate("new_covariate_dialog", "type of covariate:", None))

import icons_rc
