# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'running.ui'
#
# Created: Wed Apr 17 14:37:20 2013
#      by: PyQt4 UI code generator 4.10
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

class Ui_running(object):
    def setupUi(self, running):
        running.setObjectName(_fromUtf8("running"))
        running.setWindowModality(QtCore.Qt.ApplicationModal)
        running.resize(373, 70)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        running.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        running.setWindowIcon(icon)
        running.setSizeGripEnabled(False)
        running.setModal(True)
        self.verticalLayout = QtGui.QVBoxLayout(running)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.progress_bar = QtGui.QProgressBar(running)
        self.progress_bar.setMaximum(0)
        self.progress_bar.setProperty("value", -1)
        self.progress_bar.setAlignment(QtCore.Qt.AlignCenter)
        self.progress_bar.setObjectName(_fromUtf8("progress_bar"))
        self.verticalLayout.addWidget(self.progress_bar)

        self.retranslateUi(running)
        QtCore.QMetaObject.connectSlotsByName(running)

    def retranslateUi(self, running):
        running.setWindowTitle(_translate("running", "running analysis...", None))

import icons_rc
