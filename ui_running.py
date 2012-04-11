# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'running.ui'
#
# Created: Wed Apr 11 15:12:46 2012
#      by: PyQt4 UI code generator 4.8.5
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_running(object):
    def setupUi(self, running):
        running.setObjectName(_fromUtf8("running"))
        running.setWindowModality(QtCore.Qt.ApplicationModal)
        running.resize(468, 70)
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        running.setFont(font)
        running.setWindowTitle(QtGui.QApplication.translate("running", "running analysis...", None, QtGui.QApplication.UnicodeUTF8))
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
        pass

import icons_rc
