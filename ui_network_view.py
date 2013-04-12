# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'network_view_window.ui'
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

class Ui_network_view_dialog(object):
    def setupUi(self, network_view_dialog):
        network_view_dialog.setObjectName(_fromUtf8("network_view_dialog"))
        network_view_dialog.resize(625, 555)
        network_view_dialog.setMaximumSize(QtCore.QSize(625, 555))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        network_view_dialog.setFont(font)
        self.frame = QtGui.QFrame(network_view_dialog)
        self.frame.setGeometry(QtCore.QRect(0, 509, 621, 40))
        self.frame.setFrameShape(QtGui.QFrame.StyledPanel)
        self.frame.setFrameShadow(QtGui.QFrame.Raised)
        self.frame.setObjectName(_fromUtf8("frame"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.frame)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.label_3 = QtGui.QLabel(self.frame)
        self.label_3.setMaximumSize(QtCore.QSize(50, 16777215))
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.horizontalLayout.addWidget(self.label_3)
        self.outcome_cbo_box = QtGui.QComboBox(self.frame)
        self.outcome_cbo_box.setObjectName(_fromUtf8("outcome_cbo_box"))
        self.horizontalLayout.addWidget(self.outcome_cbo_box)
        self.label_4 = QtGui.QLabel(self.frame)
        self.label_4.setMaximumSize(QtCore.QSize(60, 16777215))
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.horizontalLayout.addWidget(self.label_4)
        self.follow_up_cbo_box = QtGui.QComboBox(self.frame)
        self.follow_up_cbo_box.setObjectName(_fromUtf8("follow_up_cbo_box"))
        self.horizontalLayout.addWidget(self.follow_up_cbo_box)
        self.network_viewer = QtGui.QGraphicsView(network_view_dialog)
        self.network_viewer.setGeometry(QtCore.QRect(1, 1, 621, 501))
        self.network_viewer.setObjectName(_fromUtf8("network_viewer"))

        self.retranslateUi(network_view_dialog)
        QtCore.QMetaObject.connectSlotsByName(network_view_dialog)

    def retranslateUi(self, network_view_dialog):
        network_view_dialog.setWindowTitle(_translate("network_view_dialog", "Network", None))
        self.label_3.setText(_translate("network_view_dialog", "outcome:", None))
        self.label_4.setText(_translate("network_view_dialog", "follow-up:", None))

