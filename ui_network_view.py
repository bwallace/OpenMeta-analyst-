# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'network_view_window.ui'
#
# Created: Wed Feb 17 10:04:40 2010
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_network_view_dialog(object):
    def setupUi(self, network_view_dialog):
        network_view_dialog.setObjectName("network_view_dialog")
        network_view_dialog.resize(625, 555)
        network_view_dialog.setMaximumSize(QtCore.QSize(625, 555))
        font = QtGui.QFont()
        font.setFamily("Verdana")
        network_view_dialog.setFont(font)
        self.frame = QtGui.QFrame(network_view_dialog)
        self.frame.setGeometry(QtCore.QRect(0, 509, 621, 40))
        self.frame.setFrameShape(QtGui.QFrame.StyledPanel)
        self.frame.setFrameShadow(QtGui.QFrame.Raised)
        self.frame.setObjectName("frame")
        self.horizontalLayout = QtGui.QHBoxLayout(self.frame)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label_3 = QtGui.QLabel(self.frame)
        self.label_3.setMaximumSize(QtCore.QSize(50, 16777215))
        self.label_3.setObjectName("label_3")
        self.horizontalLayout.addWidget(self.label_3)
        self.outcome_cbo_box = QtGui.QComboBox(self.frame)
        self.outcome_cbo_box.setObjectName("outcome_cbo_box")
        self.horizontalLayout.addWidget(self.outcome_cbo_box)
        self.label_4 = QtGui.QLabel(self.frame)
        self.label_4.setMaximumSize(QtCore.QSize(60, 16777215))
        self.label_4.setObjectName("label_4")
        self.horizontalLayout.addWidget(self.label_4)
        self.follow_up_cbo_box = QtGui.QComboBox(self.frame)
        self.follow_up_cbo_box.setObjectName("follow_up_cbo_box")
        self.horizontalLayout.addWidget(self.follow_up_cbo_box)
        self.network_viewer = QtGui.QGraphicsView(network_view_dialog)
        self.network_viewer.setGeometry(QtCore.QRect(1, 1, 621, 501))
        self.network_viewer.setObjectName("network_viewer")

        self.retranslateUi(network_view_dialog)
        QtCore.QMetaObject.connectSlotsByName(network_view_dialog)

    def retranslateUi(self, network_view_dialog):
        network_view_dialog.setWindowTitle(QtGui.QApplication.translate("network_view_dialog", "Network", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("network_view_dialog", "outcome:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("network_view_dialog", "follow-up:", None, QtGui.QApplication.UnicodeUTF8))

