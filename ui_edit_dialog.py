# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'edit_dialog.ui'
#
# Created: Tue Feb 09 15:34:03 2010
#      by: PyQt4 UI code generator 4.4.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_edit_dialog(object):
    def setupUi(self, edit_dialog):
        edit_dialog.setObjectName("edit_dialog")
        edit_dialog.resize(624, 306)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        edit_dialog.setFont(font)
        self.horizontalLayout = QtGui.QHBoxLayout(edit_dialog)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.editTabWidget = QtGui.QTabWidget(edit_dialog)
        font = QtGui.QFont()
        font.setFamily("Verdana")
        self.editTabWidget.setFont(font)
        self.editTabWidget.setObjectName("editTabWidget")
        self.arms_tab = QtGui.QWidget()
        self.arms_tab.setObjectName("arms_tab")
        self.widget = QtGui.QWidget(self.arms_tab)
        self.widget.setGeometry(QtCore.QRect(9, 9, 581, 241))
        self.widget.setObjectName("widget")
        self.gridLayout = QtGui.QGridLayout(self.widget)
        self.gridLayout.setObjectName("gridLayout")
        self.networkGraphicsView = QtGui.QGraphicsView(self.widget)
        self.networkGraphicsView.setMinimumSize(QtCore.QSize(100, 0))
        self.networkGraphicsView.setObjectName("networkGraphicsView")
        self.gridLayout.addWidget(self.networkGraphicsView, 0, 0, 2, 1)
        self.label = QtGui.QLabel(self.widget)
        self.label.setMaximumSize(QtCore.QSize(70, 16777215))
        self.label.setObjectName("label")
        self.gridLayout.addWidget(self.label, 0, 1, 1, 1)
        self.tx_group_cbox = QtGui.QComboBox(self.widget)
        self.tx_group_cbox.setMinimumSize(QtCore.QSize(190, 0))
        self.tx_group_cbox.setObjectName("tx_group_cbox")
        self.gridLayout.addWidget(self.tx_group_cbox, 0, 2, 1, 1)
        self.frame = QtGui.QFrame(self.widget)
        self.frame.setMinimumSize(QtCore.QSize(300, 0))
        self.frame.setFrameShape(QtGui.QFrame.Box)
        self.frame.setFrameShadow(QtGui.QFrame.Plain)
        self.frame.setObjectName("frame")
        self.arm_name_le = QtGui.QLineEdit(self.frame)
        self.arm_name_le.setGeometry(QtCore.QRect(94, 31, 146, 20))
        self.arm_name_le.setObjectName("arm_name_le")
        self.label_2 = QtGui.QLabel(self.frame)
        self.label_2.setGeometry(QtCore.QRect(51, 31, 37, 20))
        self.label_2.setObjectName("label_2")
        self.gridLayout.addWidget(self.frame, 1, 1, 1, 2)
        self.editTabWidget.addTab(self.arms_tab, "")
        self.outcomes_tab = QtGui.QWidget()
        self.outcomes_tab.setObjectName("outcomes_tab")
        self.editTabWidget.addTab(self.outcomes_tab, "")
        self.follow_up_tab = QtGui.QWidget()
        self.follow_up_tab.setObjectName("follow_up_tab")
        self.editTabWidget.addTab(self.follow_up_tab, "")
        self.cov_tab = QtGui.QWidget()
        self.cov_tab.setObjectName("cov_tab")
        self.editTabWidget.addTab(self.cov_tab, "")
        self.horizontalLayout.addWidget(self.editTabWidget)

        self.retranslateUi(edit_dialog)
        self.editTabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(edit_dialog)

    def retranslateUi(self, edit_dialog):
        edit_dialog.setWindowTitle(QtGui.QApplication.translate("edit_dialog", "Edit", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("edit_dialog", "tx group:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("edit_dialog", "name:", None, QtGui.QApplication.UnicodeUTF8))
        self.editTabWidget.setTabText(self.editTabWidget.indexOf(self.arms_tab), QtGui.QApplication.translate("edit_dialog", "tx groups (arms)", None, QtGui.QApplication.UnicodeUTF8))
        self.editTabWidget.setTabText(self.editTabWidget.indexOf(self.outcomes_tab), QtGui.QApplication.translate("edit_dialog", "outcomes", None, QtGui.QApplication.UnicodeUTF8))
        self.editTabWidget.setTabText(self.editTabWidget.indexOf(self.follow_up_tab), QtGui.QApplication.translate("edit_dialog", "follow-ups", None, QtGui.QApplication.UnicodeUTF8))
        self.editTabWidget.setTabText(self.editTabWidget.indexOf(self.cov_tab), QtGui.QApplication.translate("edit_dialog", "covariates", None, QtGui.QApplication.UnicodeUTF8))

