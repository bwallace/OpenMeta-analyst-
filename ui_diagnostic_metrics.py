# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'diagnostic_metrics.ui'
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

class Ui_diag_metric(object):
    def setupUi(self, diag_metric):
        diag_metric.setObjectName(_fromUtf8("diag_metric"))
        diag_metric.setWindowModality(QtCore.Qt.ApplicationModal)
        diag_metric.resize(348, 160)
        diag_metric.setMinimumSize(QtCore.QSize(250, 140))
        diag_metric.setMaximumSize(QtCore.QSize(10000, 10000))
        font = QtGui.QFont()
        font.setFamily(_fromUtf8("Verdana"))
        diag_metric.setFont(font)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/images/meta.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        diag_metric.setWindowIcon(icon)
        self.gridLayout_2 = QtGui.QGridLayout(diag_metric)
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        self.metrics_grp_box = QtGui.QGroupBox(diag_metric)
        self.metrics_grp_box.setObjectName(_fromUtf8("metrics_grp_box"))
        self.verticalLayout = QtGui.QVBoxLayout(self.metrics_grp_box)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.gridLayout = QtGui.QGridLayout()
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.chk_box_sens = QtGui.QCheckBox(self.metrics_grp_box)
        self.chk_box_sens.setChecked(True)
        self.chk_box_sens.setObjectName(_fromUtf8("chk_box_sens"))
        self.gridLayout.addWidget(self.chk_box_sens, 0, 0, 1, 1)
        self.chk_box_spec = QtGui.QCheckBox(self.metrics_grp_box)
        self.chk_box_spec.setChecked(True)
        self.chk_box_spec.setObjectName(_fromUtf8("chk_box_spec"))
        self.gridLayout.addWidget(self.chk_box_spec, 0, 1, 1, 1)
        self.chk_box_lr = QtGui.QCheckBox(self.metrics_grp_box)
        self.chk_box_lr.setChecked(True)
        self.chk_box_lr.setObjectName(_fromUtf8("chk_box_lr"))
        self.gridLayout.addWidget(self.chk_box_lr, 2, 0, 1, 1)
        self.chk_box_dor = QtGui.QCheckBox(self.metrics_grp_box)
        self.chk_box_dor.setChecked(True)
        self.chk_box_dor.setObjectName(_fromUtf8("chk_box_dor"))
        self.gridLayout.addWidget(self.chk_box_dor, 2, 1, 1, 1)
        spacerItem = QtGui.QSpacerItem(20, 40, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.gridLayout.addItem(spacerItem, 1, 0, 1, 1)
        self.verticalLayout.addLayout(self.gridLayout)
        self.gridLayout_2.addWidget(self.metrics_grp_box, 0, 0, 1, 1)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        spacerItem1 = QtGui.QSpacerItem(200, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem1)
        self.btn_ok = QtGui.QPushButton(diag_metric)
        self.btn_ok.setMaximumSize(QtCore.QSize(75, 23))
        self.btn_ok.setObjectName(_fromUtf8("btn_ok"))
        self.horizontalLayout.addWidget(self.btn_ok)
        self.gridLayout_2.addLayout(self.horizontalLayout, 1, 0, 1, 1)

        self.retranslateUi(diag_metric)
        QtCore.QMetaObject.connectSlotsByName(diag_metric)

    def retranslateUi(self, diag_metric):
        diag_metric.setWindowTitle(_translate("diag_metric", "Diagnostic Metrics", None))
        self.metrics_grp_box.setTitle(_translate("diag_metric", "select metrics for analysis", None))
        self.chk_box_sens.setText(_translate("diag_metric", "sensitivity", None))
        self.chk_box_spec.setText(_translate("diag_metric", "specificity", None))
        self.chk_box_lr.setText(_translate("diag_metric", "likelihood ratio", None))
        self.chk_box_dor.setText(_translate("diag_metric", "diagnostic odds ratio", None))
        self.btn_ok.setText(_translate("diag_metric", "next >", None))

import icons_rc
