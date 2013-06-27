# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'choose_metric_page.ui'
#
# Created: Thu Jun 27 10:21:34 2013
#      by: PyQt4 UI code generator 4.10.1
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

class Ui_WizardPage(object):
    def setupUi(self, WizardPage):
        WizardPage.setObjectName(_fromUtf8("WizardPage"))
        WizardPage.resize(400, 220)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(WizardPage.sizePolicy().hasHeightForWidth())
        WizardPage.setSizePolicy(sizePolicy)
        WizardPage.setMinimumSize(QtCore.QSize(400, 220))
        WizardPage.setMaximumSize(QtCore.QSize(400, 220))
        WizardPage.setSubTitle(_fromUtf8(""))
        self.verticalLayout = QtGui.QVBoxLayout(WizardPage)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label_2 = QtGui.QLabel(WizardPage)
        self.label_2.setWordWrap(True)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.verticalLayout.addWidget(self.label_2)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.label = QtGui.QLabel(WizardPage)
        self.label.setObjectName(_fromUtf8("label"))
        self.horizontalLayout.addWidget(self.label)
        self.metric_cbo_box = QtGui.QComboBox(WizardPage)
        self.metric_cbo_box.setObjectName(_fromUtf8("metric_cbo_box"))
        self.horizontalLayout.addWidget(self.metric_cbo_box)
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.verticalLayout.addLayout(self.horizontalLayout)
        spacerItem1 = QtGui.QSpacerItem(20, 46, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem1)

        self.retranslateUi(WizardPage)
        QtCore.QMetaObject.connectSlotsByName(WizardPage)

    def retranslateUi(self, WizardPage):
        WizardPage.setWindowTitle(_translate("WizardPage", "WizardPage", None))
        WizardPage.setTitle(_translate("WizardPage", "Choose a metric...", None))
        self.label_2.setText(_translate("WizardPage", "Please choose the appropriate metric for the data type you just selected or just accept the listed default.\n"
"\n"
"Note, however, that if you are importing data via a CSV and you have effect data, it is ESSENTIAL that you choose the proper metric corresponding to your data now.", None))
        self.label.setText(_translate("WizardPage", "Metric:", None))

