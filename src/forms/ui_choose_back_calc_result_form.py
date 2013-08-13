# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'choose_back_calc_result_form.ui'
#
# Created: Tue Aug 13 11:04:43 2013
#      by: PyQt4 UI code generator 4.10.2
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

class Ui_ChooseBackCalcResultForm(object):
    def setupUi(self, ChooseBackCalcResultForm):
        ChooseBackCalcResultForm.setObjectName(_fromUtf8("ChooseBackCalcResultForm"))
        ChooseBackCalcResultForm.resize(482, 221)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.MinimumExpanding, QtGui.QSizePolicy.MinimumExpanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(ChooseBackCalcResultForm.sizePolicy().hasHeightForWidth())
        ChooseBackCalcResultForm.setSizePolicy(sizePolicy)
        ChooseBackCalcResultForm.setMinimumSize(QtCore.QSize(480, 0))
        self.verticalLayout = QtGui.QVBoxLayout(ChooseBackCalcResultForm)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.info_label = QtGui.QLabel(ChooseBackCalcResultForm)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.info_label.sizePolicy().hasHeightForWidth())
        self.info_label.setSizePolicy(sizePolicy)
        self.info_label.setWordWrap(True)
        self.info_label.setObjectName(_fromUtf8("info_label"))
        self.verticalLayout.addWidget(self.info_label)
        self.line_2 = QtGui.QFrame(ChooseBackCalcResultForm)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName(_fromUtf8("line_2"))
        self.verticalLayout.addWidget(self.line_2)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.choice1_btn = QtGui.QRadioButton(ChooseBackCalcResultForm)
        self.choice1_btn.setChecked(True)
        self.choice1_btn.setObjectName(_fromUtf8("choice1_btn"))
        self.horizontalLayout.addWidget(self.choice1_btn)
        self.choice2_btn = QtGui.QRadioButton(ChooseBackCalcResultForm)
        self.choice2_btn.setObjectName(_fromUtf8("choice2_btn"))
        self.horizontalLayout.addWidget(self.choice2_btn)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.buttonBox = QtGui.QDialogButtonBox(ChooseBackCalcResultForm)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName(_fromUtf8("buttonBox"))
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(ChooseBackCalcResultForm)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("accepted()")), ChooseBackCalcResultForm.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL(_fromUtf8("rejected()")), ChooseBackCalcResultForm.reject)
        QtCore.QMetaObject.connectSlotsByName(ChooseBackCalcResultForm)

    def retranslateUi(self, ChooseBackCalcResultForm):
        ChooseBackCalcResultForm.setWindowTitle(_translate("ChooseBackCalcResultForm", "Choose Back Calculation Result", None))
        self.info_label.setText(_translate("ChooseBackCalcResultForm", "The back-calculation has resulted in two possible sets of choices for the counts.\n"
"\n"
"Please choose one from below:", None))
        self.choice1_btn.setText(_translate("ChooseBackCalcResultForm", "Choice 1", None))
        self.choice2_btn.setText(_translate("ChooseBackCalcResultForm", "Choice 2", None))

