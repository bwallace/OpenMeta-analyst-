# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'start_upV2.ui'
#
# Created: Tue Mar  5 15:55:49 2013
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

class Ui_WelcomeDialog(object):
    def setupUi(self, WelcomeDialog):
        WelcomeDialog.setObjectName(_fromUtf8("WelcomeDialog"))
        WelcomeDialog.resize(591, 442)
        WelcomeDialog.setSizeGripEnabled(False)
        self.verticalLayout = QtGui.QVBoxLayout(WelcomeDialog)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setObjectName(_fromUtf8("horizontalLayout_3"))
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem)
        self.logoLabel = QtGui.QLabel(WelcomeDialog)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.logoLabel.sizePolicy().hasHeightForWidth())
        self.logoLabel.setSizePolicy(sizePolicy)
        self.logoLabel.setMinimumSize(QtCore.QSize(321, 241))
        self.logoLabel.setMaximumSize(QtCore.QSize(321, 241))
        self.logoLabel.setText(_fromUtf8(""))
        self.logoLabel.setPixmap(QtGui.QPixmap(_fromUtf8("images/meta.png")))
        self.logoLabel.setScaledContents(True)
        self.logoLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.logoLabel.setObjectName(_fromUtf8("logoLabel"))
        self.horizontalLayout_3.addWidget(self.logoLabel)
        spacerItem1 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem1)
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.fundedby_lbl = QtGui.QLabel(WelcomeDialog)
        self.fundedby_lbl.setAlignment(QtCore.Qt.AlignCenter)
        self.fundedby_lbl.setObjectName(_fromUtf8("fundedby_lbl"))
        self.verticalLayout.addWidget(self.fundedby_lbl)
        spacerItem2 = QtGui.QSpacerItem(20, 39, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem2)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.oma_onlineLabel = QtGui.QLabel(WelcomeDialog)
        self.oma_onlineLabel.setMinimumSize(QtCore.QSize(120, 0))
        self.oma_onlineLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.oma_onlineLabel.setOpenExternalLinks(True)
        self.oma_onlineLabel.setObjectName(_fromUtf8("oma_onlineLabel"))
        self.horizontalLayout_2.addWidget(self.oma_onlineLabel)
        self.line_3 = QtGui.QFrame(WelcomeDialog)
        self.line_3.setFrameShape(QtGui.QFrame.VLine)
        self.line_3.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_3.setObjectName(_fromUtf8("line_3"))
        self.horizontalLayout_2.addWidget(self.line_3)
        self.how_to_citeLabel = QtGui.QLabel(WelcomeDialog)
        self.how_to_citeLabel.setEnabled(False)
        self.how_to_citeLabel.setMinimumSize(QtCore.QSize(80, 0))
        self.how_to_citeLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.how_to_citeLabel.setOpenExternalLinks(True)
        self.how_to_citeLabel.setObjectName(_fromUtf8("how_to_citeLabel"))
        self.horizontalLayout_2.addWidget(self.how_to_citeLabel)
        self.line_4 = QtGui.QFrame(WelcomeDialog)
        self.line_4.setFrameShape(QtGui.QFrame.VLine)
        self.line_4.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_4.setObjectName(_fromUtf8("line_4"))
        self.horizontalLayout_2.addWidget(self.line_4)
        self.helpLabel = QtGui.QLabel(WelcomeDialog)
        self.helpLabel.setMinimumSize(QtCore.QSize(70, 0))
        self.helpLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.helpLabel.setOpenExternalLinks(True)
        self.helpLabel.setObjectName(_fromUtf8("helpLabel"))
        self.horizontalLayout_2.addWidget(self.helpLabel)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.line = QtGui.QFrame(WelcomeDialog)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.verticalLayout.addWidget(self.line)
        self.label = QtGui.QLabel(WelcomeDialog)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.open_btn = QtGui.QPushButton(WelcomeDialog)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8("images/function_icon_set/folder_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.open_btn.setIcon(icon)
        self.open_btn.setObjectName(_fromUtf8("open_btn"))
        self.horizontalLayout.addWidget(self.open_btn)
        self.open_recent_btn = QtGui.QPushButton(WelcomeDialog)
        self.open_recent_btn.setIcon(icon)
        self.open_recent_btn.setObjectName(_fromUtf8("open_recent_btn"))
        self.horizontalLayout.addWidget(self.open_recent_btn)
        self.create_new_btn = QtGui.QPushButton(WelcomeDialog)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8("images/function_icon_set/add_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.create_new_btn.setIcon(icon1)
        self.create_new_btn.setObjectName(_fromUtf8("create_new_btn"))
        self.horizontalLayout.addWidget(self.create_new_btn)
        self.verticalLayout.addLayout(self.horizontalLayout)

        self.retranslateUi(WelcomeDialog)
        QtCore.QMetaObject.connectSlotsByName(WelcomeDialog)

    def retranslateUi(self, WelcomeDialog):
        WelcomeDialog.setWindowTitle(_translate("WelcomeDialog", "Welcome to OpenMeta", None))
        self.fundedby_lbl.setText(_translate("WelcomeDialog", "Funded by the Agency for Healthcare Research and Quality (AHRQ)\n"
"Grant # R01 HS 018574\n"
"Contract No. HHSA 290 2007 10055 I; Task Order #2", None))
        self.oma_onlineLabel.setText(_translate("WelcomeDialog", "<html><head/><body><p><a href=\"http://www.cebm.brown.edu/open_meta\"><span style=\" text-decoration: underline; color:#0000ff;\">OpenMeta Website</span></a></p></body></html>", None))
        self.how_to_citeLabel.setText(_translate("WelcomeDialog", "<a href=\"www.google.com\">How to cite</a>", None))
        self.helpLabel.setText(_translate("WelcomeDialog", "<a href=\"http://tuftscaes.org/open_meta/help/openMA_help.html\">Help</a>", None))
        self.label.setText(_translate("WelcomeDialog", "I want to:", None))
        self.open_btn.setText(_translate("WelcomeDialog", "Open an existing project", None))
        self.open_recent_btn.setText(_translate("WelcomeDialog", "open recent ...", None))
        self.create_new_btn.setText(_translate("WelcomeDialog", "Create a new Project", None))

