# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'welcome_page.ui'
#
# Created: Fri May 10 10:15:04 2013
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

class Ui_WizardPage(object):
    def setupUi(self, WizardPage):
        WizardPage.setObjectName(_fromUtf8("WizardPage"))
        WizardPage.resize(401, 243)
        self.verticalLayout_2 = QtGui.QVBoxLayout(WizardPage)
        self.verticalLayout_2.setObjectName(_fromUtf8("verticalLayout_2"))
        spacerItem = QtGui.QSpacerItem(20, 9, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout_2.addItem(spacerItem)
        self.horizontalLayout_6 = QtGui.QHBoxLayout()
        self.horizontalLayout_6.setObjectName(_fromUtf8("horizontalLayout_6"))
        spacerItem1 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem1)
        self.verticalLayout = QtGui.QVBoxLayout()
        self.verticalLayout.setSpacing(0)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setSpacing(0)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.create_new_btn = QtGui.QPushButton(WizardPage)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8(":/function_icon_set/function_icon_set/add_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.create_new_btn.setIcon(icon)
        self.create_new_btn.setObjectName(_fromUtf8("create_new_btn"))
        self.horizontalLayout.addWidget(self.create_new_btn)
        spacerItem2 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem2)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setSpacing(0)
        self.horizontalLayout_3.setObjectName(_fromUtf8("horizontalLayout_3"))
        self.import_csv_btn = QtGui.QPushButton(WizardPage)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(_fromUtf8(":/function_icon_set/function_icon_set/box_download_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.import_csv_btn.setIcon(icon1)
        self.import_csv_btn.setObjectName(_fromUtf8("import_csv_btn"))
        self.horizontalLayout_3.addWidget(self.import_csv_btn)
        spacerItem3 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem3)
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtGui.QHBoxLayout()
        self.horizontalLayout_4.setSpacing(0)
        self.horizontalLayout_4.setObjectName(_fromUtf8("horizontalLayout_4"))
        self.open_recent_btn = QtGui.QPushButton(WizardPage)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(_fromUtf8(":/function_icon_set/function_icon_set/folder_48.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.open_recent_btn.setIcon(icon2)
        self.open_recent_btn.setObjectName(_fromUtf8("open_recent_btn"))
        self.horizontalLayout_4.addWidget(self.open_recent_btn)
        spacerItem4 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem4)
        self.verticalLayout.addLayout(self.horizontalLayout_4)
        self.horizontalLayout_5 = QtGui.QHBoxLayout()
        self.horizontalLayout_5.setSpacing(0)
        self.horizontalLayout_5.setObjectName(_fromUtf8("horizontalLayout_5"))
        self.open_btn = QtGui.QPushButton(WizardPage)
        self.open_btn.setIcon(icon2)
        self.open_btn.setObjectName(_fromUtf8("open_btn"))
        self.horizontalLayout_5.addWidget(self.open_btn)
        spacerItem5 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem5)
        self.verticalLayout.addLayout(self.horizontalLayout_5)
        self.horizontalLayout_6.addLayout(self.verticalLayout)
        spacerItem6 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem6)
        self.verticalLayout_2.addLayout(self.horizontalLayout_6)
        spacerItem7 = QtGui.QSpacerItem(20, 9, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout_2.addItem(spacerItem7)
        self.line = QtGui.QFrame(WizardPage)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.verticalLayout_2.addWidget(self.line)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.oma_onlineLabel = QtGui.QLabel(WizardPage)
        self.oma_onlineLabel.setMinimumSize(QtCore.QSize(120, 0))
        self.oma_onlineLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.oma_onlineLabel.setOpenExternalLinks(True)
        self.oma_onlineLabel.setObjectName(_fromUtf8("oma_onlineLabel"))
        self.horizontalLayout_2.addWidget(self.oma_onlineLabel)
        self.line_3 = QtGui.QFrame(WizardPage)
        self.line_3.setFrameShape(QtGui.QFrame.VLine)
        self.line_3.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_3.setObjectName(_fromUtf8("line_3"))
        self.horizontalLayout_2.addWidget(self.line_3)
        self.how_to_citeLabel = QtGui.QLabel(WizardPage)
        self.how_to_citeLabel.setEnabled(False)
        self.how_to_citeLabel.setMinimumSize(QtCore.QSize(80, 0))
        self.how_to_citeLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.how_to_citeLabel.setOpenExternalLinks(True)
        self.how_to_citeLabel.setObjectName(_fromUtf8("how_to_citeLabel"))
        self.horizontalLayout_2.addWidget(self.how_to_citeLabel)
        self.line_4 = QtGui.QFrame(WizardPage)
        self.line_4.setFrameShape(QtGui.QFrame.VLine)
        self.line_4.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_4.setObjectName(_fromUtf8("line_4"))
        self.horizontalLayout_2.addWidget(self.line_4)
        self.helpLabel = QtGui.QLabel(WizardPage)
        self.helpLabel.setMinimumSize(QtCore.QSize(70, 0))
        self.helpLabel.setAlignment(QtCore.Qt.AlignCenter)
        self.helpLabel.setOpenExternalLinks(True)
        self.helpLabel.setObjectName(_fromUtf8("helpLabel"))
        self.horizontalLayout_2.addWidget(self.helpLabel)
        self.verticalLayout_2.addLayout(self.horizontalLayout_2)

        self.retranslateUi(WizardPage)
        QtCore.QMetaObject.connectSlotsByName(WizardPage)

    def retranslateUi(self, WizardPage):
        WizardPage.setWindowTitle(_translate("WizardPage", "WizardPage", None))
        WizardPage.setTitle(_translate("WizardPage", "What would you like to do?", None))
        self.create_new_btn.setText(_translate("WizardPage", "Create a new Project", None))
        self.import_csv_btn.setText(_translate("WizardPage", "Import CSV", None))
        self.open_recent_btn.setText(_translate("WizardPage", "open recent ...", None))
        self.open_btn.setText(_translate("WizardPage", "Open an existing project", None))
        self.oma_onlineLabel.setText(_translate("WizardPage", "<html><head/><body><p><a href=\"http://www.cebm.brown.edu/open_meta\"><span style=\" text-decoration: underline; color:#0000ff;\">OpenMeta Website</span></a></p></body></html>", None))
        self.how_to_citeLabel.setText(_translate("WizardPage", "<a href=\"www.google.com\">How to cite</a>", None))
        self.helpLabel.setText(_translate("WizardPage", "<a href=\"http://tuftscaes.org/open_meta/help/openMA_help.html\">Help</a>", None))

import icons_rc
