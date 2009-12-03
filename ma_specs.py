#######################################
#                                                                                   #
#                           Byron C. Wallace                              #
#  Tufts Medical Center                                                 #
#  OpenMeta[analyst]                                                    #
#                                                                                   #
#  This is the code for the ui dialog that handles          #
#  the method selection and algorithm specifications  #
#                                                                                   #
#######################################


from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *

import ui_ma_specs

class MA_Specs(QDialog, ui_ma_specs.Ui_Dialog):
    
    def __init__(self, parent=None):
        super(MA_Specs, self).__init__(parent)
        self.setupUi(self)
    