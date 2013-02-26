'''
Created on Feb 25, 2013

@author: george
'''

from PyQt4.Qt import *
from PyQt4.QtGui import *
#from PyQt4.QtCore import *

import ui_create_new_projectdlg

class CreateNewProjectDlg(QDialog, ui_create_new_projectdlg.Ui_newprojectdialog):
    
    def __init__(self, parent=None):
        super(CreateNewProjectDlg, self).__init__(parent)
        self.setupUi(self)
        
        self._setup_connections()
        self.summary = {}
        
        pal = self.outcome_name_LineEdit.palette()
        self.orignal_background_color = pal.color(self.outcome_name_LineEdit.backgroundRole())
    
    
    def _setup_connections(self):
        QObject.connect(self.outcome_name_LineEdit, SIGNAL("textEdited(QString)"), self._set_outcome_name)

        QObject.connect(self.diagnostic_Button                , SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.onearm_generic_effect_size_Button, SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.onearm_mean_Button               , SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.onearm_proportion_Button         , SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.onearm_single_reg_coef_Button    , SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.twoarm_means_Button              , SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.twoarm_proportions_Button        , SIGNAL("toggled(bool)"), self._project_button_toggled)
        QObject.connect(self.twoarm_smds_Button               , SIGNAL("toggled(bool)"), self._project_button_toggled)
        
    def _set_outcome_name(self,outcome_name):
        if outcome_name != "":
            self.summary['name'] = outcome_name
            self.ok_Button.setEnabled(True)
        else:
            self.ok_Button.setEnabled(False)
    
    def _project_button_toggled(self,abool):
        checked_button = (self.diagnostic_Button.isChecked()                 or
                          self.onearm_generic_effect_size_Button.isChecked() or 
                          self.onearm_mean_Button.isChecked()                or
                          self.onearm_proportion_Button.isChecked()          or
                          self.onearm_single_reg_coef_Button.isChecked()     or
                          self.twoarm_means_Button.isChecked()               or
                          self.twoarm_proportions_Button.isChecked()         or
                          self.twoarm_smds_Button.isChecked())
       
        pal = self.outcome_name_LineEdit.palette()
        
        # TO RESET BG COLOR (FOR REFERENCE)
        #pal.setColor(self.outcome_name_LineEdit.backgroundRole(), self.orignal_background_color)
        #self.outcome_name_LineEdit.setPalette(pal)
        
        if checked_button:
            self.outcome_name_LineEdit.setEnabled(True)
            pal.setColor(self.outcome_name_LineEdit.backgroundRole(), Qt.green)
            self.outcome_name_LineEdit.setPalette(pal)
            self.outcome_name_LineEdit.setFocus()
        else:
            self.outcome_name_LineEdit.setEnabled(False)
        
        self.summary['arms'] = None
        self.summary['data_type'] = None
        if self.onearm_proportion_Button.isChecked():
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'Binary'
        if self.onearm_mean_Button.isChecked():    
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'Continuous'
        if self.onearm_single_reg_coef_Button.isChecked():
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'Continuous'
        if self.onearm_generic_effect_size_Button.isChecked():
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'Continuous'   
    
        if self.twoarm_proportions_Button.isChecked(): 
            self.summary['arms'] = 'two'
            self.summary['data_type'] = 'Binary'
        if self.twoarm_means_Button.isChecked():
            self.summary['arms'] = 'two'     
            self.summary['data_type'] = 'Continuous'     
        if self.twoarm_smds_Button.isChecked():
            self.summary['arms'] = 'two' 
            self.summary['data_type'] = 'Continuous'
            
        if self.diagnostic_Button.isChecked():
            self.summary['data_type'] = 'Diagnostic'
            
    def get_summary(self):
        # Summary results:
        #  name: Outcome name
        #  data_type: Binary, Continuous, or Diagnostic
        #  arms: one or two
        # 
        return self.summary

if __name__ == "__main__":
    import sys

    app = QApplication(sys.argv)
    form = CreateNewProjectDlg()
    form.show()
    app.exec_()