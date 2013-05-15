'''
Created on Feb 25, 2013

@author: george
'''

from PyQt4.Qt import *
from PyQt4.QtGui import *
#from PyQt4.QtCore import *

import forms.ui_create_new_projectdlg
import meta_globals
#from meta_globals import BINARY, CONTINUOUS

class CreateNewProjectDlg(QDialog, forms.ui_create_new_projectdlg.Ui_newprojectdialog):
    
    def __init__(self, parent=None):
        super(CreateNewProjectDlg, self).__init__(parent)
        self.setupUi(self)
        
        self._setup_connections()
        self.summary = {}
        
        pal = self.outcome_name_LineEdit.palette()
        self.orignal_background_color = pal.color(self.outcome_name_LineEdit.backgroundRole())
        self.metric_choice_frame.setVisible(False)
        self.adjustSize()
    
    
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
        
        QObject.connect(self.metric_cbo_box, SIGNAL("currentIndexChanged(int)"), self._metric_choice_changed)
        
    def _metric_choice_changed(self, newindex):
        self.summary['effect'] = str(self.metric_cbo_box.itemData(newindex).toString())
        
    def _set_outcome_name(self,outcome_name):
        if outcome_name != "":
            self.summary['name'] = outcome_name
            self.ok_Button.setEnabled(True)
            self.ok_Button.setDefault(True)
        else:
            self.ok_Button.setEnabled(False)
            self.ok_Button.setDefault(False)
    
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
        self.summary['sub_type'] = None
        metric_choices = []
        #one_arm
        if self.onearm_proportion_Button.isChecked():
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'binary'
            self.summary['sub_type'] = 'proportion'
            default_effect = "PR"
            metric_choices = meta_globals.BINARY_ONE_ARM_METRICS
        if self.onearm_mean_Button.isChecked():    
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'continuous'
            self.summary['sub_type'] = 'mean'
            default_effect = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
            metric_choices = meta_globals.CONTINUOUS_ONE_ARM_METRICS
        if self.onearm_single_reg_coef_Button.isChecked():
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'continuous'
            self.summary['sub_type'] = 'reg_coef'
            default_effect = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
            metric_choices = meta_globals.CONTINUOUS_ONE_ARM_METRICS
        if self.onearm_generic_effect_size_Button.isChecked():
            self.summary['arms'] = 'one'
            self.summary['data_type'] = 'continuous'
            self.summary['sub_type'] = 'generic_effect' # TODO: Should disable_two-arm metrics for generic effect
            default_effect = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
            metric_choices = meta_globals.CONTINUOUS_ONE_ARM_METRICS
        #twoarm
        if self.twoarm_proportions_Button.isChecked(): 
            self.summary['arms'] = 'two'
            self.summary['data_type'] = 'binary'
            self.summary['sub_type'] = 'proportions'
            default_effect = "OR"
            metric_choices = meta_globals.BINARY_TWO_ARM_METRICS
        if self.twoarm_means_Button.isChecked():
            self.summary['arms'] = 'two'     
            self.summary['data_type'] = 'continuous'
            self.summary['sub_type'] = 'means'
            default_effect = "MD"
            metric_choices = meta_globals.CONTINUOUS_TWO_ARM_METRICS
        if self.twoarm_smds_Button.isChecked():
            self.summary['arms']      = 'two' 
            self.summary['data_type'] = 'continuous'
            self.summary['sub_type']  = 'smd'
            default_effect = "SMD"
            metric_choices = meta_globals.CONTINUOUS_TWO_ARM_METRICS
        #diagnostic
        if self.diagnostic_Button.isChecked():
            self.summary['data_type'] = 'diagnostic'
        
        if 'default_effect' in locals():
            self.summary['effect'] = default_effect
        
        # Add metric choices to combo box
        self.metric_cbo_box.blockSignals(True)
        self.metric_cbo_box.clear()
        self.metric_cbo_box.blockSignals(False)
        if self.summary['data_type'] != 'diagnostic':
            self.metric_cbo_box.blockSignals(True)
            for metric in metric_choices:
                metric_pretty_name = meta_globals.ALL_METRIC_NAMES[metric]
                self.metric_cbo_box.addItem(QString(metric + ": " + metric_pretty_name), userData=QVariant(QString(metric)))
            index_of_default = self.metric_cbo_box.findData(QVariant(QString(default_effect)))
            self.metric_cbo_box.setCurrentIndex(index_of_default)
            
            default_item_text = self.metric_cbo_box.itemText(index_of_default)
            default_item_text += QString(" (DEFAULT)")
            self.metric_cbo_box.setItemText(index_of_default, default_item_text)
            # Make the combo box visible and resize the dialog
            self.metric_choice_frame.setVisible(True)
            self.metric_cbo_box.blockSignals(False)
        else:
            self.metric_choice_frame.setVisible(False)
        self.adjustSize()
            
    def get_summary(self):
        # Summary results:
        #  name: Outcome name
        #  data_type: binary, continuous, or diagnostic
        #  sub_type: more specific than just data_type
        #  arms: one or two
        # 
        return self.summary

if __name__ == "__main__":
    import sys

    app = QApplication(sys.argv)
    form = CreateNewProjectDlg()
    form.show()
    app.exec_()