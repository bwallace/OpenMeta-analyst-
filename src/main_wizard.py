import forms.wizardpages.ui_choose_metric_page
import forms.wizardpages.ui_csv_import_page
import forms.wizardpages.ui_data_type_page
import forms.wizardpages.ui_outcome_name_page
import forms.wizardpages.ui_welcome_page

from PyQt4.Qt import *
from PyQt4.QtGui import *
from PyQt4 import QtCore, QtGui
import pdb
import meta_py_r
import meta_globals


class WelcomePage(QWizardPage, forms.wizardpages.ui_welcome_page.Ui_WizardPage):
    def __init__(self, parent=None, recent_datasets=None, start_up=True):
        super(WelcomePage, self).__init__(parent)
        self.setupUi(self)
        
        #print("Parent of page is %s" % str(self.parent))
        
        self.recent_datasets = recent_datasets or []
        self.recent_datasets.reverse() # most recently accessed dataset first
        #self.start_up = start_up #???
        
        self.selected_dataset = None
        ###self.registerField("selected_dataset", self.selected_dataset)

        self._setup_connections()
        
    def initializePage(self):
        self.wizard().adjustSize()

    def isComplete(self): # disable next/back buttons
        return False
    
    def nextId(self):
        #print("wizard path is: %s" % str(self.wizard().get_wizard_path()))
        if self.wizard().get_wizard_path() == "open":
            return -1
        else:
            return Page_DataType   
             
    def _setup_connections(self):
        QObject.connect(self.create_new_btn, SIGNAL("clicked()"), self.new_dataset)
        QObject.connect(self.open_btn,       SIGNAL("clicked()"), self.open_dataset)
        self._setup_open_recent_btn()
        QObject.connect(self.import_csv_btn, SIGNAL("clicked()"), self.import_csv)

            
    def _setup_open_recent_btn(self):
        if len(self.recent_datasets) > 0:
            ### 
            # then add a drop-down to the 'open recent' 
            # button with the recent datasets.
            qm = QMenu()
            for dataset in self.recent_datasets:
                action_item = QAction(QString(dataset), qm)
                qm.addAction(action_item)
                # I wanted to handle this with lambdas, but the method would
                # inexplicably always be invoked with the last dataset as the
                # argument. Instead, I've opted to use the .sender method to
                # retrieve the action_item, i.e., dataset, selected (see
                # the dataset_selected routine).
                QObject.connect(action_item, SIGNAL("triggered()"), self.dataset_selected) 
            self.open_recent_btn.setMenu(qm)
        else:
            self.open_recent_btn.setEnabled(False)
    
    def dataset_selected(self):
        self.wizard().set_wizard_path("open")
        
        # we use the sender method to see which menu item was
        # triggered
        dataset_path = QObject.sender(self).text()
        self.selected_dataset = dataset_path
        self.wizard().accept()
        
    def open_dataset(self):
        self.wizard().set_wizard_path("open")
        
        self.selected_dataset = unicode(QFileDialog.getOpenFileName(self,
                                    "OpenMeta[analyst] - Open File", ".", 
                                    "open meta files (*.oma)"))
        if self.selected_dataset != '':
            self.wizard().accept()

    def import_csv(self):
        self.wizard().set_wizard_path("csv_import")
        self.wizard().next()
        
        # TODO: import csv Do more here.....???
        
    def new_dataset(self):
        self.wizard().set_wizard_path("new_outcome")
        self.wizard().next()
        
################################################################################

class ProjectInfo:
    # Just a little class to store some info related to the new dataset outcome
    def __init__(self, arms=None, data_type=None, sub_type=None, effect=None,
                 metric_choices=[], outcome_name=None):
        self.arms = arms
        self.data_type = data_type
        self.sub_type = sub_type
        self.effect = effect
        self.metric_choices = metric_choices
        self.outcome_name = outcome_name
        
class DataTypePage(QWizardPage, forms.wizardpages.ui_data_type_page.Ui_DataTypePage):
    def __init__(self, parent=None):
        super(DataTypePage, self).__init__(parent)
        self.setupUi(self)
        
        self.selected_datatype = None
        self.summary = ProjectInfo()
        
        QObject.connect(self.buttonGroup, SIGNAL("buttonClicked(QAbstractButton*)"), self._button_selected)
    
    def initializePage(self):
        self.wizard().adjustSize()
    
    def _button_selected(self, button):
        #print("button clicked %s" % str(button))
        
        if button == self.onearm_proportion_Button:
            self.summary.arms      = 'one'
            self.summary.data_type = 'Binary'
            self.summary.sub_type  = 'proportion'
            self.summary.effect    = "PR" # default effect
            self.summary.metric_choices = meta_globals.BINARY_ONE_ARM_METRICS
        elif button == self.onearm_mean_Button:
            self.summary.arms      = 'one'
            self.summary.data_type = 'Continuous'
            self.summary.sub_type  = 'mean'
            self.summary.effect    = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
            self.summary.metric_choices = meta_globals.CONTINUOUS_ONE_ARM_METRICS
        elif button == self.onearm_single_reg_coef_Button:
            self.summary.arms      = 'one'
            self.summary.data_type = 'Continuous'
            self.summary.sub_type  = 'reg_coef'
            self.summary.effect    = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
            self.summary.metric_choices = meta_globals.CONTINUOUS_ONE_ARM_METRICS
        elif button == self.onearm_generic_effect_size_Button:
            self.summary.arms      = 'one'
            self.summary.data_type = 'Continuous'
            self.summary.sub_type  = 'generic_effect' # TODO: Should disable_two-arm metrics for generic effect
            self.summary.effect    = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
            self.summary.metric_choices = meta_globals.CONTINUOUS_ONE_ARM_METRICS
        #twoarm
        elif button == self.twoarm_proportions_Button:
            self.summary.arms      = 'two'
            self.summary.data_type = 'Binary'
            self.summary.sub_type  = 'proportions'
            self.summary.effect    = "OR"
            self.summary.metric_choices = meta_globals.BINARY_TWO_ARM_METRICS
        elif button == self.twoarm_means_Button:
            self.summary.arms      = 'two'     
            self.summary.data_type = 'Continuous'
            self.summary.sub_type  = 'means'
            self.summary.effect    = "MD"
            self.summary.metric_choices = meta_globals.CONTINUOUS_TWO_ARM_METRICS
        elif button == self.twoarm_smds_Button:
            self.summary.arms      = 'two' 
            self.summary.data_type = 'Continuous'
            self.summary.sub_type  = 'smd'
            self.summary.effect    = "SMD"
            self.summary.metric_choices = meta_globals.CONTINUOUS_TWO_ARM_METRICS
        #diagnostic
        elif button == self.diagnostic_Button:
            self.summary.data_type = 'Diagnostic'
            
        # Put information from pressing the button into the wizard storage area
        self.wizard().set_dataset_info(self.summary)
        self.emit(SIGNAL("completeChanged()"))
        
    def isComplete(self):
        #print(self.buttonGroup.checkedButton())
        
        if self.buttonGroup.checkedButton():
            return True
        else:
            return False
        
    def nextId(self):
        if self.wizard().get_dataset_info() and self.wizard().get_dataset_info().data_type == 'Diagnostic':
            return Page_OutcomeName
        else: #normal case
            return Page_ChooseMetric
        
        
        
        
###############################################################################
class ChooseMetricPage(QtGui.QWizardPage, forms.wizardpages.ui_choose_metric_page.Ui_WizardPage):
    def __init__(self, parent=None):
        super(ChooseMetricPage, self).__init__(parent)
        self.setupUi(self)
        
        QObject.connect(self.metric_cbo_box, SIGNAL("currentIndexChanged(int)"), self._metric_choice_changed)
        
    def initializePage(self):
        data_type = self.wizard().get_dataset_info().data_type
        metric_choices = self.wizard().get_dataset_info().metric_choices
        default_effect = self.wizard().get_dataset_info().effect
        
        # Add metric choices to combo box
        self.metric_cbo_box.blockSignals(True)
        self.metric_cbo_box.clear()
        self.metric_cbo_box.blockSignals(False)
        if data_type != 'Diagnostic':
            self.metric_cbo_box.blockSignals(True)
            for metric in metric_choices:
                metric_pretty_name = meta_globals.ALL_METRIC_NAMES[metric]
                self.metric_cbo_box.addItem(QString(metric + ": " + metric_pretty_name), userData=QVariant(QString(metric)))
            index_of_default = self.metric_cbo_box.findData(QVariant(QString(default_effect)))
            self.metric_cbo_box.setCurrentIndex(index_of_default)
            
            default_item_text = self.metric_cbo_box.itemText(index_of_default)
            default_item_text += QString(" (DEFAULT)")
            self.metric_cbo_box.setItemText(index_of_default, default_item_text)
            # Resize the dialog
            self.metric_cbo_box.blockSignals(False)
            
        self.wizard().adjustSize()
        
    def _metric_choice_changed(self, newindex):
        self.wizard().set_effect(str(self.metric_cbo_box.itemData(newindex).toString()))
        
    def nextId(self):
        return Page_OutcomeName
###############################################################################     

import csv

YEAR = 1
class CsvImportPage(QWizardPage, forms.wizardpages.ui_csv_import_page.Ui_WizardPage):
    def __init__(self, parent=None):
        super(CsvImportPage, self).__init__(parent)
        self.setupUi(self)
        
        ######################################################
        self.file_path = None
        self._reset_data()  # self.headers, self.covariate_names, self.covariate_types, self.imported_data, self.imported_data_ok
        ######################################################
    
        self.connect(self.select_file_btn, SIGNAL("clicked()"), self._select_file)
        self.connect(self.from_excel_chkbx,  SIGNAL("stateChanged(int)"), self._rebuild_display)
        self.connect(self.has_headers_chkbx, SIGNAL("stateChanged(int)"), self._rebuild_display)
    
    def initializePage(self):
        self.required_header_labels = self._get_required_header_labels()
        self.required_fmt_table.setRowCount(2)
        self.required_fmt_table.setColumnCount(len(self.required_header_labels))

        self.required_fmt_table.setHorizontalHeaderLabels(self.required_header_labels)
        self.required_fmt_table.resizeColumnsToContents()
        self.required_fmt_table.resizeRowsToContents()
        
        # Set up preview format table
        for row in range(self.required_fmt_table.rowCount()):
            for col in range(self.required_fmt_table.columnCount()):
                self.required_fmt_table.setItem(row,col,QTableWidgetItem(""))
                self.required_fmt_table.item(row, col).setFlags(Qt.NoItemFlags)
    
    def isComplete(self):
        # We must have a file selected
        if not self.file_path:
            return False 
            
        if self.imported_data_ok:
            return True
        else:
            return False

    def _reset_data(self):
        self.preview_table.clear()
        self.headers = []
        self.covariate_names = []
        self.covariate_types = []
        self.imported_data   = []
        self.imported_data_ok = True
        
    def _select_file(self):
        self.file_path = unicode(QFileDialog.getOpenFileName(self, "OpenMeta[analyst] - Import CSV", ".", "csv files (*.csv)"))
        if self.file_path:
            self.file_path_lbl.setText(QString(self.file_path))
        
        self._rebuild_display()
        
    def _rebuild_display(self):
        self._reset_data()
        try:
            self.extract_data()
        except Exception as e:
            print(e)
            QMessageBox.warning(self, "Whoops", "Something went wrong while trying to import csv, try again")
            self.imported_data_ok = False
            return False
        
        num_rows = len(self.imported_data)
        num_cols = len(self.imported_data[0])
        self._handle_covariates_in_extracted_data(
                num_rows, num_cols, headers = self.headers, 
                expected_headers = self.required_header_labels)
    
        if len(self.imported_data) == 0:
            QMessageBox.warning(self, "Whoops", "No data in CSV!, try again")
            self.imported_data_ok = False
            return False
        
        # set up table
        self.preview_table.setRowCount(num_rows)
        self.preview_table.setColumnCount(num_cols)
        if self.headers != []:
            self.preview_table.setHorizontalHeaderLabels(self.headers)
        else:
            preview_header_labels = self.required_header_labels[:]
            preview_header_labels.extend(self.covariate_names)
            self.preview_table.setHorizontalHeaderLabels(preview_header_labels)
        
        # copy extracted data to table
        for row in range(num_rows):
            for col in range(num_cols):
                item = QTableWidgetItem(QString(self.imported_data[row][col]))
                item.setFlags(Qt.NoItemFlags)
                self.preview_table.setItem(row,col,item)
        self.preview_table.resizeColumnsToContents()
        self.preview_table.resizeRowsToContents()
        
        # Validate table entries
        self._validate_imported_data()
        self.emit(SIGNAL("completeChanged()"))
        
    def _validate_imported_data(self):
        # Make sure there are at least as many columns as required columns
        # (additional columns are covariates hopefully) 
#        if self.preview_table.columnCount() < self.required_fmt_table.columnCount():
#            QMessageBox.warning(self, "Whoops", "There are two few columns in the imported csv, try again with a properly formatted CSV.")
#            self._reset_data
#            return False
        
        # Are the years integers?
        for row in range(len(self.imported_data)):
            try:
                int(self.imported_data[row][YEAR])
            except ValueError:
                QMessageBox.warning(self, "Whoops", "The year at row " + str(row+1) + " is not an integer number.")
                self.imported_data_ok = False
                return False
        # More validation??
        
    def _get_required_header_labels(self):
        '''
        Provides column header labels based on chosen datatype and subtype
        ** Must be updated if header_data() is ma_data_table_model is changed
        '''
        # TODO: Make this follow what is in header_data w/o having to make a new model
        # maybe make a static function in ma_data_table_model that takes
        # data_type, subtype, and effect as arguments and returns header_data info...
        # header_data in ma_data_table_model would then call this function
        
        
        data_type = self.wizard().get_dataset_info().data_type
        data_subtype = self.wizard().get_dataset_info().sub_type
        effect = self.wizard().get_dataset_info().effect
        
        print("data type: %s\nsubtype: %s\neffect: %s" % (data_type, data_subtype, effect))
        
        header_labels = ["study name", "year"]
        raw_labels = []
        outcome_labels = []
        grp_names = meta_globals.DEFAULT_GROUP_NAMES
        bin_raw_suffixes = [" #evts", " #total"]
        cont_raw_suffixes = [" N", " mean", " SD"]
        
        regular_outcome_prefixes = [effect,"lower","upper"]
       
        
        
        if data_type == "Binary":
            raw_prefixes = [0,0,1,1]
            raw_suffixes = [0,1,0,1]
            raw_labels = [grp_names[i] + bin_raw_suffixes[j] for i,j in zip(raw_prefixes, raw_suffixes)]
            outcome_labels = regular_outcome_prefixes
        elif data_type == "Continuous":
            if data_subtype == "generic_effect":
                raw_labels = []
                outcome_labels = [effect, "se"]
            else:
                raw_prefixes = [0,0,0,1,1,1]
                raw_suffixes = [0,1,2,0,1,2]
                raw_labels = [grp_names[i]+cont_raw_suffixes[j] for i,j in zip(raw_prefixes, raw_suffixes)]
                outcome_labels = regular_outcome_prefixes
        else: #diagnostic
            raw_labels = ["TP","FN","FP","TN"]
            outcome_labels = ["sens.", "lower", "upper", "spec.", "lower", "upper"]
            
            
        header_labels.extend(raw_labels)
        header_labels.extend(outcome_labels)
        return header_labels
    
    def csv_data(self):
        ''' Imported data is a list of rows. A row is a list of
        cell contents (as strings) '''
        
        if self.imported_data_ok:
            return {'headers':self.headers,
                    'data':self.imported_data,
                    'expected_headers':self.required_header_labels,
                    'covariate_names': self.covariate_names,
                    'covariate_types': self.covariate_types,}
        else:
            print("Something went wrong while trying to import from csv")
            return None
    
    def _handle_covariates_in_extracted_data(self, num_rows, num_cols, headers=[], expected_headers=[]):
        if num_cols > len(expected_headers): # Do we have covariates?
            num_covariates = num_cols - len(expected_headers)
            print("There are %d covariates" % num_covariates)
        else:
            return None # no covariates to deal with
        
        def covariate_name(index, given_name):
            if str(given_name).strip() == "":
                return "Covariate "+str(index+1)
            else:
                return given_name
        
        if self._hasHeaders():
            covariate_names = headers[len(expected_headers):]
        else:
            covariate_names = [""]*num_covariates
        self.covariate_names = [covariate_name(i, name) for i,name in enumerate(covariate_names)]
        
        def covariate_type(data):
            for x in data:
                try:
                    float(x)
                except ValueError:
                    return "Factor" # these types are important to get right (look in covariate constructor)
            return "Continuous"     #

        index_offset = len(expected_headers)
        for cov_index in range(len(covariate_names)):
            cov_data = [self.imported_data[row][index_offset+cov_index] for row in range(num_rows)]
            self.covariate_types.append(covariate_type(cov_data))
    
    def extract_data(self):
        with open(self._get_filepath(), 'rU') as csvfile:
            args_csv_reader = {'delimiter': self._get_delimter(),
                               'quotechar': self._get_quotechar(),
                               }
            if self._isFromExcel():
                args_csv_reader = {}
                args_csv_reader['dialect']='excel'
            
            # set up reader object
            reader = csv.reader(csvfile, **args_csv_reader)
            
            self.headers = []
            self.imported_data = []
            if self._hasHeaders():
                self.headers = reader.next()
            for row in reader:
                self.imported_data.append(row)
        self.print_extracted_data() # just for debugging
        
    def print_extracted_data(self):
        print("Data extracted from csv:")
        print(self.headers)
        for row in self.imported_data:
            print(str(row))
            
    def accept(self):
        QDialog.accept(self)

    def _get_filepath(self):
        return str(self.file_path)
    def _isFromExcel(self):
        return self.from_excel_chkbx.isChecked()
    def _hasHeaders(self):
        return self.has_headers_chkbx.isChecked()
    def _get_delimter(self):
        return str(self.delimter_le.text())
    def _get_quotechar(self):
        return str(self.quotechar_le.text())
################################################################################
class OutcomeNamePage(QWizardPage, forms.wizardpages.ui_outcome_name_page.Ui_WizardPage):
    def __init__(self, parent=None):
        super(OutcomeNamePage, self).__init__(parent)
        self.setupUi(self)
        
        self.registerField("outcomeName*", self.outcome_name_LineEdit)
        
    def initializePage(self):
        self.wizard().adjustSize()
        
    def nextId(self):
        if self.wizard().get_wizard_path()=="csv_import":
            return Page_CsvImport
        else: #normal case
            return -1
################################################################################          
Page_Welcome, Page_DataType, Page_ChooseMetric, Page_OutcomeName, Page_CsvImport = range(5)
class MainWizard(QtGui.QWizard):
    def __init__(self, parent=None, mode=None):
        super(MainWizard, self).__init__(parent)
        
        self.mode = mode
        self.info_d = {}

        self.setPage(Page_Welcome, WelcomePage())
        self.setPage(Page_DataType, DataTypePage())
        self.setPage(Page_ChooseMetric, ChooseMetricPage())
        self.setPage(Page_OutcomeName, OutcomeNamePage())
        self.setPage(Page_CsvImport, CsvImportPage())
        
        if mode is None:
            self.setStartId(Page_Welcome)
            self.setWindowTitle("Open Meta-Analyst")
 

        #self.setPixmap(QtGui.QWizard.BannerPixmap,
        #        QtGui.QPixmap(':/misc/meta.png'))
        #self.setPixmap(QtGui.QWizard.BackgroundPixmap,
        #               QtGui.QPixmap(':/misc/meta.png'))

#    def nextId(self):
#        page_id = self.currentId()
#        
#        if page_id == Page_Welcome:
#            if self.get_wizard_path() == "open":
#                return -1
#            else:
#                return Page_DataType
#        elif page_id == Page_DataType:
#            return Page_ChooseMetric
#        elif page_id == Page_ChooseMetric:
#            return Page_OutcomeName
#        elif page_id == Page_OutcomeName:
#            if self.get_wizard_path() == "csv_import":
#                return Page_CsvImport
#            else:
#                return -1 #end
#        else: # Unrecognized pageID (shouldn't get here)
#            print("Unrecognized Page ID")
#            return -1
    
    def set_wizard_path(self, path):
        self.info_d['path']=path
    def get_wizard_path(self):
        if 'path' in self.info_d:
            return self.info_d['path']
        else:
            return None
    
    def set_dataset_info(self, outcome_info):
        self.info_d['outcome_info'] = outcome_info
    def get_dataset_info(self):
        if 'outcome_info' in self.info_d:
            return self.info_d['outcome_info']
        else:
            return None
    def set_effect(self, effect_name):
        self.info_d['outcome_info'].effect = effect_name
    def get_effect(self):
        return self.info_d['outcome_info'].effect
    
    def get_results(self):
        information = {}
        information['path']=self.info_d['path']
        information['outcome_name']=self.field("outcomeName")
        information['arms'] = self.info_d['outcome_info'].arms
        information['data_type'] = self.info_d['outcome_info'].data_type
        information['sub_type'] = self.info_d['outcome_info'].sub_type
        information['effect'] = self.info_d['outcome_info'].effect
        
if __name__ == '__main__':

    import sys

    app = QtGui.QApplication(sys.argv)
    wizard = MainWizard()
    wizard.show()
    sys.exit(app.exec_())