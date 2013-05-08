'''
Created on Apr 29, 2013

@author: George Dietz
'''

from PyQt4.QtCore import *
from PyQt4.QtGui import *

import forms.ui_import_csv_dlg
import csv
import pdb

YEAR = 1
        
class ImportCsvDlg(QDialog, forms.ui_import_csv_dlg.Ui_ImportCsvDlg):
    def __init__(self, parent=None):
        super(ImportCsvDlg, self).__init__(parent)
        self.setupUi(self)
        
        ######################################################
        self.file_path = None
        self._reset_data()  # self.headers, self.covariate_names, self.covariate_types, self.imported_data, self.imported_data_ok
        ######################################################
    
        self.model = self.parent().tableView.model()
        
        self.required_header_labels = self._get_header_labels_from_parent()
        self.required_fmt_table.setRowCount(2)
        self.required_fmt_table.setColumnCount(len(self.required_header_labels))
        
        self.required_fmt_table.setHorizontalHeaderLabels(self.required_header_labels)
        self.required_fmt_table.resizeColumnsToContents()
        self.required_fmt_table.resizeRowsToContents()
        
        self.ok_button = self.buttonBox.button(QDialogButtonBox.Ok)
        self.ok_button.setEnabled(False)
        
        for row in range(self.required_fmt_table.rowCount()):
            for col in range(self.required_fmt_table.columnCount()):
                self.required_fmt_table.setItem(row,col,QTableWidgetItem(""))
                self.required_fmt_table.item(row, col).setFlags(Qt.NoItemFlags)
        ###############################################################
        self.connect(self.select_file_btn, SIGNAL("clicked()"), self._select_file)
        self.connect(self.from_excel_chkbx,  SIGNAL("stateChanged(int)"), self._rebuild_display)
        self.connect(self.has_headers_chkbx, SIGNAL("stateChanged(int)"), self._rebuild_display)
        
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
        
        if self.imported_data_ok:
            self.ok_button.setEnabled(True)
        else:
            self.ok_button.setEnabled(False)
    
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
        
    def _get_header_labels_from_parent(self):
        header_labels = []
        
        model_cols = [self.model.NAME, self.model.YEAR]
        model_cols.extend(self.model.RAW_DATA)
        model_cols.extend(self.model.OUTCOMES)
        
        for col in model_cols:
            col_name = self.model.headerData(col, Qt.Horizontal, role=Qt.DisplayRole)
            col_name = str(col_name.toString())
            header_labels.append(col_name)
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
    
        