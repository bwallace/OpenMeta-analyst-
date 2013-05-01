'''
Created on Apr 29, 2013

@author: George Dietz
'''

from PyQt4.QtCore import *
from PyQt4.QtGui import *

import ui_import_csv_dlg
import csv
import pdb

YEAR = 1
        
class ImportCsvDlg(QDialog, ui_import_csv_dlg.Ui_ImportCsvDlg):
    def __init__(self, parent=None):
        super(ImportCsvDlg, self).__init__(parent)
        self.setupUi(self)
        
        ######################################################
        self.file_path = None
        self.headers = []
        self.imported_data = []
        self.imported_data_ok = True
        ######################################################
    
        self.model = self.parent().tableView.model()
        
        required_header_labels = self._get_header_labels_from_parent()
        
        self.required_fmt_table.setRowCount(2)
        self.required_fmt_table.setColumnCount(len(required_header_labels))
        
        
        self.required_fmt_table.setHorizontalHeaderLabels(required_header_labels)
        self.required_fmt_table.resizeColumnsToContents()
        self.required_fmt_table.resizeRowsToContents()
        
        for row in range(self.required_fmt_table.rowCount()):
            for col in range(self.required_fmt_table.columnCount()):
                self.required_fmt_table.setItem(row,col,QTableWidgetItem(""))
                self.required_fmt_table.item(row, col).setFlags(Qt.NoItemFlags)
        ###############################################################
        self.connect(self.select_file_btn, SIGNAL("clicked()"), self._select_file)
        
    def _select_file(self):
        self.file_path = unicode(QFileDialog.getOpenFileName(self, "OpenMeta[analyst] - Import CSV", ".", "csv files (*.csv)"))
        if self.file_path:
            self.file_path_lbl.setText(QString(self.file_path))
        
        try:
            self.extract_data()
        except:
            QMessageBox.warning(self, "Whoops", "Something went wrong while trying to import csv, try again")
            self.preview_table.clear()
            self.headers = []
            self.imported_data = []
            return False
        
        if len(self.imported_data) == 0:
            QMessageBox.warning(self, "Whoops", "No data in CSV!, try again")
            self.preview_table.clear()
            self.headers = []
            self.imported_data = []
            return False
        
        # set up table
        self.preview_table.setRowCount(len(self.imported_data))
        self.preview_table.setColumnCount(len(self.imported_data[0]))
        if self.headers != []:
            self.preview_table.setHorizontalHeaderLabels(self.headers)

        # copy extracted data to table
        for row in range(self.preview_table.rowCount()):
            for col in range(self.preview_table.columnCount()):
                item = QTableWidgetItem(QString(self.imported_data[row][col]))
                item.setFlags(Qt.NoItemFlags)
                self.preview_table.setItem(row,col,item)
        self.preview_table.resizeColumnsToContents()
        self.preview_table.resizeRowsToContents()
        
        # Validate table entries
        self._validate_imported_data()
    
    def _validate_imported_data(self):
        # Make sure there are at least as many columns as required columns
        # (additional columns are covariates hopefully) 
#        if self.preview_table.columnCount() < self.required_fmt_table.columnCount():
#            self.imported_data_ok = False
#            QMessageBox.warning(self, "Whoops", "There are two few columns in the imported csv, try again with a properly formatted CSV.")
#            self._reset_values()
#            return False
        
        # Are the years integers?
        for row in range(len(self.imported_data)):
            try:
                int(self.imported_data[row][YEAR])
            except ValueError:
                self.imported_data_ok = False
                QMessageBox.warning(self, "Whoops", "The year at row " + str(row+1) + " is not an integer number.")
                self.reset_values()
                return False
        # More validation??
    
    def _reset_values(self):
        self.preview_table.clear()
        self.headers = []
        self.imported_data = []
        
        
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
            return {'headers':self.headers, 'data':self.imported_data}
        else:
            print("Somethign went wrong while trying to import from csv")
            return None
    
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
    
        