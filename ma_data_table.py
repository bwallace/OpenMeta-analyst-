#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  MetaAnalyst 2... We need a better name
##  
##
## Custom QTableWidget, implements copy/paste and undo/redo.
##
#############################################################################


from PyQt4 import QtCore, QtGui
from PyQt4.Qt import *
import pdb

class MADataTable(QtGui.QTableView):
    
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        #self.undoStack = QUndoStack(self)
        # set the first row id to 0
        #item = QTableWidgetItem()
        #item.setData(Qt.UserRole, QVariant(0))
        #self.setItem(0, 0, item)
        #self.model = QSqlTableModel(self)





    
        
    
    def keyPressEvent(self, event):
        # undo/redo
        if (event.modifiers() & QtCore.Qt.ControlModifier):
            if event.key() == QtCore.Qt.Key_Z:
                self.undoStack.undo()
            elif event.key() == QtCore.Qt.Key_Y:
                self.undoStack.redo()    
                    
        # copy/paste: these only happen if at least one cell is selected
        selected_ranges = self.selectedRanges()
        if len(selected_ranges) > 0:
            if (event.modifiers() & QtCore.Qt.ControlModifier):
                if event.key() == QtCore.Qt.Key_C:
                    # ctrl + c = copy
                    self.copy_contents_in_range(selected_ranges[0], to_clipboard=True)
                elif event.key() == QtCore.Qt.Key_V:
                    # ctrl + v = paste
                    self.paste_from_clipboard()
        
    def rowMoved(self, row, oldIndex, newIndex):
        print "zldfjkdfj"
        
    def sortByColumn(self, col):
        print "!!!!!!!!!!!!!\n\n"
        
    def dataChanged(self, top_left, lower_right):
        print "DATA CHANGED"
        #print dir(self)
        print self.model()
        self.model().submitAll()
        QSqlDatabase.database().commit()
        
    def cell_changed(self, row, col):
        print "cell changed. row: %s, col: %s" % (row, col)
        print self.rowCount()
        if row ==  self.rowCount()-1:
            self.cur_row_id += 1
            self._add_new_row()

    def paste_from_clipboard(self):
        ''' pastes the data in the clipboard starting at the currently selected cell.'''
        #
        # TODO unit tests for edge cases!
        #
        selected_ranges = self.selectedRanges()
        if len(selected_ranges) > 0:
            clipboard = QApplication.clipboard()
            new_content = self._str_to_matrix(clipboard.text())
            upper_left_coord = (selected_ranges[0].topRow(), selected_ranges[0].leftColumn())
            top, left = upper_left_coord
            cur_num_rows = self.rowCount()

            # the constructor takes: top, left, bottom, right (respectively)
            range_to_copy = QtGui.QTableWidgetSelectionRange(top, left, max(cur_num_rows, top + len(new_content))-1,
                                                                                                left + len(new_content[0])-1)
            

            old_content = self._str_to_matrix(self.copy_contents_in_range(range_to_copy, to_clipboard=False))
            paste_command =  CommandPaste(self, new_content, old_content,
                                                                    upper_left_coord, cur_num_rows, self.column_widths(), "paste %s" % new_content)
            
            self.undoStack.push(paste_command) 
        
        
        
    def copy_contents_in_range(self, cell_range, to_clipboard):
        ''' 
        copy the (textual) content of the cells in provided cell_range -- the copied contents will be
        cast to python Unicode strings and returned. If the to_clipboard flag is true, the contents will
        also be copied to the system clipboard
        '''
        print (cell_range.leftColumn(), cell_range.rightColumn()+1)
        text_matrix = []
        for row in range(cell_range.topRow(), cell_range.bottomRow()+1):
            current_row = []
            for col in range(cell_range.leftColumn(), cell_range.rightColumn()+1):
                if self.item(row, col) is not None:
                    current_row.append(self.item(row, col).text().__str__())    
                else:
                    current_row.append("")
            text_matrix.append(current_row)

        copied_str = self._matrix_to_str(text_matrix)
        
        if to_clipboard:     
            clipboard = QApplication.clipboard()
            clipboard.setText(copied_str)
            
        return copied_str


    def paste_contents_in_range(self, upper_left_coord, source_content):
        ''' 
        paste the content in source_content into the matrix starting at the upper_left_coord
        cell. new rows will be added as needed; existing data will be overwritten
        '''
        
        #
        # TODO there is a problem with this architecture -- you shouldn't use the row/column coordinates in undoing/redoing, unless you also
        # make any operation that changes these coordinates undoable as well (e.g., sorting!) ... but how to catch the sort event?
        #
        print "pasting:"
        for row in source_content:
           self._print_row(row)
            
        origin_row, origin_col = upper_left_coord
        
        # temporarily disable sorting to prevent automatic sorting of pasted data. 
        # (note: this is consistent with Excel's approach.)
        self.setSortingEnabled(False)
        for src_row in range(len(source_content)-1):
            # do we need to append a row?
            cur_row_count = self.rowCount()
            if  cur_row_count <= origin_row + src_row:
                self._add_new_row()
            for src_col in range(len(source_content[0])):
                try:
                    new_item = QTableWidgetItem(source_content[src_row][src_col])
                    self.setItem(origin_row + src_row, origin_col + src_col, new_item)
                    self.resizeColumnToContents(origin_col + src_col)
                except:
                    pass
        self.setSortingEnabled(True)
        
        
    def column_widths(self):
        ''' returns the current column widths '''
        return [self.columnWidth(col_index) for col_index in range(self.columnCount())]
        
        
    def set_column_widths(self, widths):
        for col_index, width in enumerate(widths):
            self.setColumnWidth(col_index, width)
            
        
    def _add_new_row(self):
        ''' 
        add a new row to the dataTable; note that we briefly toggle sorting off so the row
        is beneath the existing rows. 
        '''
        cur_row_count = self.rowCount()
        self.setRowCount(cur_row_count+1)
        item = QTableWidgetItem()
        item.setData(Qt.UserRole, QVariant(self.cur_row_id))
        self.items.append(item)
        print "setting item %s" % (cur_row_count - 1)
        self.setItem(cur_row_count - 1, 0, item)
        #print self.item(cur_row_count -1, 0).toInt()
        self.cur_row_id += 1
        
    def _str_to_matrix(self, text, col_delimiter="\t", row_delimiter="\n"):
        ''' transforms raw text (e.g., from the clipboard) to a structured matrix '''
        m = [] 
        rows  = text.split(row_delimiter)
        for row in rows: 
            cur_row = row.split(col_delimiter)
            m.append(cur_row)
        return m
    
    def _print_row(self, r):
        print "length of row: %s" % len(r)
        for x in r:
            print x == ""
            print "%s," % x
        print "\n"
        
    def _is_blank_row(self, r):
        return len(r) == 1 and r[0] == ""

    def _matrix_to_str(self, m, col_delimiter="\t", row_delimiter="\n", append_new_line = True):
        ''' takes a matrix of data (i.e., a nested list) and converts to a string representation '''
        m_str = []
        for row in m:
            m_str.append(col_delimiter.join(row))
        return_str = row_delimiter.join(m_str)
        if append_new_line:
            return_str += row_delimiter
        return return_str


class CommandPaste(QUndoCommand):
    '''
    here we make use of QT's undo/redo framework. this implementation handles the paste action;
    the redo is just repasting the former contents into the same cells
    '''
    def __init__(self, ma_datatable, new_content, old_content, 
                                        upper_left_coord, nrows, old_col_widths, description):
        super(CommandPaste, self).__init__(description)
        self.ma_datatable = ma_datatable
        self.new_content = new_content
        self.old_content = old_content
        self.upper_left_coord = upper_left_coord
        self.old_column_widths = old_col_widths
        self.nrows = nrows

    def redo(self):
        self.ma_datatable.paste_contents_in_range(self.upper_left_coord, self.new_content)
        
    def undo(self):
        print "pasting this from undo:"
        print self.old_content
        self.ma_datatable.paste_contents_in_range(self.upper_left_coord, self.old_content)
        print "setting column widths to %s, they are currently %s" % (self.old_column_widths, self.ma_datatable.column_widths())
        self.ma_datatable.set_column_widths(self.old_column_widths)
        self.ma_datatable.setRowCount(self.nrows)
