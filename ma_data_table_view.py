#####################################################
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta(analyst)
#
#
# Custom QTableView, implements copy/paste and undo/redo.
#
#####################################################


from PyQt4 import QtCore, QtGui
from PyQt4.Qt import *
import pdb

import binary_data_form

class MADataTable(QtGui.QTableView):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        # None maps to the special, no outcome/no follow up
        # undo stack
        self.undo_stack_dict = {None:QUndoStack(self)}
        # self.undoStack = self.undo_stack_dict[None]
        self.undoStack = QUndoStack(self)

        header = self.horizontalHeader()
        self.connect(header, SIGNAL("sectionClicked(int)"), self.header_clicked)

        vert_header = self.verticalHeader()
        self.connect(vert_header, SIGNAL("sectionClicked(int)"), self.row_header_clicked)

        self.reverse_column_sorts = {0: False, 1: False}
        self.setAlternatingRowColors(True)
        
        
    def keyPressEvent(self, event):
        # undo/redo
        if (event.modifiers() & QtCore.Qt.ControlModifier):
            if event.key() == QtCore.Qt.Key_Z:
                self.undoStack.undo()
            elif event.key() == QtCore.Qt.Key_Y:
                self.undoStack.redo()

        # copy/paste: these only happen if at least one cell is selected
        selected_indexes = self.selectionModel().selectedIndexes()
        upper_left_index, lower_right_index = (self._upper_left(selected_indexes),
                                               self._lower_right(selected_indexes))

        if (event.modifiers() & QtCore.Qt.ControlModifier):
            if event.key() == QtCore.Qt.Key_C:
                # ctrl + c = copy
                self.copy_contents_in_range(upper_left_index, lower_right_index,
                                                                to_clipboard=True)
            elif event.key() == QtCore.Qt.Key_V:
                # ctrl + v = paste
                self.paste_from_clipboard(upper_left_index)
        
        
    def row_header_clicked(self, row):
        #
        # TODO: we're assuming here that we're dealing with binary
        #               data, and only handling that case.
        #
        ma_unit = self.model().get_current_ma_unit_for_study(row)
        cur_txs = self.model().current_txs
        cur_effect = self.model().current_effect
        form =  binary_data_form.BinaryDataForm2(ma_unit, cur_txs, cur_effect, parent=self)
        
        if form.exec_():
            pass

    def rowMoved(self, row, oldIndex, newIndex):
        pass

    def displayed_ma_changed(self):
        cur_outcome = self.model().current_outcome
        cur_follow_up = self.model().current_time_point
        '''
        if not cur_outcome in self.undo_stack_dict:
            self.undo_stack_dict [cur_outcome] = {}

        if not cur_follow_up in self.undo_stack_dict[cur_outcome]:
            self.undo_stack_dict[cur_outcome][cur_follow_up] = QUndoStack(self)

        self.undoStack = self.undo_stack_dict[cur_outcome][cur_follow_up]
        '''

    def cell_content_changed(self, index, old_val, new_val):
        cell_edit = CommandCellEdit(self, index, old_val, new_val)
        self.undoStack.push(cell_edit)

    def header_clicked(self, column):
        can_sort_by = [self.model().NAME, self.model().YEAR]
        if column in can_sort_by:
            sort_command = CommandSort(self.model(), column, self.reverse_column_sorts[column])
            self.undoStack.push(sort_command)
            self.reverse_column_sorts[column] = not self.reverse_column_sorts[column]

    def paste_from_clipboard(self, upper_left_index):
        ''' pastes the data in the clipboard starting at the currently selected cell.'''

        clipboard = QApplication.clipboard()
        new_content = self._str_to_matrix(clipboard.text())

        lower_row = upper_left_index.row() + len(new_content)
        lower_col = upper_left_index.column() + len(new_content[0])
        print "lower row: %s, lower col: %s" % (lower_row, lower_col)
        num_studies_pre_paste = len(self.model().dataset)
        studies_pre_paste = list(self.model().dataset.studies)
        lower_right_index = self.model().createIndex(lower_row-1, lower_col-1)
        old_content = self._str_to_matrix(self.copy_contents_in_range(upper_left_index, lower_right_index, to_clipboard=False))

        print "old content: %s" % old_content
        print "new content: %s" % new_content
        print "upper left index:"
        print self._print_index(upper_left_index)
        paste_command =  CommandPaste(self, new_content, old_content,
                                                                upper_left_index, studies_pre_paste,
                                                                self.column_widths(),
                                                                "paste %s" % new_content)

        self.undoStack.push(paste_command)

    def copy_contents_in_range(self, upper_left_index, lower_right_index, to_clipboard):
        '''
        copy the (textual) content of the cells in provided cell_range -- the copied contents will be
        cast to python Unicode strings and returned. If the to_clipboard flag is true, the contents will
        also be copied to the system clipboard
        '''
        print "upper left index: %s, upper right index: %s" % (self._print_index(upper_left_index), self._print_index(lower_right_index))
        text_matrix = []
        # +1s are because range() is right interval exclusive
        for row in range(upper_left_index.row(), lower_right_index.row()+1):
            current_row = []
            for col in range(upper_left_index.column(), lower_right_index.column()+1):
                cur_index = self.model().createIndex(row, col)
                cur_data = self.model().data(cur_index)
                if cur_data is not None:
                    # this looks redundant, but in fact the toString method
                    # converts the string into a QString
                    cur_str = str(cur_data.toString())
                    current_row.append(cur_str)
                else:
                    current_row.append("")
            text_matrix.append(current_row)

        copied_str = self._matrix_to_str(text_matrix)

        if to_clipboard:
            clipboard = QApplication.clipboard()
            clipboard.setText(copied_str)
        print "copied str: %s" % copied_str
        return copied_str

    def paste_contents(self, upper_left_index, source_content):
        '''
        paste the content in source_content into the matrix starting at the upper_left_coord
        cell. new rows will be added as needed; existing data will be overwritten
        '''
        origin_row, origin_col = upper_left_index.row(), upper_left_index.column()

        if isinstance(source_content[-1], QtCore.QStringList) and \
                             len(str(source_content[-1].join(" ")))==0:
            # then there's a blank line; Excel has a habit
            # of appending blank lines (\ns) to copied
            # text -- we get rid of it here
            source_content = source_content[:-1]

        # temporarily disable sorting to prevent automatic sorting of pasted data.
        # (note: this is consistent with Excel's approach.)
        self.model().blockSignals(True)

        for src_row in range(len(source_content)):
            # do we need to append a row?
            cur_row_count = self.model().rowCount()
            if  cur_row_count <= origin_row + src_row:
                self._add_new_row()
            for src_col in range(len(source_content[0])):
                try:
                    # note that we treat all of the data pasted as
                    # one event; i.e., when undo is called, it undos the
                    # whole paste
                    index = self.model().createIndex(origin_row+src_row, origin_col+src_col)
                    self.model().setData(index, QVariant(source_content[src_row][src_col]))
                except Exception, e:
                    print "whoops, exception while pasting: %s" % e

        self.model().blockSignals(False)
        self.model().reset()

    def set_data_in_model(self, index, val):
        self.model().setData(index, val)
        self.model().reset()

    def column_widths(self):
        ''' returns the current column widths '''
        return [self.columnWidth(col_index) for col_index in range(self.model().columnCount())]

    def set_column_widths(self, widths):
        for col_index, width in enumerate(widths):
            self.setColumnWidth(col_index, width)

    def _print_index(self, index):
        print "(%s, %s)" % (index.row(), index.column())

    def _add_new_row(self):
        '''
        add a new row to the dataTable; note that we briefly toggle sorting off so the row
        is beneath the existing rows.
        '''
        model = self.model()
        cur_row_count = model.rowCount()
        model.insertRow(cur_row_count)

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

    def _matrix_to_str(self, m, col_delimiter="\t", row_delimiter="\n", append_new_line =False):
        ''' takes a matrix of data (i.e., a nested list) and converts to a string representation '''
        m_str = []
        for row in m:
            m_str.append(col_delimiter.join(row))
        return_str = row_delimiter.join(m_str)
        if append_new_line:
            return_str += row_delimiter
        return return_str

    def _upper_left(self, indexes):
        ''' returns the upper most index object in the indexes list.'''
        if len(indexes) > 0:
            upper_left = indexes[0]
            for index in indexes[1:]:
                if index.row() < upper_left.row() or index.column() < upper_left.column():
                    upper_left = index
            return upper_left
        return None

    def _lower_right(self, indexes):
        if len(indexes) > 0:
            lower_right = indexes[0]
            for index in indexes[1:]:
                if index.row() > lower_right.row() or index.column() > lower_right.column():
                    lower_right = index
            return lower_right
        return None


class CommandCellEdit(QUndoCommand):
    '''
    Here we make use of QT's undo/redo framework. This is an UndoCommand for individual
    cell edits (as opposed to paste actions, which are represented by CommandPaste objects,
    defined below).
    '''
    def __init__(self, ma_data_table_view, index, original_content, new_content, description=""):
        super(CommandCellEdit, self).__init__(description)
        self.first_call = True
        self.original_content = original_content
        self.new_content = new_content
        self.row, self.col = index.row(), index.column()
        self.ma_data_table_view = ma_data_table_view
        self.added_study = None

    def redo(self):
        index = self._get_index()
        if self.first_call:
            self.first_call = False
            self.added_study = self.ma_data_table_view.model().study_auto_added
            self.ma_data_table_view.model().study_auto_added = None
        else:
            model = self.ma_data_table_view.model()
            # here we block signals from the model. this is
            # to prevent memory access problems on the c
            # side of things, when the model emits
            # the data edited signal.
            model.blockSignals(True)
            model.setData(index, self.new_content)
            self.added_study = self.ma_data_table_view.model().study_auto_added
            self.ma_data_table_view.model().study_auto_added = None
            model.blockSignals(False)
            # make the view reflect the update
            self.ma_data_table_view.model().reset()

    def undo(self):
        index = self._get_index()
        model = self.ma_data_table_view.model()
        # as in the redo method, we block signals before
        # editing the model data
        model.blockSignals(True)
        model.setData(index, self.original_content)
        model.blockSignals(False)
        self.ma_data_table_view.model().reset()
        if self.added_study is not None:
            self.ma_data_table_view.model().remove_study(self.added_study)

    def _get_index(self):
        return self.ma_data_table_view.model().createIndex(self.row, self.col)

    
class CommandPaste(QUndoCommand):
    '''
    We again make use of QT's undo/redo framework. this implementation handles the paste action;
    the redo is just repasting the former contents into the same cells.
    '''
    def __init__(self, ma_data_table_view, new_content, old_content,
                                        upper_left_coord, old_studies, old_col_widths, description):
        super(CommandPaste, self).__init__(description)
        self.new_content, self.old_content = new_content, old_content
        self.upper_left_coord = upper_left_coord
        self.old_column_widths = old_col_widths
        self.ma_data_table_view = ma_data_table_view
        self.old_studies = old_studies
        self.added_study = None

    def redo(self):
        self.ma_data_table_view.paste_contents(self.upper_left_coord, self.new_content)
        self.added_study = self.ma_data_table_view.model().study_auto_added
        self.ma_data_table_view.model().study_auto_added = None

    def undo(self):
        self.ma_data_table_view.paste_contents(self.upper_left_coord, self.old_content)
        if self.added_study is not None:
            self.ma_data_table_view.model().remove_study(self.added_study)
        self.ma_data_table_view.model().dataset.studies = self.old_studies
        self.ma_data_table_view.model().reset()


class CommandSort(QUndoCommand):
    def __init__(self, ma_data_table_model, col, reverse_order, description="Sort"):
        super(CommandSort, self).__init__(description)
        self.model = ma_data_table_model
        self.col = col
        self.reverse = reverse_order
        self.previous_order = None

    def redo(self):
        self.previous_order = self.model.get_ordered_study_ids()
        self.model.sort_studies(self.col, self.reverse)

    def undo(self):
        self.model.order_studies(self.previous_order)



class StudyDelegate(QItemDelegate):

      def __init__(self, parent=None):
        super(StudyDelegate, self).__init__(parent)

      def setEditorData(self, editor, index):
        text = index.model().data(index, Qt.DisplayRole).toString()
        editor.setText(text)