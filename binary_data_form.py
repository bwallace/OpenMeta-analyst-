from PyQt4.Qt import *
from PyQt4 import QtGui

import ui_binary_data_form
from ui_binary_data_form import Ui_BinaryDataForm

class BinaryDataForm2(QDialog, ui_binary_data_form.Ui_BinaryDataForm):
    
    def __init__(self, parent=None, raw_data=None):
        #QtGui.QFormLayout.addLayout = add_layout_fix
        super(BinaryDataForm2, self).__init__(parent)
        self.setupUi(self)
        self.raw_data = raw_data
        self._populate_raw_data_table()
    
    def _populate_raw_data_table(self):
        #
        # @TODO this method will need to be updated. It is to rigid
        # as is. For one, we'll need to update it to work with a dictionary
        # or struct raw data rather than a list
        #
        col = 0
        print self.raw_data
        for i in range(4):
            item = QTableWidgetItem(str(self.raw_data[i]))
            #print "row: %s, col+offset: %s, item: %s" % (row, col, self.raw_data[row+col])
            row = 0 if i < 2 else 1
            col = i
            if row == 1:
                col = i - 2
            if col in (1,3):
                col += 1
            self.raw_data_table.setItem(row, col, item)
            
        # now compute the numbers with no events, if possible.
        # 
        # the raw data is of the form g_n / g_N where g_N is the *total* 
        # and g_n is the event count. thus no event = g_N - g_n.
        e1, n1, e2, n2 = [int(x) if x != "" else None for x in self.raw_data]
        no_events1, no_events2 = None, None
        if e1 is not None and n1 is not None:
            no_events1 = n1 - e1
            self.raw_data_table.setItem(0, 1, \
                                                        QTableWidgetItem(str(no_events1)))
                                                 
        if e2 is not None and n2 is not None:
            no_events2 = n2 - e2
            self.raw_data_table.setItem(1, 1, \
                                                        QTableWidgetItem(str(no_events2)))
            

        # total the totals (if possible)
        if n1 is not None and n2 is not None:
            self.raw_data_table.setItem(2, 2, \
                                                        QTableWidgetItem(str(n1 + n2)))

        # and the totals of *no* events
        if no_events1 is not None and no_events2 is not None:
            self.raw_data_table.setItem(2, 1, \
                                                        QTableWidgetItem(str(no_events1 + no_events2)))
                    
        # and now compute the sum of events
        if e1 is not None and e2 is not None:
            no_events_total = e1 + e2
            self.raw_data_table.setItem(2, 0, \
                                                        QTableWidgetItem(str(no_events_total)))
                

