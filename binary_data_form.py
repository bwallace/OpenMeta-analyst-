from PyQt4.Qt import *
from PyQt4 import QtGui
import meta_py_r

import ui_binary_data_form
from ui_binary_data_form import Ui_BinaryDataForm

# @TODO this should be an *application global*. It is now a
# global here and in the data_table_view class. (However
# here we show four digits; there it is 3. We want different
# levels of granularity).
NUM_DIGITS = 4 

class BinaryDataForm2(QDialog, ui_binary_data_form.Ui_BinaryDataForm):
    
    def __init__(self, ma_unit, cur_txs, cur_effect, parent=None):
        super(BinaryDataForm2, self).__init__(parent)
        self.setupUi(self)
        self.ma_unit = ma_unit
        self.raw_data = self.ma_unit.get_raw_data_for_groups(cur_txs)
        self.cur_groups = cur_txs
        self.cur_effect = cur_effect
        self._populate_raw_data_table()
        self._populate_effect_data()
    
    def _populate_effect_data(self):
        q_effects = sorted([QString(effect_str) for effect_str in self.ma_unit.effects_dict.keys()])
        self.effect_cbo_box.addItems(q_effects)
        self.effect_cbo_box.setCurrentIndex(q_effects.index(QString(self.cur_effect)))
        effect_dict = self.ma_unit.effects_dict[self.cur_effect]
        for s, txt_box in zip(['est', 'lower', 'upper'], [self.effect_txt_box, self.low_txt_box, self.high_txt_box]):
            if effect_dict[s] is not None:
                txt_box.setText(QString("%s" % round(effect_dict[s], NUM_DIGITS)))
            else:
                txt_box.setText(QString(""))

        
    def _populate_raw_data_table(self):
        ''' Generates the 2x2 table with whatever parametric data was provided '''
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
                

