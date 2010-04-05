#################################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Binary data form module; for flexible entry of dichotomous
#  outcome data
################################################################


from PyQt4.Qt import *
from PyQt4 import QtGui
import meta_py_r

import ui_continuous_data_form
from ui_continuous_data_form import Ui_ContinuousDataForm

# @TODO this should be an *application global*. It is now a
# global here and in the data_table_view class. (However
# here we show four digits; there it is 3. We want different
# levels of granularity).
NUM_DIGITS = 4 

class ContinuousDataForm(QDialog, ui_continuous_data_form.Ui_ContinuousDataForm):
        def __init__(self, ma_unit, cur_txs, cur_effect, parent=None):
            super(ContinuousDataForm, self).__init__(parent)
            self.setupUi(self)
            #self._setup_signals_and_slots()
            self.ma_unit = ma_unit
            self.raw_data = self.ma_unit.get_raw_data_for_groups(cur_txs)
            self.cur_groups = cur_txs
            self.cur_effect = cur_effect
            #self._update_raw_data()
            #self._update_data_table()
            #self._populate_effect_data()