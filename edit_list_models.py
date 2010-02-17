#################################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  ---
#  Proxy interfaces for mediating between the underlying representation (in ma_dataset.py)
#  and the editing UI.
################################################################

# core libraries
import PyQt4
from PyQt4 import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import pdb

# home-grown
import ma_dataset
from ma_dataset import *
import meta_py_r

class TXGroupModel(QAbstractTableModel):
    '''
    This module mediates between the classes comprising a dataset
    (i.e., study & ma_unit objects) and the view. In particular, we
    subclass the QAbstractTableModel and provide the fields of interest
    to the view.
    '''
    def __init__(self, filename=QString(), dataset=None):
        super(TXGroupModel, self).__init__()
        self.dataset = dataset
        self.group_list = self.dataset.get_group_names()
        
    def data(self, index, role=Qt.DisplayRole):
        if not index.isValid() or not (0 <= index.row() < len(self.dataset)):
            return QVariant()
        group_name = self.group_list[index.row()]
        if role == Qt.DisplayRole:
            return QVariant(group_name)
        elif role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        return QVariant()
    
    def rowCount(self, index=QModelIndex()):
        return len(self.group_list)
        
    def columnCount(self, index=QModelIndex()):
        return 1
        
    def setData(self, index, value, role=Qt.EditRole):
        pass
        
    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)