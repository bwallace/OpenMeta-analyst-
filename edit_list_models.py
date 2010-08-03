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

class TXGroupsModel(QAbstractTableModel):
    '''
    This module mediates between the classes comprising a dataset
    (i.e., study & ma_unit objects) and the view. In particular, we
    subclass the QAbstractTableModel and provide the fields of interest
    to the view.
    '''
    def __init__(self, filename=QString(), dataset=None):
        super(TXGroupsModel, self).__init__()
        self.dataset = dataset
        self.group_list = self.dataset.get_group_names()
        
    def refresh_group_list(self, outcome, follow_up):
        self.group_list = self.dataset.get_group_names_for_outcome_fu(outcome, follow_up)
        self.reset()
        
    def data(self, index, role=Qt.DisplayRole):
        self.group_list = self.dataset.get_group_names()
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
        old_name = self.group_list[index.row()]
        new_name = unicode(value.toString().toUtf8(), "utf-8")

        ###
        # we don't allow empty strings for group names; just pass
        # if this happens (typically this will be an accident on the user's part)
        if new_name == "":
            return False
        self.dataset.change_group_name(old_name, new_name)
        return True
        
    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)

        
class OutcomesModel(QAbstractTableModel):
    '''
    A simple table model for editing/deleting/adding outcomes.
    Subclasses the QAbstractTableModel and provide the fields of interest
    to the view.
    '''
    def __init__(self, filename=QString(), dataset=None):
        super(OutcomesModel, self).__init__()
        self.dataset = dataset
        self.outcome_list = self.dataset.get_outcome_names()
        
    def refresh_outcome_list(self):
        self.outcome_list = self.dataset.get_outcome_names()
        self.reset()
        
    def data(self, index, role=Qt.DisplayRole):
        self.outcome_list = self.dataset.get_outcome_names()
        if not index.isValid() or not (0 <= index.row() < len(self.dataset)):
            return QVariant()
        outcome_name = ""
        try:
            outcome_name = self.outcome_list[index.row()]
        except:
            pass
        if role == Qt.DisplayRole:
            return QVariant(outcome_name)
        elif role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        return QVariant()
    
    def rowCount(self, index=QModelIndex()):
        return len(self.outcome_list)
        
    def columnCount(self, index=QModelIndex()):
        return 1
        
    def setData(self, index, value, role=Qt.EditRole):
        old_outcome_name = self.outcome_list[index.row()]
        new_outcome_name = unicode(value.toString().toUtf8(), "utf-8")
        if new_outcome_name == "":
            return False
            
        self.dataset.change_outcome_name(old_outcome_name, new_outcome_name)
        return True
                
    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)
                            
class FollowUpsModel(QAbstractTableModel):
    '''
    A simple table model for editing/deleting/adding follow-ups.
    Subclasses the QAbstractTableModel and provide the fields of interest
    to the view.
    '''
    def __init__(self, filename=QString(), dataset=None, outcome = None):
        super(FollowUpsModel, self).__init__()
        self.dataset = dataset
        ## we maintain a current outcome string variable because
        # the follow-ups are outcome specific
        self.current_outcome = outcome
        self.follow_up_list = self.dataset.get_follow_up_names()
        
    def refresh_follow_up_list(self):  
        self.follow_up_list = self.dataset.get_follow_up_names_for_outcome(self.current_outcome)
        self.reset()
        
    def data(self, index, role=Qt.DisplayRole):
        if not index.isValid() or not (0 <= index.row() < len(self.dataset)):
            return QVariant()
        follow_up_name = None
        try:
            follow_up_name = self.follow_up_list[index.row()] 
        except:
            pass
            
        if role == Qt.DisplayRole:
            return QVariant(follow_up_name)
        elif role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        return QVariant()
    
    def rowCount(self, index=QModelIndex()):
        return len(self.follow_up_list)
        
    def columnCount(self, index=QModelIndex()):
        return 1
        
    def setData(self, index, value, role=Qt.EditRole):
        old_follow_up_name = self.follow_up_list[index.row()]
        new_follow_up_name = unicode(value.toString().toUtf8(), "utf-8")
        self.dataset.change_follow_up_name(self.current_outcome, old_follow_up_name, new_follow_up_name)
        self.refresh_follow_up_list()
        #self.dataset.outcome_names_to_follow_ups[self.current_outcome]
        pyqtRemoveInputHook()
        pdb.set_trace()
        return True
        
        
        
    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(QAbstractTableModel.flags(self, index)|
                            Qt.ItemIsEditable)
                            