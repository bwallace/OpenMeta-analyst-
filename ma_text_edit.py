import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import pdb

class MATextEdit(QTextEdit):
    def __init__(self, *args):
        QTextEdit.__init__(self, *args)

    def event(self, event):
        if event.type()==QEvent.KeyPress:
           if event.key()==Qt.Key_Enter or event.key()==Qt.Key_Return:
              self.emit(SIGNAL("returnPressed"))
              return True
           elif event.key()==Qt.Key_Up:
              print "up??"
              return True

        return QTextEdit.event(self, event)
