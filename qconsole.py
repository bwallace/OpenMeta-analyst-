from PyQt4.QtCore import *
from PyQt4.QtGui import *


class QConsole(QTextEdit):
    def __init__(self, parent):
        super(QConsole, self).__init__(parent)
        self.parent = parent
        
    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Return:
            self.emit(SIGNAL("returnPressed()"))
        elif event.key() == Qt.Key_Up:
            self.emit(SIGNAL("upArrowPressed()"))
        elif event.key() == Qt.Key_Down:
            self.emit(SIGNAL("downArrowPressed()"))
        else:
            #self.keyPressEvent(event)
            super(QConsole, self).keyPressEvent(event)
            