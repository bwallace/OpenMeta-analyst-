from PyQt4.QtCore import *
from PyQt4.QtGui import *

START_COLUMN = 3

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
        elif event.key() in (Qt.Key_Left, Qt.Key_Backspace) and \
            self.textCursor().columnNumber() == START_COLUMN:
            # we just want to 'block' here, i.e., do nothing; the user
            # has navigated to the start of the column
            pass
            
        else:
            #self.keyPressEvent(event)
            super(QConsole, self).keyPressEvent(event)
            
    def mousePressEvent(self, event):
        ### this works but now you need to set the cursor 
        # on the console initially...
        #self.textCursor().setPosition(100)
        #self.find(">> ")
        for i in range(3):
            self.moveCursor(16)
        self.moveCursor(15)
        print "(mouse clicked)"
        