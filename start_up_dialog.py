from PyQt4.Qt import *
import pdb
import ui_start_up

class StartUp(QDialog, ui_start_up.Ui_WelcomeDialog):
    
    def __init__(self, parent=None):

        super(StartUp, self).__init__(parent)
        self.setupUi(self)
        self._setup_connections()
        self.parent = parent
        self.setFocus()
        
        
    def _setup_connections(self):
        QObject.connect(self.create_new_btn, SIGNAL("pressed()"),
                                    self.new_dataset)
        QObject.connect(self.open_btn, SIGNAL("pressed()"),
                                    self.open_dataset)
                                    
        # TODO implement open recent dataset...
        #QObject.connect(self.create_new_btn, SIGNAL("pressed()"),
        #                            self.new_dataset)
      
    def new_dataset(self):
        name = unicode(self.dataset_name_le.text().toUtf8(), "utf-8")
        is_diag = self.diag_radio.isChecked()
        self.parent.new_dataset(name=name, is_diag=is_diag)
        tmp = self.parent.cur_dimension
        self.parent.cur_dimension = "outcome"
        self.hide()
        self.parent.add_new()
        self.parent.cur_dimension = tmp
        self.close()
    
    def open_dataset(self):
        self.parent.open()
        self.close()