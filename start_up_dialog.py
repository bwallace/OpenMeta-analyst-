from PyQt4.Qt import *
import pdb
import ui_start_up

class StartUp(QDialog, ui_start_up.Ui_WelcomeDialog):
    
    def __init__(self, parent=None, recent_datasets=None, start_up=True):

        super(StartUp, self).__init__(parent)
        self.recent_datasets = recent_datasets or []
        ###
        # most recently accessed dataset first
        self.recent_datasets.reverse()
        self.setupUi(self)
        self.start_up = start_up
        self._setup_connections()
        self.parent = parent
        self.setFocus()
        self.raise_()
        if not self.start_up:
            ### 
            # in the case that the user has selected
            # 'new dataset', we don't want to show
            # options to, e.g., open an existing
            # dataset.
            self.setWindowTitle("New Dataset")
            self.chk_show.setVisible(False)
            self.open_btn.setVisible(False)
            self.open_recent_btn.setVisible(False)
            self.adjustSize()
      
    def _setup_connections(self):
        QObject.connect(self.create_new_btn, SIGNAL("pressed()"),
                                    self.new_dataset)
        QObject.connect(self.open_btn, SIGNAL("pressed()"),
                                    self.open_dataset)
        QObject.connect(self.chk_show,  SIGNAL("stateChanged(int)"),
                            lambda: self.parent.update_user_prefs("splash", \
                                    self.chk_show.isChecked()))

        if self.start_up and len(self.recent_datasets) > 0:
            ### 
            # then add a drop-down to the 'open recent' 
            # button with the recent datasets.
            qm = QMenu()
            for dataset in self.recent_datasets:
                action_item = QAction(QString(dataset), qm)
                qm.addAction(action_item)
                # I wanted to handle this with lambdas, but the method would
                # inexplicably always be invoked with the last dataset as the
                # argument. Instead, I've opted to use the .sender method to
                # retrieve the action_item, i.e., dataset, selected (see
                # the dataset_selected routine).
                QObject.connect(action_item, SIGNAL("triggered()"), self.dataset_selected) 
            self.open_recent_btn.setMenu(qm)
        else:
            self.open_recent_btn.setEnabled(False)
       
    def dataset_selected(self):
        # we use the sender method to see which menu item was
        # triggered
        dataset_path = QObject.sender(self).text()
        self.parent.open(file_path=dataset_path)
        self.close()
        
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
        # fix for issue #61 -keep dialog open if the 
        # user cancels.
        if self.parent.open():
            self.close()