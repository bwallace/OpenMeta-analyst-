from PyQt4.Qt import *
import pdb
import ui_start_upV2

import create_new_project_dialog
import meta_globals

class StartUp(QDialog, ui_start_upV2.Ui_WelcomeDialog):
    
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
        #QObject.connect(self.chk_show,  SIGNAL("stateChanged(int)"),
        #                    lambda: self.parent.update_user_prefs("splash", \
        #                            self.chk_show.isChecked()))

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
        #name = unicode(self.dataset_name_le.text().toUtf8(), "utf-8")
        name = unicode("untitled_dataset", "utf-8")
        #is_diag = self.diag_radio.isChecked()
        
        new_project_dialog = create_new_project_dialog.CreateNewProjectDlg(parent=self)
        if new_project_dialog.exec_():
            dataset_info = new_project_dialog.get_summary()
            is_diag = dataset_info['data_type'] == "Diagnostic"
        
            self.parent.new_dataset(name=name, is_diag=is_diag)
            print("error before here?")
            tmp = self.parent.cur_dimension
            self.parent.cur_dimension = "outcome"
            self.hide()
            self.parent.add_new(dataset_info)        
            self.parent.cur_dimension = tmp
            
            # SET DEFAULT METRICS
            if dataset_info['data_type'] == "Binary":
                if dataset_info['arms'] == "one":
                    self.parent.model.current_effect = meta_globals.DEFAULT_BINARY_ONE_ARM
                elif dataset_info['arms'] == "two":
                    self.parent.model.current_effect = meta_globals.DEFAULT_BINARY_TWO_ARM
            if dataset_info['data_type'] == "Continuous":
                if dataset_info['arms'] == "one":
                    self.parent.model.current_effect = meta_globals.DEFAULT_CONTINUOUS_ONE_ARM
                elif dataset_info['arms'] == "two":
                    self.parent.model.current_effect = meta_globals.DEFAULT_CONTINUOUS_TWO_ARM
            # Put this stuff in an if just in case...
            if dataset_info['data_type'] == "Binary" or dataset_info['data_type'] == "Continuous":
                self.parent.populate_metrics_menu(metric_to_check=self.parent.model.current_effect)
                self.parent.model.try_to_update_outcomes()
                self.parent.model.reset()
            
            
            self.close()
    
    def open_dataset(self):
        # fix for issue #61 -keep dialog open if the 
        # user cancels.
        if self.parent.open():
            self.close()