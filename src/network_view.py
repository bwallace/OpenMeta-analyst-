from PyQt4.Qt import *
import forms.ui_network_view
import edit_list_models
import meta_py_r
import pdb

PageSize = (500, 300)

class ViewDialog(QDialog, forms.ui_network_view.Ui_network_view_dialog):

    def __init__(self, model, parent=None):
        super(ViewDialog, self).__init__(parent)
        self.setupUi(self)
        
        self.model = model
        self.dataset = model.dataset
        self.cur_outcome = model.current_outcome
        self.cur_follow_up = model.get_current_follow_up_name()
        
        self.populate_cbo_boxes()
        self.setup_signals()
        
        self.x_coord = 5
        self.y_coord = 5
        self.scene = QGraphicsScene(self)
        self.scene.setSceneRect(0, 0, PageSize[0], PageSize[1])
        self.network_viewer.setScene(self.scene)
        self.graph_network(self.cur_outcome, self.cur_follow_up)
    
    def setup_signals(self):
        QObject.connect(self.outcome_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                                            self.outcome_changed)
        QObject.connect(self.follow_up_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                                            self.follow_up_changed)
        
    def outcome_changed(self, new_outcome):
        self.cur_outcome = str(new_outcome)
        self.graph_network(self.cur_outcome, self.cur_follow_up)
        
    def follow_up_changed(self, new_follow_up):
        self.cur_follow_up = str(new_follow_up)
        self.graph_network(self.cur_outcome, self.cur_follow_up)
        
    def populate_cbo_boxes(self):
        self.outcome_cbo_box.addItems(self.dataset.get_outcome_names())
        self.follow_up_cbo_box.addItems(self.dataset.get_follow_up_names())

        # set the current item to reflect the selected/active outcome
        # and follow-up
        cur_outcome_index = self.outcome_cbo_box.findText(self.cur_outcome)
        self.outcome_cbo_box.setCurrentIndex(cur_outcome_index)
        
        cur_follow_up_index = self.follow_up_cbo_box.findText(self.cur_follow_up)
        self.follow_up_cbo_box.setCurrentIndex(cur_follow_up_index)
        
    def graph_network(self, outcome, follow_up):
        data_type = self.model.get_outcome_type(outcome, get_str=False)
        
        img_path = meta_py_r.ma_dataset_to_simple_network(
                                      table_model=self.model,
                                      data_type=data_type,
                                      outcome=outcome,
                                      follow_up=follow_up)
        pixmap = QPixmap(img_path)
        self.scene.addPixmap(pixmap)