from PyQt4.Qt import *
import ui_network_view
import edit_list_models
import meta_py_r
import pdb

PageSize = (500, 300)

class ViewDialog(QDialog, ui_network_view.Ui_network_view_dialog):

    def __init__(self, model, parent=None):
        super(ViewDialog, self).__init__(parent)
        self.setupUi(self)
        
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
        nodes, edges = self.dataset.get_network(outcome, follow_up)
        # get_network returns a list of tuples, wherein
        # each tuple is an edge with group names representing
        # nodes, e.g., [("tx a", "tx b"), ("tx b", "tx c")]. however the igraph
        # library wants a flat list, e.g., ["tx_a", "tx_b", "tx_b", "tx_c"]
        # thus we flatten out the list here

        flattened_edges = []
        for edge in edges:
            flattened_edges.extend(edge)

        # now add nodes that have no connections; these won't be
        # included in the edgelist, as they don't belong to edges
        unconnected_vertices = []
        for node in nodes:
            if node not in flattened_edges:
                unconnected_vertices.append(node)
        
        img_path = meta_py_r.draw_network(flattened_edges, unconnected_vertices)
        print img_path
        # now add the image to the display
        pixmap = QPixmap(img_path)
        #pixmap = pixmap.scaled(QSize(PageSize[0], PageSize[1]))
        #pixmap.resize(PageSize[0], PageSize[1])
        self.scene.addPixmap(pixmap)

        