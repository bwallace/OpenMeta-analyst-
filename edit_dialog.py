from PyQt4.Qt import *
import ui_edit_dialog
import edit_list_models
import meta_py_r
import pdb

PageSize = (500, 300)

class EditDialog(QDialog, ui_edit_dialog.Ui_edit_dialog):

    def __init__(self, dataset, parent=None):
        super(EditDialog, self).__init__(parent)
        self.setupUi(self)
        self.groups_model = edit_list_models.TXGroupModel(dataset = dataset)
        self.group_list.setModel(self.groups_model)
        
        self.dataset = dataset
        self.x_coord = 5
        self.y_coord = 5
        self.scene = QGraphicsScene(self)
        self.scene.setSceneRect(0, 0, PageSize[0], PageSize[1])
        self.network_viewer.setScene(self.scene)
        self.graph_network()
        #pyqtRemoveInputHook()
        #pdb.set_trace()
        
    def graph_network(self):
        edges = self.dataset.get_network("death", 0)
        # get_network returns a list of tuples, wherein
        # each tuple is an edge with group names representing
        # nodes, e.g., [("tx a", "tx b"), ("tx b", "tx c")]. however the igraph
        # library wants a flat list, e.g., ["tx_a", "tx_b", "tx_b", "tx_c"]
        # thus we flatten out the list here
        flattened_edges = []
        for edge in edges:
            flattened_edges.extend(edge)
        img_path = meta_py_r.draw_network(flattened_edges)
        print img_path
        # now add the image to the display
        pixmap = QPixmap(img_path)
        pixmap = pixmap.scaled(QSize(PageSize[0], PageSize[1]))
        #pixmap.resize(PageSize[0], PageSize[1])
        self.scene.addPixmap(pixmap)

        