import random
from PyQt4.Qt import *
import pdb

import ui_results_window


PageSize = (612, 792)
padding = 25

class ResultsWindow(QMainWindow, ui_results_window.Ui_ResultsWindow):

    def __init__(self, results, parent=None):

        super(ResultsWindow, self).__init__(parent)
        self.setupUi(self)
        self.copied_item = QByteArray()
        self.paste_offset = 5
        self.add_offset = 5
        self.buffer_size = 2
        self.prev_point = QPoint()
        self.borders = []
        self.printer = QPrinter(QPrinter.HighResolution)
        self.printer.setPageSize(QPrinter.Letter)


        QObject.connect(self.nav_tree, SIGNAL("itemClicked(QTreeWidgetItem*, int)"),
                                       self.item_clicked)
        QObject.connect(self.nav_tree, SIGNAL("itemSelectionChanged()"),
                                       self.yo)


        #QObject.connect(self.nav_tree, SIGNAL("currentItemChanged(QTreeWidgetItem*, QTreeWidgetItem*)"), self.x)
        self.nav_tree.setHeaderLabels(["results"])
        self.nav_tree.setItemsExpandable(True)

        self.x_coord = 5
        self.y_coord = 5
        self.scene = QGraphicsScene(self)
        self.scene.setSceneRect(0, 0, PageSize[0], PageSize[1])
        self.graphics_view.setScene(self.scene)

        self.images = results["images"]
        print self.images
        self.items_to_coords = {}

        self.texts = results["texts"]


        self.add_text()
        self.add_images()

    def add_images(self):
        for title, image in self.images.items():
            print "title: %s; image: %s" % (title, image)
            cur_y = max(0, self.y_coord)
            print "cur_y: %s" % cur_y
            # first add the title
            qt_item = self.add_title(title)

            # now the image
            pixmap = QPixmap(image)
            img_shape = self.create_pixmap_item(pixmap, self.position())
            disp_rect = QRectF(self.x_coord, cur_y, img_shape.width(), img_shape.height()+padding)
            self.items_to_coords[qt_item] =  disp_rect

    def add_text(self):
        for title, text in self.texts.items():
            print "title: %s; text: %s" % (title, text)
            cur_y = max(0, self.y_coord)
            print "cur_y: %s" % cur_y
            # first add the title
            qt_item = self.add_title(title)

            # now the text
            text_item_rect = self.create_text_item(str(text), self.position())
            self.items_to_coords[qt_item] =  text_item_rect


    def add_title(self, title):
        text = QGraphicsTextItem()
        # I guess we should use a style sheet here,
        # but it seems like it'd be overkill.
        html_str = \
           '<p style="font-size: 14pt; color: black; face:verdana">%s</p>' % title
        text.setHtml(html_str)
        text.setPos(self.position())
        print "title at: %s" % self.y_coord
        self.y_coord += padding
        self.scene.addItem(text)
        qt_item = QTreeWidgetItem(self.nav_tree, [title])
        self.scene.setSceneRect(0, 0, PageSize[0], self.y_coord+padding)
        return qt_item

    def item_clicked(self, item, column):
        print self.items_to_coords[item]
        #self.graphics_view.centerOn(self.x_coord, self.items_to_coords[item])
        self.graphics_view.ensureVisible(self.items_to_coords[item])

    def create_text_item(self, text, position):
        txt_item = QGraphicsTextItem(QString(text))
        #txt_item.setText(
        self.scene.addItem(txt_item)
        self.y_coord +=txt_item.boundingRect().size().height()
        self.scene.setSceneRect(0, 0, max(PageSize[0], txt_item.boundingRect().size().width()), self.y_coord+padding)
        txt_item.setPos(position)
        return txt_item.boundingRect()


    def create_pixmap_item(self, pixmap, position, matrix=QMatrix()):
        item = QGraphicsPixmapItem(pixmap)
        self.y_coord +=item.boundingRect().size().height()
        item.setFlags(QGraphicsItem.ItemIsSelectable|
                      QGraphicsItem.ItemIsMovable)
        item.setPos(position)
        item.setMatrix(matrix)
        self.scene.clearSelection()
        self.scene.addItem(item)
        #item.setSelected(True)
        self.scene.setSceneRect(0, 0, PageSize[0], self.y_coord+padding)
        return item.boundingRect().size()

    def position(self):
        point = QPoint(self.x_coord, self.y_coord)
        return self.graphics_view.mapToScene(point)