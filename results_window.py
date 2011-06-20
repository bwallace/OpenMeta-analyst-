#############################################
#                                           #
#  Byron C. Wallace                         #
#  Tufts Medical Center                     #
#  OpenMeta[analyst]                        #
#                                           #
#  This is the component responsible        #
#  for rendering MA results.                #
#                                           #
#############################################

import random
from PyQt4.Qt import *
import pdb

import ui_results_window
import meta_py_r


PageSize = (612, 792)
padding = 25
SCALE_P = .5 # percent images are to be scaled

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
                         
        self.psuedo_console.blockSignals(False)              
        QObject.connect(self.psuedo_console, SIGNAL("returnPressed()"),
                                       self.process_console_input)
        QObject.connect(self.psuedo_console, SIGNAL("upArrowPressed()"),
                                       self.f)
        QObject.connect(self.psuedo_console, SIGNAL("downArrowPressed()"),
                                       self.f)
                                       
                                       
        self.nav_tree.setHeaderLabels(["results"])
        self.nav_tree.setItemsExpandable(True)
        self.x_coord = 5
        self.splitter.setSizes([400, 100])
        self.y_coord = 5
        self.scene = QGraphicsScene(self)
        self.scene.setSceneRect(0, 0, PageSize[0], PageSize[1])
        self.graphics_view.setScene(self.scene)

        self.images = results["images"]
        print "images returned from analytic routine: %s" % self.images
        
        self.image_var_names = results["image_var_names"]
        self.set_psuedo_console_text()
        self.items_to_coords = {}
        self.texts = results["texts"]

        self.add_text()
        self.add_images()
        # scroll to top
        self.graphics_view.ensureVisible(QRectF(0,0,0,0))


    def f(self):
        print self.current_line()

    def set_psuedo_console_text(self):
        text = ["\t\tOpenMeta(analyst)",
               "This is a pipe to the R console. The image names are as follows:"]
        if self.image_var_names is not None:
            for image_var_name in self.image_var_names.values():
                text.append(image_var_name)
        self.psuedo_console.setPlainText(QString("\n".join(text)))
        self.psuedo_console.append(">> ")


    def add_images(self):
        for title, image in self.images.items():
            print "title: %s; image: %s" % (title, image)
            cur_y = max(0, self.y_coord)
            print "cur_y: %s" % cur_y
            # first add the title
            qt_item = self.add_title(title)
            
            # now the image
            pixmap = QPixmap(image)
            
            ###
            # we scale to address issue #23.
            # should probably pick a 'target' width/height, in case
            # others generate smaller images by default.
            scaled_width = SCALE_P*pixmap.width()
            scaled_height = SCALE_P*pixmap.height()
            
            # arbitrary
            pixmap = pixmap.scaled(scaled_width, scaled_height, transformMode = Qt.SmoothTransformation)
            
            img_shape = self.create_pixmap_item(pixmap, self.position())
            disp_rect = QRectF(self.x_coord, cur_y, img_shape.width(), img_shape.height()+padding)
            
            #pyqtRemoveInputHook()
            #pdb.set_trace()
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
        txt_item.setFont(QFont("courier", 8))
        txt_item.setTextInteractionFlags(Qt.TextEditable)
        self.scene.addItem(txt_item)
        self.y_coord +=txt_item.boundingRect().size().height()
        self.scene.setSceneRect(0, 0, max(PageSize[0], txt_item.boundingRect().size().width()), self.y_coord+padding)
        txt_item.setPos(position)
        return txt_item.boundingRect()


    def process_console_input(self):
        res = meta_py_r.evaluate_in_r(self.current_line())
        #self.psuedo_console.setPlainText(QString(old_text + res))
        # echo the result
        self.psuedo_console.append(QString(res))
        self.psuedo_console.append(">> ")

    def current_line(self):
        last_line = self.psuedo_console.toPlainText().split("\n")[-1]
        return str(last_line.replace(">>", "")).strip()

    def create_pixmap_item(self, pixmap, position, matrix=QMatrix()):
        item = QGraphicsPixmapItem(pixmap)
        self.y_coord +=item.boundingRect().size().height()
        item.setFlags(QGraphicsItem.ItemIsSelectable|
                      QGraphicsItem.ItemIsMovable)
        item.setPos(position)
        item.setMatrix(matrix)
        self.scene.clearSelection()
        self.scene.addItem(item)
        self.scene.setSceneRect(0, 0, PageSize[0], self.y_coord+padding)
        return item.boundingRect().size()

    def position(self):
        point = QPoint(self.x_coord, self.y_coord)
        return self.graphics_view.mapToScene(point)

