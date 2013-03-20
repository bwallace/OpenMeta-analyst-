#############################################
#                                           #
#  Byron C. Wallace                         #
#  Brown University                         #
#  OpenMeta[analyst]                        #
#                                           #
#  This is the component responsible        #
#  for rendering MA results.                #
#                                           #
#############################################

import random
from PyQt4.Qt import *
import pdb
import os
import sys
import ui_results_window
import edit_forest_plot_form
import meta_py_r

PageSize = (612, 792)
padding = 25
horizontal_padding = 75
SCALE_P = .5 # percent images are to be scaled

# these are special forest plots, in that multiple parameters objects are
# require to re-generate them (and we invoke a different method!)
SIDE_BY_SIDE_FOREST_PLOTS = ("NLR and PLR Forest Plot", "Sensitivity and Specificity")
ROW_HEIGHT = 15 # by trial-and-error; seems to work very well

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
        
        #########################
        print("results from results window:",results)
        print("--------------- END OF RESULTS FROM RESULTS WINDOW")
        ########################

        QObject.connect(self.nav_tree, SIGNAL("itemClicked(QTreeWidgetItem*, int)"),
                                       self.item_clicked)
                       
        self.psuedo_console.blockSignals(False)              
        QObject.connect(self.psuedo_console, SIGNAL("returnPressed(void)"),
                                       self.process_console_input)
        QObject.connect(self.psuedo_console, SIGNAL("upArrowPressed()"),
                                       self.f)
        QObject.connect(self.psuedo_console, SIGNAL("downArrowPressed()"),
                                       self.f)
                                       
                              
        self.nav_tree.setHeaderLabels(["results"])
        self.nav_tree.setItemsExpandable(True)
        self.x_coord = 5
        self.y_coord = 5

        # set (default) splitter sizes
        self.splitter.setSizes([400, 100])
        self.results_nav_splitter.setSizes([200,500])

        self.scene = QGraphicsScene(self)

        self.images = results["images"]
        print "images returned from analytic routine: %s" % self.images
        self.image_order = None
        if "image_order" in results:
            self.image_order = results["image_order"]
            print "image display order: %s" % self.image_order

        self.params_paths = {}
        if "image_params_paths" in results:
            self.params_paths = results["image_params_paths"]
    
        self.image_var_names = results["image_var_names"]
        self.set_psuedo_console_text()
        self.items_to_coords = {}
        self.texts = results["texts"]


        # first add the text to self.scene
        self.add_text()

        self.y_coord += ROW_HEIGHT/2.0

        # additional padding for Windows..
        # again, hueristic. I don't know
        # why windows requires so much padding.
        if sys.platform.startswith('win'):
            self.y_coord += 2*ROW_HEIGHT

        # and now the images
        self.add_images()

        # reset the scene
        self.graphics_view.setScene(self.scene)
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
        # temporary fix!
        image_order = self.images.keys()
        if self.image_order is not None:
            image_order = self.image_order

        for title in image_order:
            image = self.images[title]
            print "title: %s; image: %s" % (title, image)
            cur_y = max(0, self.y_coord)
            print "cur_y: %s" % cur_y
            # first add the title
            qt_item = self.add_title(title)

            pixmap = self.generate_pixmap(image)
            
            # if there is a parameters object associated with this object
            # (i.e., it is a forest plot of some variety), we pass it along
            # to the create_pixmap_item method to for the context_menu 
            # construction
            params_path = None
            if self.params_paths is not None and title in self.params_paths:
                params_path = self.params_paths[title]

            img_shape, pos, pixmap_item = self.create_pixmap_item(pixmap, self.position(),\
                                                title, image, params_path=params_path)

            self.items_to_coords[qt_item] = pos
            
            


    def generate_pixmap(self, image):
        # now the image
        pixmap = QPixmap(image)
        
        ###
        # we scale to address issue #23.
        # should probably pick a 'target' width/height, in case
        # others generate smaller images by default.
        scaled_width = SCALE_P*pixmap.width()
        scaled_height = SCALE_P*pixmap.height()
        

        if scaled_width > self.scene.width():
            self.scene.setSceneRect(0, 0, \
                                scaled_width+horizontal_padding,\
                                self.scene.height())
        

        pixmap = pixmap.scaled(scaled_width, scaled_height, \
                            transformMode=Qt.SmoothTransformation)

        return pixmap


    def add_text(self):
        for title, text in self.texts.items():
            try:
                print "title: %s; text: %s" % (title, text)
                cur_y = max(0, self.y_coord)
                print "cur_y: %s" % cur_y
                # first add the title
                qt_item = self.add_title(title)

                # now the text
                text_item_rect, pos = self.create_text_item(str(text), self.position())
                self.items_to_coords[qt_item] =  pos
            except:
                pass

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
        self.scene.setSceneRect(0, 0, self.scene.width(), self.y_coord + padding)
        return qt_item

    def item_clicked(self, item, column):
        print self.items_to_coords[item]
        self.graphics_view.centerOn(self.items_to_coords[item])

    def create_text_item(self, text, position):
        txt_item = QGraphicsTextItem(QString(text))
        txt_item.setFont(QFont("courier", 12))
        txt_item.setToolTip("To copy the text:\n1) Right click on the text and choose \"Select All\".\n2) Right click again and choose \"Copy\".")
        txt_item.setTextInteractionFlags(Qt.TextEditable)
        self.scene.addItem(txt_item)
        # fix for issue #149; was formerly txt_item.boundingRect().size().height()
        
        self.y_coord += ROW_HEIGHT*text.count("\n")
        self.scene.setSceneRect(0, 0, max(self.scene.width(), \
                                          txt_item.boundingRect().size().width()),\
                                          self.y_coord+padding)
        

        txt_item.setPos(position)
        return (txt_item.boundingRect(), position)


    def process_console_input(self):
        res = meta_py_r.evaluate_in_r(self.current_line())

        # echo the result
        self.psuedo_console.append(QString(res))
        self.psuedo_console.append(">> ")

    def current_line(self):
        last_line = self.psuedo_console.toPlainText().split("\n")[-1]
        return str(last_line.replace(">>", "")).strip()

    def _get_plot_type(self, title):
        # at present we use the *title* as the type --
        # this is currently _not_ set by the user, so it's
        # 'safe', but it's not exactly elegant. probably
        # we should return a type directly from R.
        # on other hand, this couples R + Python even
        # more...
        plot_type = None
        tmp_title = title.lower()
        if "forest" in tmp_title:
            plot_type = "forest"
        elif "regression" in tmp_title:
            plot_type = "regression"
        return plot_type

    def create_pixmap_item(self, pixmap, position, title, image_path,\
                             params_path=None, matrix=QMatrix()):
        item = QGraphicsPixmapItem(pixmap)
        item.setToolTip("To save the image:\nright-click on the image and choose \"save image as\".")
        
        
        self.y_coord += item.boundingRect().size().height()
#        item.setFlags(QGraphicsItem.ItemIsSelectable|
#                      QGraphicsItem.ItemIsMovable)
        item.setFlags(QGraphicsItem.ItemIsSelectable)


        self.scene.setSceneRect(0, 0, \
                                   max(self.scene.width(), \
                                   item.boundingRect().size().width()),\
                                   self.y_coord+padding)

        print "creating item @:%s" % position
        
        item.setMatrix(matrix)
        self.scene.clearSelection()
        self.scene.addItem(item)
        item.setPos(position)
        
        # for now we're inferring the plot type (e.g., 'forest'
        # from the title of the plot (see in-line comments, above)
        plot_type = self._get_plot_type(title)

        # attach event handler for mouse-clicks, i.e., to handle
        # user right-clicks
        if params_path is not None:
            item.contextMenuEvent = self._make_context_menu(\
                            params_path, title, image_path, item,
                            plot_type=plot_type)

        return (item.boundingRect().size(), position, item)


    def _make_context_menu(self, params_path, title, png_path, 
                                qpixmap_item, plot_type="forest"):
        def _graphics_item_context_menu(event):
            action = QAction("save image as...", self)
            QObject.connect(action, SIGNAL("triggered()"), \
                        lambda : self.save_image_as(params_path, title, 
                                        plot_type=plot_type))
            context_menu = QMenu(self)
            context_menu.addAction(action)

            # only know how to edit *simple* (i.e., _not_ side-by-side, as 
            # in sens and spec plotted on the same canvass) forest plots for now
            if plot_type == "forest" and not self._is_side_by_side_fp(title):
                action = QAction("edit plot...", self)
                QObject.connect(action, SIGNAL("triggered()"), \
                            lambda : self.edit_image(\
                                       params_path, title, png_path, qpixmap_item))
                context_menu.addAction(action)

            pos = event.screenPos()
            context_menu.popup(pos)
            event.accept()

        return _graphics_item_context_menu


    def _is_side_by_side_fp(self, title):
        return any([side_by_side in title for side_by_side in SIDE_BY_SIDE_FOREST_PLOTS])

    def save_image_as(self, params_path, title, plot_type="forest"):
        # note that the params object will, by convention,
        # have the (generic) name 'plot.data' -- after this
        # call, this object will be in the namespace
        meta_py_r.load_in_R("%s.plotdata" % params_path)

        default_path = \
            {"forest":"forest_plot.pdf", \
             "regression":"regression.pdf"}[plot_type]

        # where to save the graphic?
        file_path = unicode(QFileDialog.getSaveFileName(self, 
                                                        "OpenMeta[Analyst] -- save plot as", 
                                                        default_path))

        # now we re-generate it, unless they canceled, of course
        if file_path != "":
            if plot_type == "forest":
                if self._is_side_by_side_fp(title):
                    meta_py_r.generate_forest_plot(file_path, side_by_side=True)
                else:
                    meta_py_r.generate_forest_plot(file_path)
            elif plot_type == "regression":
                meta_py_r.generate_reg_plot(file_path)
            else:
                print "sorry -- I don't know how to draw %s plots!" % plot_type

    def edit_image(self, params_path, title, png_path, pixmap_item):
        plot_editor_window = edit_forest_plot_form.EditPlotWindow(\
                                            params_path, png_path,\
                                            pixmap_item, parent=self)
        if plot_editor_window is not None:
            plot_editor_window.show()
        else:
            # TODO show a warning
            print "sorry - can't edit"
        
    def position(self):
        point = QPoint(self.x_coord, self.y_coord)
        return self.graphics_view.mapToScene(point)

