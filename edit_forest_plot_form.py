import pdb
import os

from PyQt4.Qt import *
from PyQt4 import QtGui

import ui_edit_forest_plot
import ma_specs
import meta_py_r


class EditPlotWindow(QDialog, ui_edit_forest_plot.Ui_edit_forest_plot_dlg):

    def __init__(self, img_params_path, png_path, qpixmap_item, parent=None):
        super(EditPlotWindow, self).__init__(parent)
        self.setupUi(self)

        # img_params is a string that is the variable
        # name for the R object 
        self.img_params_path = img_params_path
        print "parameters: %s" % self.img_params_path

        # if we're unable to load the required R data files,
        # e.g., because they were moved or deleted, then fail
        if not meta_py_r.load_vars_for_plot(self.img_params_path):
            print "can't load R data for plot editing!"
            return None

        # @TODO reflect current params in UI at launch

        # this is the QPixMap object that houses the
        # plot image
        self.pixmap_item = qpixmap_item

        self.png_path = png_path

        # the handle to the window in which
        # the image is being displayed
        self.results_window = parent
        
        #self.current_param_vals = meta_py_r.load_plot_params(self.img_params_path, \
        #                                return_dict=True)
        self.current_param_vals = {}


        # get the button object
        self.apply_button = self.buttonBox.button(QtGui.QDialogButtonBox.Apply)
        QObject.connect(self.apply_button, SIGNAL("clicked()"), self.regenerate_graph)
        self.populate_params()

    def populate_params(self):
        '''
        fill in parameters will current values
        '''
        self.current_param_vals["fp_show_col1"] = self.show_1.isChecked()
        self.current_param_vals["fp_col1_str"] = unicode(self.col1_str_edit.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_show_col2"] = self.show_2.isChecked()
        self.current_param_vals["fp_col2_str"] = unicode(self.col2_str_edit.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_show_col3"] = self.show_3.isChecked()
        self.current_param_vals["fp_col3_str"] = unicode(self.col3_str_edit.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_show_col4"] = self.show_4.isChecked()
        self.current_param_vals["fp_col4_str"] = unicode(self.col4_str_edit.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_xlabel"] = unicode(self.x_lbl_le.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_outpath"] = unicode(self.image_path.text().toUtf8(), "utf-8")
    
        plot_lb = unicode(self.plot_lb_le.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_plot_lb"] = "[default]"
        if plot_lb != "[default]" and check_plot_bound(plot_lb):
                self.current_param_vals["fp_plot_lb"] = plot_lb

        plot_ub = unicode(self.plot_ub_le.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_plot_ub"] = "[default]"
        if plot_ub != "[default]" and check_plot_bound(plot_ub):
            self.current_param_vals["fp_plot_ub"] = plot_ub

        xticks = unicode(self.x_ticks_le.text().toUtf8(), "utf-8")
        self.current_param_vals["fp_xticks"] = "[default]"
        if xticks != "[default]" and seems_sane(xticks):
            self.current_param_vals["fp_xticks"] = xticks
    
        self.current_param_vals["fp_show_summary_line"] = \
                                self.show_summary_line.isChecked()



    def swap_graphic(self):
        new_pixmap = self.results_window.generate_pixmap(self.png_path)
        self.pixmap_item.setPixmap(new_pixmap)
        print "ok -- plot updated in ui"
        # maybe do something pretty here... ?

    def update_plot(self):
        '''
        update the plot parameters to select the user's
        preferences. also writes these to disk.
        '''
        # map the ui elements to the corresponding
        # parameter names in the plot params list
        ma_specs.add_plot_params(self)

        # load things up in the R side
        meta_py_r.load_vars_for_plot(self.img_params_path)

        # update relevant variables (on the R side)
        # with new values
        meta_py_r.update_plot_params(self.current_param_vals)

        # now re-generate the plot data on the R side of
        # things
        meta_py_r.regenerate_plot_data()


        # finally, actually make the plot and spit it to disk
        self.png_path = self.current_param_vals["fp_outpath"]
        meta_py_r.generate_forest_plot(self.png_path)



    def regenerate_graph(self):
        # this loads the plot.data into R's environment;
        # the variable name will be plot.data
        self.update_plot()
        self.swap_graphic()

        # will need to tell it to 
        #meta_py_r.generate_forest_plot(self.png_path)
        print "OK!"

   
    

