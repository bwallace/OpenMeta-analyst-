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
        #pyqtRemoveInputHook()
        #pdb.set_trace()

        # get the button object
        self.apply_button = self.buttonBox.button(QtGui.QDialogButtonBox.Apply)
        QObject.connect(self.apply_button, SIGNAL("clicked()"), self.update_plot)
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


    def update_plot(self):
        self.regenerate_graph()
        new_pixmap = self.results_window.generate_pixmap(self.png_path)
        print "plot updated in ui"
        #pyqtRemoveInputHook()
        #pdb.set_trace()


    def update_plot_params(self):
        '''
        update the plot parameters to select the user's
        preferences. also writes these to disk.
        '''
        ma_specs.add_plot_params(self)
        print "parameters updated - here they are:"
        print self.current_param_vals
        print "setting them on the R side."
        meta_py_r.set_plot_data(self.current_param_vals)

    def regenerate_graph(self):
        # this loads the plot.data into R's environment;
        # the variable name will be plot.data
        meta_py_r.load_plot_params(self.img_params_path)
        self.update_plot_params()

        # will need to tell it to 
        meta_py_r.generate_forest_plot(self.png_path)
        print "OK!"
   
    

