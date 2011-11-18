from PyQt4.Qt import *
import ui_edit_forest_plot
import ma_specs
import meta_py_r
import pdb

class EditPlotWindow(QDialog, ui_edit_forest_plot.Ui_edit_forest_plot_dlg):

    def __init__(self, img_params_name, png_path, parent=None):
        super(EditPlotWindow, self).__init__(parent)
        self.setupUi(self)

        # img_params is a string that is the variable
        # name for the R object 
        self.img_params_name = img_params_name
        print "parameters: %s" % self.img_params_name

        self.png_path = png_path

        # the handle to the window in which
        # the image is being displayed
        self.results_window = parent

        self.populate_params()

    def populate_params(self):
        '''
        fill in parameters will current values
        '''
        #pyqtRemoveInputHook()
        #pdb.set_trace()
        pass

    def update_plot_params(self):
        '''
        update the plot parameters to select the user's
        preferences. also writes these to disk.
        '''
        pass

    def regenerate_graph(self):
        # this loads the plot.data into R's environment;
        # the variable name will be plot.data
        meta_py_r.load_plot_params(os.path.join("r_tmp", self.img_params_name))
        self.update_plot_params()
        meta_py_r.generate_forest_plot(png_path)

   
    

