from PyQt4.Qt import *
import ui_edit_forest_plot
import ma_specs
import pdb

class EditPlotWindow(QDialog, ui_edit_forest_plot.Ui_edit_forest_plot_dlg):

    def __init__(self, img_params_name, parent=None):
        super(EditPlotWindow, self).__init__(parent)
        self.setupUi(self)

        # img_params is a string that is the variable
        # name for the R object 
        self.img_params_name = img_params_name
        print "parameters: %s" % self.img_params_name
        self.populate_params()

    def populate_params(self):
        '''
        fill in parameters will current values
        '''
        #pyqtRemoveInputHook()
        #pdb.set_trace()
        pass

    def regenerate_graph(self):
        pass
       
    

