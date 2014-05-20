#############################################
#                                           #
#  Byron C. Wallace                         #
#  George Dietz                             #
#  CEBM @ Brown                             #
#  This is the code for the ui dialog       #
#  that handles the method selection        #
#  and algorithm specifications             #
#                                           #                                                                            
#  OpenMeta[analyst]                        #
#                                           #
#  This is also where the calls to run      #
#  meta-analyses originate, via a callback  #
#  to the go() routine on meta_form.        #
#                                           #
#############################################

from PyQt4.QtCore import *
from PyQt4.QtGui import *
from PyQt4 import QtCore, QtGui

import copy

import forms.ui_ma_specs
#import meta_py_r
from meta_globals import *
from settings import *
import diagnostic_explain

###
# ack.. string encoding messiness
try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s
    

class MA_Specs(QDialog, forms.ui_ma_specs.Ui_Dialog):

    def __init__(self, model, parent=None, meta_f_str=None,
                 external_params=None, diag_metrics=None,
                 diag_metrics_to_analysis_details_d=None,
                 fp_specs_only=False, conf_level=None):

        super(MA_Specs, self).__init__(parent)
        self.setupUi(self)

        self.current_param_vals = external_params or {}
        self.model = model
        
        if conf_level is None:
            raise ValueError("CONFIDENCE LEVEL MUST BE SPECIFIED")
        self.conf_level = conf_level

        # if not none, we assume we're running a meta
        # method 
        self.meta_f_str = meta_f_str

        QObject.connect(self.buttonBox, SIGNAL("accepted()"), self.run_ma)
        QObject.connect(self.buttonBox, SIGNAL("rejected()"), self.cancel)
        QObject.connect(self.save_btn, SIGNAL("pressed()"), self.select_out_path)
        QObject.connect(self.method_cbo_box, SIGNAL("currentIndexChanged(QString)"),
                                             self.method_changed)

        self.data_type = self.model.get_current_outcome_type()
        print "data type: %s" % self.data_type
        if self.meta_f_str is not None:
            # we pre-prend the data type to the meta-method function
            # name. thus the caller (meta_form) needn't worry about
            # the data type, only about the method name (e.g., cumulative)
            if not self.meta_f_str.endswith(self.data_type): 
                self.meta_f_str = ".".join((self.meta_f_str, self.data_type))
            
        
        if self.data_type != "binary":
            self.disable_bin_only_fields()
            if self.data_type == "diagnostic":
                self.enable_diagnostic_fields()

        # disable second arm display for one-arm analyses
        if self.model.current_effect in ONE_ARM_METRICS:
            self.setup_fields_for_one_arm()    
        

        self.current_widgets = []
        self.current_method = None
        self.current_params = None
        self.current_defaults = None
        self.var_order = None
        
        ####
        # the following are variables for the case of diagnostic 
        # data. in other cases, these are meaningless/None.
        # diagnostic data is special because we allow the user to
        # to run analyses on multiple metrics at once.
        #
        # this dictionary maps metrics to analysis methods and
        # corresponding parameters. e.g., 
        #   diag_metrics["sens"] -> (method, parameters)
        # for each metric selected by the user. note that
        # the method and parameters will in fact be the same for 
        # sens/spec (and for lr/dor), but we map the metrics
        # to their own tuples for convenience.
        self.diag_metrics_to_analysis_details = \
                        diag_metrics_to_analysis_details_d or {}

        # note that we assume the metrics for which analysis
        # details have already been acquired (i.e,. those in 
        # the above dictionary) are not included in the diag_metrics
        # list -- we do not explicitly check for this here.
        self.diag_metrics = diag_metrics 

        # diagnostic data requires a different UI because multiple
        # metrics can be selected. we handle this by allowing the 
        # user to specify different methods for different groups
        # of metrics.
        if self.diag_metrics is not None:
            # these are the two 'groups' of metrics (sens/spec & DOR/LR+/-)
            # these booleans tell us for which of these groups we're getting parameters
            self.sens_spec = any([m in ("sens", "spec") for m in self.diag_metrics])
            self.lr_dor = any([m in ("lr", "dor") for m in self.diag_metrics])
            self.setup_diagnostic_ui()

        self.populate_cbo_box()

    def cancel(self):
        print "(cancel)"
        self.reject()

    def select_out_path(self):
        out_f = "."
        out_f = unicode(QFileDialog.getSaveFileName(self, "OpenMeta[analyst] - Plot Path",
                                                    out_f, "png image files: (.png)"))
        if out_f == "" or out_f == None:
            return None
        else:
            self.image_path.setText(out_f)
        
    def make_indefinite_progress_bar(self):
        bar = QtGui.QProgressBar(parent=self)
        bar.setWindowTitle("running analysis...")
        bar.setRange(0, 0) # makes it indefinite
        #specs_form_pos = self.pos()
        #specs_form_width, specs_form_height = self.width(), self.height()

        bar_width = 250 # seems to look ok?
        bar.setFixedWidth(bar_width)
        #center_width = specs_form_pos.x() + specs_form_width/2 - bar_width/2
        #center_height = specs_form_pos.y() + specs_form_height/2

        bar.setAlignment(Qt.AlignCenter)
        #center_point = QPoint(center_width, center_height)
        
        #bar.move(center_point)
    
        return bar
    
    def run_network_analysis(self):
        # first, let's fire up a progress bar
        bar = MetaProgress(self)
        bar.show()
        result = None
        
        if self.data_type == "binary":
            data_type = BINARY
        
        if self.data_type not in ["binary","continuous"]:
            raise ValueError("Network Analysis can currently only be done with binary or continuous data")
        
        meta_py_r.ma_dataset_to_simple_network(table_model=self.model,
                                     var_name="tmp_obj",
                                     data_type=None,
                                     outcome=None,
                                     follow_up=None,
                                     network_path='./r_tmp/network.png')

    def run_ma(self):
        ###
        # first, let's fire up a progress bar
        bar = MetaProgress(self)
        bar.show()
        result = None
        
        # this method is defined statically, below
        add_plot_params(self)

        # also add the metric to the parameters
        # -- this is for scaling

        if not self.data_type == "diagnostic":
            self.current_param_vals["measure"] = self.model.current_effect 
        
        # dispatch on type; build an R object, then run the analysis
        if self.data_type == "binary":
            # note that this call creates a tmp object in R called
            # tmp_obj (though you can pass in whatever var name
            # you'd like)
            meta_py_r.ma_dataset_to_simple_binary_robj(self.model)
            if self.meta_f_str is None:
                result = meta_py_r.run_binary_ma(self.current_method, self.current_param_vals)
                #pass
            else:
                result = meta_py_r.run_meta_method(self.meta_f_str, self.current_method, self.current_param_vals)
                
            #_writeout_test_data(self.meta_f_str, self.current_method, self.current_param_vals, result) # FOR MAKING TESTS
        elif self.data_type == "continuous":
            meta_py_r.ma_dataset_to_simple_continuous_robj(self.model)
            if self.meta_f_str is None:
                # run standard meta-analysis
                result = meta_py_r.run_continuous_ma(self.current_method, self.current_param_vals)
            else:
                # get meta!
                result = meta_py_r.run_meta_method(self.meta_f_str, self.current_method, self.current_param_vals)
            
            #_writeout_test_data(self.meta_f_str, self.current_method, self.current_param_vals, result) # FOR MAKING TESTS
        elif self.data_type == "diagnostic":
            # add the current metrics (e.g., PLR, etc.) to the method/params
            # dictionary
            self.add_cur_analysis_details()
        
            method_names, list_of_param_vals = [], []

            if len(self.diag_metrics_to_analysis_details) == 0:
                self.add_cur_analysis_details()


            ordered_metrics = ["Sens", "Spec", "NLR", "PLR", "DOR"]
            for diag_metric in \
                  [metric for metric in ordered_metrics \
                    if metric in self.diag_metrics_to_analysis_details]:
                # pull out the method and parameters object specified for this
                # metric.
                method, param_vals = self.diag_metrics_to_analysis_details[diag_metric]
                param_vals = copy.deepcopy(param_vals)

                
                # update the forest plot path
                split_fp_path = self.current_param_vals["fp_outpath"].split(".")
                new_str = split_fp_path[0] if len(split_fp_path) == 1 else \
                          ".".join(split_fp_path[:-1])
                new_str = new_str + "_%s" % diag_metric.lower() + ".png"
                param_vals["fp_outpath"] = new_str

                # update the metric 
                param_vals["measure"] = diag_metric
                
                method_names.append(method)
                list_of_param_vals.append(param_vals)
            
            # create the DiagnosticData object on the R side -- this is going 
            # to be the same for all analyses
            meta_py_r.ma_dataset_to_simple_diagnostic_robj(self.model)

            if self.meta_f_str is None:
                # regular meta-analysis
                try:
                    result = meta_py_r.run_diagnostic_multi(method_names, list_of_param_vals)
                except Exception, e:
                    error_message = \
                        "sorry, something has gone wrong with your analysis. here is a stack trace that probably won't be terribly useful.\n %s"  \
                                            % e
                
                    QMessageBox.critical(self, "analysis failed", error_message)
                    bar.hide()
                    # reset Rs working directory 
                    meta_py_r.reset_Rs_working_dir()
                    self.accept()
                #_writeout_test_data(self.meta_f_str, method_names, list_of_param_vals, result, diag=True) # FOR MAKING TESTS
            else:
                # in the case of diagnostic, we pass in lists
                # of param values to the meta_method 
                result = meta_py_r.run_meta_method_diag(
                                self.meta_f_str, method_names, list_of_param_vals)
                #_writeout_test_data(self.meta_f_str, method_names, list_of_param_vals, result, diag=True) # FOR MAKING TESTS

        bar.hide()

        self.parent().analysis(result)
        self.accept()

    def enable_diagnostic_fields(self):
        #self.col3_str_edit.setEnabled(True)
        self.col3_str_edit.setText("[default]")
        self.show_3.setEnabled(True)
        self.show_3.setChecked(True)

    def disable_bin_only_fields(self):
        self.col3_str_edit.setEnabled(False)
        self.col4_str_edit.setEnabled(False)
        self.show_3.setChecked(False)
        self.show_3.setEnabled(False)
        self.show_4.setChecked(False)
        self.show_4.setEnabled(False)

    def setup_fields_for_one_arm(self):
        self.show_4.setChecked(False)
        self.show_4.setEnabled(False)

    def method_changed(self):
        if self.parameter_grp_box.layout() is not None:
            print("Layout items count before: %d" % self.parameter_grp_box.layout().count())
        self.clear_param_ui()
        self.current_widgets= []
        self.current_method = self.available_method_d[str(self.method_cbo_box.currentText())]
        self.setup_params()
        self.parameter_grp_box.setTitle(self.current_method)
        self.ui_for_params()

    def populate_cbo_box(self, cbo_box=None, param_box=None):
        # if no combo box is passed in, use the default 'method_cbo_box'
        if cbo_box is None:
            cbo_box = self.method_cbo_box
            param_box = self.parameter_grp_box

        # we first build an R object with the current data. this is to pass off         
        # to the R side to check the feasibility of the methods over the current data.
        # i.e., we do not display methods that cannot be performed over the 
        # current data.
        tmp_obj_name = "tmp_obj" 
        if self.data_type == "binary":
            meta_py_r.ma_dataset_to_simple_binary_robj(self.model, var_name=tmp_obj_name)
        elif self.data_type == "continuous":
            meta_py_r.ma_dataset_to_simple_continuous_robj(self.model, var_name=tmp_obj_name)
        elif self.data_type == "diagnostic":
            meta_py_r.ma_dataset_to_simple_diagnostic_robj(self.model, var_name=tmp_obj_name)

        
        self.available_method_d = None
        ###
        # in the case of diagnostic data, the is.feasible methods need also to know
        # which metric the analysis is to be run over. here we check this. note that
        # we're setting params for multiple metrics (e.g., sens./spec.) but the is.feasible
        # method only wants *one* metric. thus we arbitrarily select one or the other --
        # this is tacitly assuming that the *same* methods are feasible for, say, sens.
        # as for spec. this is a reasonable assumption is all cases of which I'm aware, 
        # but a more conservative/correct thing to do would be to pass in the *most restrictive*
        # metric to the _get_available_methods_routine
        ###
        metric = self.model.current_effect 
        if self.data_type == "diagnostic":
            if self.meta_f_str is None:
                metric = "Sens" if self.sens_spec else "DOR"
            else:
                metric = "Sens"

        
        self.available_method_d = meta_py_r.get_available_methods(for_data_type=self.data_type,\
                                         data_obj_name=tmp_obj_name, metric=metric)

        print "\n\navailable %s methods: %s" % (self.data_type, ", ".join(self.available_method_d.keys()))
        
        
        #print("----------------------------------\nAvailable methods dictionary:",self.available_method_d)

        # issue #110 -- this is NOT a general/good/flexible solution
        # -- we sort here in reverse because this will put .random
        # first. otherwise, the default is that R provides the functions
        # in alphabetical (ascending). 
        method_names = self.available_method_d.keys()
       

        ###
        # removing bivariate (sens/spec) methods when it makes no sense
        # @TODO handle this better
        if self.data_type == "diagnostic":
            biv_ml_name = "Bivariate (Maximum Likelihood)"
            for biv_method in (biv_ml_name, "HSROC"):
                if metric != "Sens" and biv_method in method_names or\
                         self.meta_f_str is not None or\
                         not ("sens" in self.diag_metrics and "spec" in self.diag_metrics):
                    method_names.remove(biv_method)
            # Fix for issue # 175            
            if set(["lr","dor"]) <= set(self.diag_metrics):    
                try:
                    method_names.remove('Diagnostic Fixed-Effect Peto')
                    #QMessageBox.warning(self.parent(), "whoops", "removed peto")
                except:
                    print("Couldn't remove 'Diagnostic Fixed-Effect Peto' for some reason... don't know why")
        
        method_names.sort(reverse=True)

        ###
        # default to bivariate method for diagnostic
        if self.data_type == "diagnostic" and biv_ml_name in method_names:
            method_names.remove(biv_ml_name)
            method_names.insert(0, biv_ml_name)
 
        for method in method_names:
            cbo_box.addItem(method)
        self.current_method = self.available_method_d[str(cbo_box.currentText())]
        self.setup_params()
        param_box.setTitle(self.current_method)


    def clear_param_ui(self):
        for widget in self.current_widgets:
            widget.deleteLater()
            widget = None
        


    def ui_for_params(self):
        if self.parameter_grp_box.layout() is None:
            layout = QGridLayout()
            self.parameter_grp_box.setLayout(layout)

        cur_grid_row = 0
        
        # add the method description
        method_description = meta_py_r.get_method_description(self.current_method)
        
        self.add_label(self.parameter_grp_box.layout(), cur_grid_row, \
                            "Description: %s" % method_description)
        cur_grid_row += 1
        

        if self.var_order is not None:
            for var_name in self.var_order:
                val = self.current_params[var_name]
                self.add_param(self.parameter_grp_box.layout(), cur_grid_row, var_name, val)
                cur_grid_row+=1
        else:
            # no ordering was provided; let's try and do something
            # sane with respect to the order in which parameters
            # are displayed.
            #
            # we want to add the parameters in groups, for example,
            # we add combo boxes (which will be lists of values) together,
            # followed by numerical inputs. thus we create an ordered list
            # of functions to check if the argument is the corresponding
            # type (float, list); if it is, we add it otherwise we pass. this isn't
            # the most efficient way to do things, but the number of parameters
            # is going to be relatively tiny anyway
            ordered_types = [lambda x: isinstance(x, list),
                             lambda x: isinstance(x, str) and x.lower()=="float"]

            for is_right_type in ordered_types:
                for key, val in self.current_params.items():
                    if is_right_type(val):
                        self.add_param(self.parameter_grp_box.layout(), cur_grid_row, key, val)
                        cur_grid_row+=1

        # do we need to set forest plot parameters? if not,
        # e.g., in the case of HSROC or other methdos that
        # don't use our forest plotting, we don't show the
        # corresponding tab for forest plot params.
        # @TODO this is hacky; plus, really we should keep
        # a list of methods that *DO* take forest plot params
        if self.current_method in METHODS_WITH_NO_FOREST_PLOT:
            self.plot_tab.setEnabled(False)
        else:
            self.plot_tab.setEnabled(True)
                        
    def add_param(self, layout, cur_grid_row, name, value):
        print "adding param. name: %s, value: %s" % (name, value)
        if isinstance(value, list):
            # then it's an enumeration of values
            self.add_enum(layout, cur_grid_row, name, value)
        elif value.lower() == "float":
            self.add_float_box(layout, cur_grid_row, name)
        elif value.lower() == "int":
            self.add_int_box(layout, cur_grid_row, name)
        # should we add an array type?
        elif value.lower() == "string":
            self.add_text_box(layout, cur_grid_row, name)
        else:
            print "unknown type! throwing up. bleccch."
            print "name:%s. value: %s" % (name, value)
            # throw exception here

    def add_enum(self, layout, cur_grid_row, name, values):
        '''
        Adds an enumeration to the UI, with the name and possible
        values as specified per the parameters.
        '''
       
        ### 
        # using the pretty name for the label now.
        self.add_label(layout, cur_grid_row, self.param_d[name]["pretty.name"], \
                                tool_tip_text=self.param_d[name]["description"])
        cbo_box = QComboBox()
        for index, value in enumerate(values):
            name_str = self._get_enum_item_pretty_name(name,value)
            cbo_box.addItem(name_str)  # TODO: replace value with pretty values
            cbo_box.setItemData(index, QVariant(value))

        if self.current_defaults.has_key(name):
            cbo_box.setCurrentIndex(cbo_box.findData(self.current_defaults[name]))
            self.current_param_vals[name] = self.current_defaults[name]

        QObject.connect(cbo_box, QtCore.SIGNAL("currentIndexChanged(int)"),
                                 self.set_param_f_from_itemdata(name))

        self.current_widgets.append(cbo_box)
        layout.addWidget(cbo_box, cur_grid_row, 1)
        
    def _get_enum_item_pretty_name(self, enum_name, item_name):
        if "rm.method.names" in self.param_d[enum_name]:
            if item_name in self.param_d[enum_name]["rm.method.names"]:
                return item_name + ": " + str(self.param_d[enum_name]["rm.method.names"][item_name])
        return item_name
    
    @QtCore.pyqtSlot()
    def set_param_f_from_itemdata(self, name, to_type=str):
        '''
        hackier version....
        Returns a function f(x) such that f(x) will set the key
        name in the parameters dictionary to the value x.
        '''
        
        def set_param(index):
            combo_box = self.sender()
            x = combo_box.itemData(index).toString()
            self.current_param_vals[name] = to_type(x)
            print str(self.current_param_vals) + " -> weirdo sender thing"

        return set_param

    def add_float_box(self, layout, cur_grid_row, name):
        self.add_label(layout, cur_grid_row, self.param_d[name]["pretty.name"],\
                                tool_tip_text=self.param_d[name]["description"])
        # now add the float input line edit
        finput = QLineEdit()

        # if a default value has been specified, use it
        if self.current_defaults.has_key(name):
            finput.setText(str(self.current_defaults[name]))
            self.current_param_vals[name] = self.current_defaults[name]


        finput.setMaximumWidth(50)
        QObject.connect(finput, QtCore.SIGNAL("textChanged(QString)"),
                                 self.set_param_f(name, to_type=float))
        self.current_widgets.append(finput)
        layout.addWidget(finput, cur_grid_row, 1)
        
    def add_int_box(self, layout, cur_grid_row, name):
        self.add_label(layout, cur_grid_row, self.param_d[name]["pretty.name"],\
                                tool_tip_text=self.param_d[name]["description"])
        # now add the int input line edit
        iinput = QLineEdit()

        # if a default value has been specified, use it
        if self.current_defaults.has_key(name):
            iinput.setText(str(int(self.current_defaults[name])))
            self.current_param_vals[name] = self.current_defaults[name]

        iinput.setMaximumWidth(50)
        QObject.connect(iinput, QtCore.SIGNAL("textChanged(QString)"),
                                 self.set_param_f(name, to_type=int))
        self.current_widgets.append(iinput)
        layout.addWidget(iinput, cur_grid_row, 1)

    def add_text_box(self, layout, cur_grid_row, name):
        self.add_label(layout, cur_grid_row, self.param_d[name]["pretty.name"],\
                                tool_tip_text=self.param_d[name]["description"])
        # now add the text
        txt_input = QLineEdit()

        # if a default value has been specified, use it
        if self.current_defaults.has_key(name):
            txt_input.setText(str(self.current_defaults[name]))
            self.current_param_vals[name] = self.current_defaults[name]

        txt_input.setMaximumWidth(200)
        QObject.connect(txt_input, QtCore.SIGNAL("textChanged(QString)"),
                                 self.set_param_f(name, to_type=float))
        self.current_widgets.append(txt_input)
        layout.addWidget(txt_input, cur_grid_row, 1)


    def set_param_f(self, name, to_type=str):
        '''
        Returns a function f(x) such that f(x) will set the key
        name in the parameters dictionary to the value x.
        '''
        def set_param(x):
            self.current_param_vals[name] = to_type(x)
            print self.current_param_vals

        return set_param

    def add_label(self, layout, cur_grid_row, name, tool_tip_text=None):
        lbl = QLabel(name, self.parameter_grp_box)
        if not tool_tip_text is None:
            lbl.setToolTip(tool_tip_text)
        self.current_widgets.append(lbl)
        layout.addWidget(lbl, cur_grid_row, 0)

    def setup_params(self):
        # parses out information about the parameters of the current method
        # param_d holds (meta) information about the parameter -- it's a each param
        # itself maps to a dictionary with a pretty name and description (assuming
        # they were provided for the given param)
        self.current_params, self.current_defaults, self.var_order, self.param_d = \
                    meta_py_r.get_params(self.current_method)

        ###
        # user selections overwrite the current parameter defaults.
        # ie., if the user has run this analysis before, the preferences
        # they selected then are automatically set as the defaults now.
        # these defaults, if they exist, are stored in the user_preferences 
        # dictionary
#         method_params = self.parent().user_prefs["method_params"]
#         if self.current_method in method_params:
#             print "loading default from user preferences!"
#             self.current_defaults = method_params[self.current_method]
            
        # override conf.level with global conf.level
        self.current_defaults['conf.level'] = self.conf_level

        print self.current_defaults


    def diag_next(self):
        add_plot_params(self)

        # if the user selected both sens/spec and lr/dor
        # then we will always show them the former first.
        # thus the parameters we have now are for sens/spec
        # note that we first add the forest plot parameters!
        self.add_cur_analysis_details()

        
        # we're going to show another analysis details form for the
        # likelihood ratio and diagnostic odds ratio analyses.
        # we pass along the parameters acquired for sens/spec
        # in the diag_metrics* dictionary.
        form =  MA_Specs(self.model, parent=self.parent(),
                    diag_metrics=list(set(["lr", "dor"]) & set(self.diag_metrics)),
                    meta_f_str = self.meta_f_str,
                    diag_metrics_to_analysis_details_d=self.diag_metrics_to_analysis_details,
                    conf_level=self.model.get_global_conf_level())
        form.show()
        self.hide()


    def add_cur_analysis_details(self):
        ''' 
        this method only applicable for diagnostic data, wherein
        we have multiple metrics. here the parameters/method for 
        these metrics are added to a dictionary.
        '''

        # this was extracted earlier, ultimately from the checkboxes
        # selected by the user
        metrics_to_run = self.diag_metrics_to_analysis_details.keys()

        if self.sens_spec:
            for metric in [m for m in ("Sens", "Spec") if m in metrics_to_run]:
                self.diag_metrics_to_analysis_details[metric] = \
                        (self.current_method, self.current_param_vals)
        else:
            # lr/dor
            for metric in [m for m in ("DOR", "PLR", "NLR") if m in metrics_to_run]:
                self.diag_metrics_to_analysis_details[metric] = \
                        (self.current_method, self.current_param_vals)



    def setup_diagnostic_ui(self):
        ###
        # here is where we set the keys of the _to_analysis_details
        # dictionary to the metrics the user selected
        
        # the names of the metrics internally are different than those that 
        # are displayed to the user, e.g., the user selects Likelihood Ratio
        # instead of LR+/LR-. we map from the UI names to the internal names
        # here
        ####

        # lr_dor is, by convention, the 'second screen' shown to the user
        if len(self.diag_metrics_to_analysis_details) == 0:
            metrics_to_run = []
            for m in self.diag_metrics:
                metrics_to_run.extend(DIAG_METRIC_NAMES_D[m])

            self.diag_metrics_to_analysis_details = \
                dict(zip(metrics_to_run, [None for m in metrics_to_run]))
                
        if self.sens_spec and self.lr_dor:
            self.buttonBox.clear()    
            next_button = self.buttonBox.addButton(QString("next >"), 0)


            # if both sets of metrics are selected, we need to next prompt the
            # user for parameters regarding the second
            QObject.disconnect(self.buttonBox, SIGNAL("accepted()"), self.run_ma)
            QObject.connect(self.buttonBox, SIGNAL("accepted()"), self.diag_next)

            # in the case that both 'families' of metrics are selected,
            # we prompt the user for two different methods (because different
            # methods are available for these). this is confusing, so
            # we'll here tell the user what's up (issue #83, issue #115)
            # (but we only show it if they haven't disabled this)

            if get_setting("explain_diag"):
                diag_explain_window = diagnostic_explain.DiagnosticExplain(parent=self)
                diag_explain_window.show()

        # change some UI elements to refelct the current method
        window_title, method_label = "", ""
        if self.sens_spec:
            window_title = "Method & Parameters for Sens./Spec."
            method_label = "method for sens./spec."
        else:
            window_title = "Method & Parameters for DOR/LR"
            method_label = "method for DOR/LR"

        self.setWindowTitle(QtGui.QApplication.translate("Dialog", window_title, \
                None, QtGui.QApplication.UnicodeUTF8))
        self.method_lbl.setText(method_label)
        
###
# the following methods are defined statically because
# they are also used by the forest plot editing window,
# which isn't really a 'child' of ma_specs, so inheritance
# didn't feel appropriate
###
def add_plot_params(specs_form):
    specs_form.current_param_vals["fp_show_col1"] = specs_form.show_1.isChecked()
    specs_form.current_param_vals["fp_col1_str"] = unicode(specs_form.col1_str_edit.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_show_col2"] = specs_form.show_2.isChecked()
    specs_form.current_param_vals["fp_col2_str"] = unicode(specs_form.col2_str_edit.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_show_col3"] = specs_form.show_3.isChecked()
    specs_form.current_param_vals["fp_col3_str"] = unicode(specs_form.col3_str_edit.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_show_col4"] = specs_form.show_4.isChecked()
    specs_form.current_param_vals["fp_col4_str"] = unicode(specs_form.col4_str_edit.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_xlabel"] = unicode(specs_form.x_lbl_le.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_outpath"] = unicode(specs_form.image_path.text().toUtf8(), "utf-8")
    
    plot_lb = unicode(specs_form.plot_lb_le.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_plot_lb"] = "[default]"
    if plot_lb != "[default]" and check_plot_bound(plot_lb):
        specs_form.current_param_vals["fp_plot_lb"] = plot_lb

    plot_ub = unicode(specs_form.plot_ub_le.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_plot_ub"] = "[default]"
    if plot_ub != "[default]" and check_plot_bound(plot_ub):
        specs_form.current_param_vals["fp_plot_ub"] = plot_ub

    xticks = unicode(specs_form.x_ticks_le.text().toUtf8(), "utf-8")
    specs_form.current_param_vals["fp_xticks"] = "[default]"
    if xticks != "[default]" and seems_sane(xticks):
        specs_form.current_param_vals["fp_xticks"] = xticks
    
    specs_form.current_param_vals["fp_show_summary_line"] = specs_form.show_summary_line.isChecked()


def _writeout_test_data(meta_f_str, method, params, results, diag=False):
    with open('test_data.txt', 'a') as f:
        f.write("method_and_params.append({\n")
        f.write("                          'meta_f_str': '%s',\n" % meta_f_str)
        if diag:
            f.write("                          'method': %s,\n" % str(method))
        else:
            f.write("                          'method': '%s',\n" % method)
        f.write("                          'parameters': %s,\n" % str(params))
        f.write("                          'results': %s,\n" % str(results))
        f.write("                         })\n\n")
        
        # Write the data to the disk for sure
        f.flush()
        os.fsync(f)
        
        
#    method_and_params.append({'meta_f_str': u'loo.ma.binary',
#                              'method'    : 'binary.random',
#                              'parameters': {'conf.level': 95.0, 'digits': 3.0, 'fp_col2_str': u'[default]', 'fp_show_col4': True, 'to': 'only0', 'fp_col4_str': u'Ev/Ctrl', 'fp_xticks': '[default]', 'fp_col3_str': u'Ev/Trt', 'fp_show_col3': True, 'fp_show_col2': True, 'fp_show_col1': True, 'fp_plot_lb': '[default]', 'fp_outpath': u'./r_tmp/forest.png', 'rm.method': 'DL', 'adjust': 0.5, 'fp_plot_ub': '[default]', 'fp_col1_str': u'Studies', 'measure': 'OR', 'fp_xlabel': u'[default]', 'fp_show_summary_line': True},
#                              'results'   : {'images': {'Leave-one-out Forest plot': './r_tmp/forest.png'}, 'texts': {'Leave-one-out Summary': 'Binary Random-Effects Model\n\nMetric: Odds Ratio\n\n Model Results\n\n Studies           Estimate   Lower bound   Upper bound   Std. error   p-Val  \n\n Overall             0.770       0.485         1.222         0.236     0.267  \n\n - Gonzalez          0.783       0.477         1.285         0.253     0.332  \n\n - Prins             0.796       0.489         1.296         0.249     0.359  \n\n - Giamarellou       0.811       0.513         1.281         0.233     0.369  \n\n - Maller            0.715       0.437         1.172         0.252     0.184  \n\n - Sturm             0.791       0.494         1.265         0.240     0.327  \n\n - Marik             0.864       0.543         1.375         0.237     0.538  \n\n - Muijsken          0.724       0.446         1.175         0.247     0.191  \n\n - Vigano            0.750       0.468         1.200         0.240     0.230  \n\n - Hansen            0.851       0.542         1.336         0.230     0.484  \n\n - De Vries          0.724       0.449         1.166         0.243     0.184  \n\n - Mauracher         0.813       0.515         1.283         0.233     0.375  \n\n - Nordstrom         0.772       0.474         1.257         0.249     0.298  \n\n - Rozdzinski        0.738       0.442         1.231         0.261     0.244  \n\n - Ter Braak         0.726       0.443         1.189         0.252     0.204  \n\n - Tulkens           0.764       0.476         1.227         0.242     0.266  \n\n - Van der Auwera    0.791       0.494         1.265         0.240     0.327  \n\n - Klastersky        0.696       0.458         1.059         0.214     0.091  \n\n - Vanhaeverbeek     0.764       0.476         1.226         0.242     0.265  \n\n - Hollender         0.780       0.486         1.253         0.241     0.304  \n\n\n'}, 'image_var_names': {'loo forest plot': 'loo_forest_plot'}, 'image_params_paths': {'Forest Plot': 'r_tmp/1371578321.65222'}, 'image_order': None},
#                      })


####
# simple progress bar
import forms.ui_running
class MetaProgress(QDialog, forms.ui_running.Ui_running):
    
    def __init__(self, parent=None):
        super(MetaProgress, self).__init__(parent)
        self.setupUi(self)