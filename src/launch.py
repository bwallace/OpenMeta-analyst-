from PyQt4 import QtCore, QtGui, Qt
from PyQt4.Qt import *
import sys
import pdb
import forms.icons_rc

import meta_py_r

def load_R_libraries(app, splash):
    ''' Loads the R libraries while updating the splash screen'''
    
    rloader = meta_py_r.RlibLoader()
    
    splash.showMessage("Loading R libraries\n..")
    app.processEvents()
    
    splash.showMessage("Loading metafor\n....")
    app.processEvents()
    rloader.load_metafor()
    
    splash.showMessage("Loading openmetar\n........")
    app.processEvents()
    rloader.load_openmetar()
    
    splash.showMessage("Loading igraph\n............")
    app.processEvents()
    rloader.load_igraph()
    
    splash.showMessage("Loading grid\n................")
    app.processEvents()
    rloader.load_grid()

def start():
    app = QtGui.QApplication(sys.argv)
    
    splash_pixmap = QPixmap(":/misc/splash.png")
    splash = QSplashScreen(splash_pixmap)
    splash.show()
    app.processEvents()
    
    load_R_libraries(app, splash)
    
    import meta_form
    meta = meta_form.MetaForm()
    meta.show()
    splash.finish(meta)
    sys.exit(app.exec_())

if __name__ == "__main__":
    start()