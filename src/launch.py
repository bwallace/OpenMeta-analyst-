import sys, time
from PyQt4 import QtGui
from PyQt4.Qt import *

import meta_py_r
import meta_form
import meta_globals
import settings
from meta_form import DISABLE_NETWORK_STUFF

SPLASH_DISPLAY_TIME = 0 # TODO: change to 5 seconds in production version

def load_R_libraries(app, splash=None):
    ''' Loads the R libraries while updating the splash screen'''
    
    meta_py_r.get_R_libpaths() # print the lib paths
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
    
    if not DISABLE_NETWORK_STUFF:
        splash.showMessage("Loading gemtc\n...................")
        app.processEvents()
        rloader.load_gemtc()

def start():
    app = QtGui.QApplication(sys.argv)
    app.setApplicationName(meta_globals.APPLICATION_NAME)
    app.setOrganizationName(meta_globals.ORGANIZATION_NAME)
    settings.setup_directories()
    
    splash_pixmap = QPixmap(":/misc/splash.png")
    splash = QSplashScreen(splash_pixmap)
    splash.show()
    splash_starttime = time.time()
    
    load_R_libraries(app, splash)
    
    # Show splash screen for at least SPLASH_DISPLAY_TIME seconds
    time_elapsed  = time.time() - splash_starttime
    print("It took %s seconds to load the R libraries" % str(time_elapsed))
    if time_elapsed < SPLASH_DISPLAY_TIME: # seconds
        print("Going to sleep for %f seconds" % float(SPLASH_DISPLAY_TIME-time_elapsed))
        QThread.sleep(int(SPLASH_DISPLAY_TIME-time_elapsed))

    meta = meta_form.MetaForm()
    splash.finish(meta)
    meta.show()
    meta.start()
    sys.exit(app.exec_())

if __name__ == "__main__":
    start()