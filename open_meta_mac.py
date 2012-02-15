#------------------------------------------------------------------------------
# open_meta_mac.py
#
#   this is an initscript used by cx_freeze for our mac
#   distribution. we set the R_HOME variable via the sys 
#   library before starting up meta_form. to use this script
#   just point cx_freeze to it via the initscript argument,
#   eg.,
#    $ build/scripts-2.7/cxfreeze /Users/byronwallace/dev/OpenMeta-analyst-/meta_form.py --init-script=/Users/byronwallace/dev/OpenMeta-analyst-/open_meta_mac.py 
#
#
#   
#------------------------------------------------------------------------------

import os
import sys
import zipimport
import pdb


sys.frozen = True
sys.path = sys.path[:4]
print "\n\nhi there, mac user -- I'm setting your R path temporarily (this console only).\n\ns"
os.environ["R_HOME"] = os.path.join(DIR_NAME, "R_dist", "2.10", "Resources")
print "DOING LESS STUFF"
'''
from ConsoleSetLibPath
'''
'''
paths = os.environ.get("LD_LIBRARY_PATH", "").split(os.pathsep)
if DIR_NAME not in paths:
    paths.insert(0, DIR_NAME)
    os.environ["LD_LIBRARY_PATH"] = os.pathsep.join(paths)
    os.execv(sys.executable, sys.argv)
'''

'''
dyd_paths = os.environ.get("DYLD_LIBRARY_PATH", "").split(os.pathsep)
if DIR_NAME not in dyd_paths:
    #pdb.set_trace()
    #paths.insert(0, DIR_NAME)
    os.environ["DYLD_LIBRARY_PATH"] = os.pathsep.join(dyd_paths)
    if not "meta_form" in sys.executable:
        os.execv(sys.executable, sys.argv)
        print "DYD PATH = %s" % DIR_NAME
    else:
        print "SKIPPING METAFORM"
'''
#os.environ["TCL_LIBRARY"] = os.path.join(DIR_NAME, "tcl")
#os.environ["TK_LIBRARY"] = os.path.join(DIR_NAME, "tk")
'''
m = __import__("__main__")
importer = zipimport.zipimporter(INITSCRIPT_ZIP_FILE_NAME)
code = importer.get_code(m.__name__)
exec code in m.__dict__
'''
       
import meta_form
meta_form.start()

versionInfo = sys.version_info[:3]
if versionInfo >= (2, 5, 0) and versionInfo <= (2, 6, 4):
    module = sys.modules.get("threading")
    if module is not None:
        module._shutdown()



