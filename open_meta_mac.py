#------------------------------------------------------------------------------
# open_meta_mac.py
#
#   this is an initscript used by cx_freeze for our mac
#   distribution. we set the R_HOME variable via the sys 
#   library before starting up meta_form. to use this script
#   just point cx_freeze to it via the initscript argument,
#   eg.,
#    $ build/scripts-2.7/cxfreeze /Users/byronwallace/dev/OpenMeta-analyst-/meta_form.py \
#                 --init-script=/Users/byronwallace/dev/OpenMeta-analyst-/open_meta_mac.py 
#
#   note that we also set the path to 
#------------------------------------------------------------------------------

import os
import sys
import zipimport
import pdb


print "\n\nR.I.P. Steve Jobs. \n\nI'm setting your R path temporarily (this console only).\n\ns"

# issue #160 - setting path to dynamic libraries 

paths = os.environ.get("DYLD_LIBRARY_PATH", "").split(os.pathsep)
if DIR_NAME not in paths:
    paths.insert(0, DIR_NAME)
    os.environ["DYLD_LIBRARY_PATH"] = os.pathsep.join(paths)
    os.execv(sys.executable, sys.argv)

#os.environ["DYLD_LIBRARY_PATH"] = DIR_NAME
#os.execv(sys.executable, sys.argv)
print "dynamic library path set... hold your nose."
#pdb.set_trace()
os.environ["R_HOME"] = os.path.join(DIR_NAME, "R_dist", "2.10", "Resources")

sys.frozen = True
sys.path = sys.path[:4]

# *now* we can import meta_form... cross your fingers.   
#import meta_form
#meta_form.start()
print "\n\nok...?\n\n"
m = __import__("__main__")
importer = zipimport.zipimporter(INITSCRIPT_ZIP_FILE_NAME)
code = importer.get_code(m.__name__)
exec code in m.__dict__

versionInfo = sys.version_info[:3]
if versionInfo >= (2, 5, 0) and versionInfo <= (2, 6, 4):
    module = sys.modules.get("threading")
    if module is not None:
        module._shutdown()





