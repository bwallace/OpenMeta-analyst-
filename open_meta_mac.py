#------------------------------------------------------------------------------
# Console.py
#   Initialization script for cx_Freeze which manipulates the path so that the
# directory in which the executable is found is searched for extensions but
# no other directory is searched. It also sets the attribute sys.frozen so that
# the Win32 extensions behave as expected.
#------------------------------------------------------------------------------
import encodings
import os
import sys
import zipimport
import pdb


'''
from ConsoleSetLibPath
'''

paths = os.environ.get("LD_LIBRARY_PATH", "").split(os.pathsep)
if DIR_NAME not in paths:
    paths.insert(0, DIR_NAME)
    os.environ["LD_LIBRARY_PATH"] = os.pathsep.join(paths)
    # 8/21/12
    #os.environ["DYLD_LIBRARY_PATH"] = os.pathsep.join(paths)
    os.execv(sys.executable, sys.argv)

print "*not* setting dydlib"

sys.frozen = True
sys.path = sys.path[:4]
####
# 8/21/12 -- TEMPORARY -- need to add this back!
#print "\n\nok, mac user -- I'm setting your R path temporarily (this console only).\n\ns"
#os.environ["R"] = os.path.join(DIR_NAME, "R_dist", "2.10", "Resources", "bin")


os.environ["TCL_LIBRARY"] = os.path.join(DIR_NAME, "tcl")
os.environ["TK_LIBRARY"] = os.path.join(DIR_NAME, "tk")

m = __import__("__main__")
importer = zipimport.zipimporter(INITSCRIPT_ZIP_FILE_NAME)
code = importer.get_code(m.__name__)
exec code in m.__dict__

versionInfo = sys.version_info[:3]
if versionInfo >= (2, 5, 0) and versionInfo <= (2, 6, 4):
    module = sys.modules.get("threading")
    if module is not None:
        module._shutdown()


'''
OK
'''
print "\n\nYEAH"
import meta_form
meta_form.start()