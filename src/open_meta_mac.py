#------------------------------------------------------------------------------
# Console.py
#   Initialization script for cx_Freeze which manipulates the path so that the
# directory in which the executable is found is searched for extensions but
# no other directory is searched. It also sets the attribute sys.frozen so that
# the Win32 extensions behave as expected.
#------------------------------------------------------------------------------
print("Entering open_meta_mac.py")

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
    print "setting ld path..."
    paths.insert(0, DIR_NAME)
    os.environ["LD_LIBRARY_PATH"] = os.pathsep.join(paths)
    print("Set LD_LIBRARY_PATH to %s" % os.pathsep.join(paths))
    # 8/21/12
    
    #print "*not* setting dydlib"
    print "setting dydlib!!!"
    executable_path = os.pathsep.join(paths)
    print("Executable path is '%s'"% executable_path)
    os.environ["DYLD_LIBRARY_PATH"] = os.pathsep.join(paths)

    # this is probably not necessary now that the above is set but whatever
    print "setting (fallback) dyld path" 
    os.environ["DYLD_FALLBACK_LIBRARY_PATH"] = os.pathsep.join(paths)
    
    os.execv(sys.executable, sys.argv)


sys.frozen = True
sys.path = sys.path[:4]
####
# 8/21/12 -- TEMPORARY -- need to add this back!
#print "\n\nok, mac user -- I'm setting your R path temporarily (this console only).\n\ns"
os.environ["R"] = \
    os.path.join(DIR_NAME, "R_dist", "3.0.2", "bin")

os.environ["R_HOME"] = \
    os.path.join(DIR_NAME, "R_dist", "3.0.2")
    
# For some reason, the X11 shows up in the build on Jens's computer but not on mine when doing diagnostic HSROC
os.environ["R_INTERACTIVE_DEVICE"] = "pdf"
os.environ["R_DEFAULT_DEVICE"] = "pdf"   
    
print("Setting PANGO_RC file path")
pangorc_path = os.path.join(DIR_NAME, "pangorc")
os.environ["PANGO_RC_FILE"] = pangorc_path
print("pango rc path is now: %s" % str(pangorc_path))    

#print("Setting fontconig file path")
#fonts_path = os.path.join(DIR_NAME, "fonts.conf")
#os.environ["FONTCONFIG_FILE"] = fonts_path
#print("fontconfig file path is now: %s", str(fonts_path))

# Very hacky and bad thing but it makes things work...
def create_pangorc():
    '''Creates pangorc file with correct path to pango.modules'''
    
    contents = "[Pango]\nModuleFiles = " + os.path.join(DIR_NAME, "pango.modules")
    f = open(pangorc_path, 'w')
    f.write(contents)
    f.close()
    print("Wrote:\n" + contents + "\n to pangorc")

create_pangorc()

os.environ["TCL_LIBRARY"] = os.path.join(DIR_NAME, "tcl")
os.environ["TK_LIBRARY"] = os.path.join(DIR_NAME, "tk")

#print "INITSCRIPT_ZIP_FILE_NAME:"
#print INITSCRIPT_ZIP_FILE_NAME

#print "DYLD_LIB... {0}".format(os.environ["DYLD_LIBRARY_PATH"])

# commenting this block out on 9/13/12
#m = __import__("__main__")
print "importing??"
importer = zipimport.zipimporter(INITSCRIPT_ZIP_FILE_NAME)
#import meta_form
import launch

#print "1"
#importer = zipimport.zipimporter(INITSCRIPT_ZIP_FILE_NAME)
#code = importer.get_code("meta_form")#m.__name__)
#code = importer.get_code(m.__name__)
#exec code in m.__dict__

versionInfo = sys.version_info[:3]
if versionInfo >= (2, 5, 0) and versionInfo <= (2, 6, 4):
    module = sys.modules.get("threading")
    if module is not None:
        module._shutdown()


'''
OK???
'''
print "starting up..."
launch.start() 

