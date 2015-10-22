# NOTE: THIS SCRIPT is adapted FROM site-packages/cx_Freeze/initscripts/Console.py
#------------------------------------------------------------------------------
# Console.py
#   Initialization script for cx_Freeze which manipulates the path so that the
# directory in which the executable is found is searched for extensions but
# no other directory is searched. It also sets the attribute sys.frozen so that
# the Win32 extensions behave as expected.
#------------------------------------------------------------------------------

def set_rpy2_env_variables():
    print ("DIR_NAME: %s" % DIR_NAME)

    # assumes R is in the Contents/Frameworks directory of the app
    #contents_app_dir = os.path.dirname(DIR_NAME)
    #R_HOME_DIR = os.path.join(contents_app_dir,'Frameworks','R.framework','Resources')
    R_HOME_DIR = os.path.join(DIR_NAME,'R')
    
    renv_var = { # environment variable settings
        'R_HOME': R_HOME_DIR,
        'R_SHARE_DIR': os.path.join(R_HOME_DIR,'share'),
        'R_INCLUDE_DIR': os.path.join(R_HOME_DIR,'include'),
        'R_DOC_DIR': os.path.join(R_HOME_DIR,'doc'),
        'R_ARCH': '',
    }
    preferred_keyorder = ['R_HOME', 'R_SHARE_DIR', 'R_INCLUDE_DIR', 'R_DOC_DIR', 'R_ARCH']
    print("Setting rpy2/R environment variables as follows: %s" % renv_var)

    for environment_variable,value in renv_var.items():
        os.environ[environment_variable] = value

    for environment_variable in renv_var.keys():
        print("%s is now %s" % (environment_variable,os.environ[environment_variable]))


import os
import sys
import zipimport

sys.frozen = True
sys.path = sys.path[:4]

os.environ["TCL_LIBRARY"] = os.path.join(DIR_NAME, "tcl")
os.environ["TK_LIBRARY"] = os.path.join(DIR_NAME, "tk")
set_rpy2_env_variables()

m = __import__("__main__")
importer = zipimport.zipimporter(INITSCRIPT_ZIP_FILE_NAME)
if INITSCRIPT_ZIP_FILE_NAME != SHARED_ZIP_FILE_NAME:
    moduleName = m.__name__
else:
    name, ext = os.path.splitext(os.path.basename(os.path.normcase(FILE_NAME)))
    moduleName = "%s__main__" % name
code = importer.get_code(moduleName)
exec(code, m.__dict__)

versionInfo = sys.version_info[:3]
if versionInfo >= (2, 5, 0) and versionInfo <= (2, 6, 4):
    module = sys.modules.get("threading")
    if module is not None:
        module._shutdown()

