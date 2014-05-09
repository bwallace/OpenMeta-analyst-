#######################################################################
#                                                                     #
#  George Dietz                                                       #
#  CEBM @ Brown                                                       # 
#  OpenMeta[analyst]                                                  # 
#                                                                     # 
#  Handle application settings and manage workspace (temp directory)  # 
#                                                                     # 
#######################################################################

import os
from PyQt4 import QtCore, QtGui
from PyQt4.Qt import *
import meta_py_r

##################### HANDLE SETTINGS #####################

MAX_RECENT_FILES = 10
DEFAULT_SETTINGS = {"splash":True,
                    "digits":3,
                    "recent_files":[],
                    "explain_diag":True,
                    #"method_params":{},
                    }

def update_setting(field, value):
    ''' updates the setting with key field to value (gets converted to a
    QVariant internally, need to reconvert to desired type in get_setting() '''
    
    
    settings = QSettings()

    # see if we need to store the value in a special way
    value_type = get_setting_type(field)
    if value_type == list:
        # Make sure that the written elements are strings (for now...., maybe extend it to scalars (i.e. number or string) in the future)
        # for now, this is just for reading the most recent files list
        if settings.contains(field):
            settings.remove(field)
        settings.beginGroup(field)
        for i,x in enumerate(value): # value is a list
            settings.setValue(str(i),x)
        settings.endGroup()
    elif value_type == dict:
        raise Exception("Not implemented yet!")
    elif value_type == bool:
        settings.setValue(field, QVariant(value))
    elif value_type == QColor:
        # just being explicit to signify i am aware of QColors and to match get_setting
        settings.setValue(field, value)
    elif value_type == int:
        settings.setValue(field, value)
    elif value_type == str:
        settings.setValue(field, value)
    else:
        # nothing special needs to be done
        print("Field: %s" % field)
        print("Value type: %s" % str(value_type))
        raise Exception("Are you SURE that NOTHING special needs to be done?")
        settings.setValue(field, value)

def get_setting_type(field):
    return type(DEFAULT_SETTINGS[field])

def get_setting(field):
    settings = QSettings()

    # see if we need to store the value in a special way
    value_type = get_setting_type(field)
    #print("Setting type: %s for %s" % (str(value_type), field))
    if value_type == list:
        settings.beginGroup(field)
        indexes = list(settings.childKeys())
        foo_list = []
        for i in indexes:
            value = str(settings.value(i).toString())
            foo_list.append(value)
        settings.endGroup()
        setting_value = foo_list
    elif value_type == dict:
        raise Exception("Not implemented yet!")
    elif value_type == bool:
        print("Converted %s to a boolean" % field)
        setting_value = settings.value(field).toBool()
    elif value_type == str:
        setting_value = settings.value(field).toString()
    elif value_type == int:
        setting_value = settings.value(field).toInt()[0]
    elif value_type == QColor:
        setting_value = QColor(settings.value(field))
    else:
        # nothing special needs to be done
        raise Exception("Are you SURE that NOTHING special needs to be done?")
        setting_value = settings.value(field)

    return setting_value


def save_settings():
    print("saved settings")
    settings = QSettings()
    settings.sync() # writes to permanent storage


def load_settings():
    ''' loads settings from QSettings object, setting suitable defaults if
    there are missing fields '''

    settings = QSettings()

    def field_is_toplevel_child_group_keys(field_name):
        childgroups = list(settings.childGroups())
        toplevel_group_keys = [str(x) for x in childgroups]
        return field_name in toplevel_group_keys

    for field, value in DEFAULT_SETTINGS.items():
        setting_present = settings.contains(field) or field_is_toplevel_child_group_keys(field)
        if not setting_present:
            print("Filling in setting for %s" % field)
            update_setting(field, value)

    save_settings()
    print("loaded settings")
    return settings


def reset_settings():
    print("Resetting settings to default")
    settings = QSettings()
    settings.clear()

    for field, value in DEFAULT_SETTINGS.items():
        update_setting(field, value)

def add_file_to_recent_files(fpath):
    # add a new file to the front of the deque
    # move existing file to the front of the deque

    if fpath in [None, ""]:
        return False

    recent_files = get_setting("recent_files")

    if fpath in recent_files: #file already in list so move to front
        recent_files.remove(fpath)
    recent_files.append(fpath)

    # only want up to MAX_RECENT_FILES
    start_index = len(recent_files) - MAX_RECENT_FILES
    if start_index > 0:
        recent_files = recent_files[start_index:]

    update_setting("recent_files", recent_files)
    save_settings()

################ END HANDLE SETTINGS ######################


###### HANDLE R_TEMP IN USER-AREA DIRECTORY ###################
def setup_directories():
    '''Makes temporary data directory, r_tmp within that
    Sets python and R working directories to temporary data directory
    clears r_tmp '''
    
    # make base path and r_tmp
    base_path = make_base_path()
    make_r_tmp()
    
    meta_py_r.reset_Rs_working_dir() # set working directory on R side
    os.chdir(os.path.normpath(base_path)) # set working directory on python side
    
    clear_r_tmp() # clear r_tmp
    
    
def make_base_path():
    ''' Creates the base path if it doesn't exist and returns the path
    On mac, this is something like: /Users/george/Library/Application Support/OpenMetaAnalyst '''

    base_path = get_base_path()

    success = QDir().mkpath(base_path)
    if not success:
        raise Exception("Could not create base path at %s" % base_path)
    print("Made base path: %s" % base_path)
    return base_path

def get_base_path(normalize=False):
    '''normalize changes the path separators according to the OS,
    Usually this shouldn't be done because R is confused by backward slashes \
    because it sees it as an escape character and Qt is fine with / throughout '''

    base_path = str(QDesktopServices.storageLocation(QDesktopServices.DataLocation))
    if normalize:
        base_path = str(QDir.toNativeSeparators(base_path))
    print("Base path is: %s" % base_path)
    return base_path

def make_r_tmp():
    ''' Makes the r_tmp folder and returns the path to it'''
    r_tmp_path = "/".join([get_base_path(),"r_tmp"])
    success = QDir().mkpath(r_tmp_path)
    if not success:
        raise Exception("Could not create r_tmp path at %s" % r_tmp_path)
    print("Made r_tmp_path at %s" % r_tmp_path)
    return r_tmp_path

def to_posix_path(path):
    ''' for now, just changes \ to /
    Assumes there are no escapes in the path, very important!'''

    new_path = path.replace('\\', '/')
    return new_path

def clear_r_tmp():
    r_tmp_dir = os.path.join(get_base_path(), "r_tmp")
    print("Clearing %s" % r_tmp_dir)
    for file_p in os.listdir(r_tmp_dir):
        file_path = os.path.join(r_tmp_dir, file_p)
        try:
            if os.path.isfile(file_path):
                print("deleting %s" % file_path)
                os.unlink(file_path) # same as remove
        except Exception, e:
            print e
            
def get_user_documents_path():
    docs_path = str(QDesktopServices.storageLocation(QDesktopServices.DocumentsLocation))
    return docs_path
            
############## END OF HANDLE R_TEMP IN USER-AREA DIRECTORY ####################