##########################
# Author:  George Dietz  #
# CEBM @ Brown           #
#
# DESCRIPTION: Script for py2exe for building windows executable
#              Run with > python gsetup.py py2exe --includes sip
#
##########################

# setup.py to build windows executable


#from py2exe.build_exe import py2exe
from distutils.core import setup
import py2exe

##path_to_launch_file = os.path.join("src","launch.py")

setup(windows=[{"script": 'launch.py'}])