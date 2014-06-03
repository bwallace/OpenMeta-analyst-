'''
This file should only be used when launching a windows build, not during
developement

@author: George Dietz
         CEBM@Brown
'''

import os

# # Set R environment variables
# oldpath = os.environ["PATH"]
# cwd = os.getcwd()
# rpath = os.path.join(cwd, "R_dist") # second 'Resources' is R directory
# # just adding the 64-bit path version for now
# os.environ["PATH"] = os.path.join(rpath, "bin","x64") + os.pathsep + oldpath
# print("new path is: %s" % os.environ["PATH"])
# 
# #os.environ["R"] = os.path.join(cwd, rpath, "bin")
# os.environ["R_HOME"] = os.path.join(cwd, rpath)
# #os.environ["R_HOME"] = os.path.join(rpath, "bin","x64")
# print("R_HOME: %s" % os.environ["R_HOME"])
# 
# os.environ["R_USER"] = "oma" 

# we are ready to start the main program loop
import launch
if __name__ == "__main__":
    launch.start()