#############################################################
#                                                           #
#   OpenMeta[Analyst]                                       #  
#   Byron C Wallace                                         # 
#                                                           # 
#  this code can be used to build the openmetar package     #
#  execute the below code in R. then, at a command line,    #
#  run:*                                                    #
#  > R CMD build openmetar                                  #
#  > R CMD INSTALL openmetar_x.x.tgz.gz                     #
#                                                           #
#  *note, you have to remove the directory R/openmetar      #
#    if it already exists prior to building                 #
#                                                           #
# If you're on windows you can use the batch script         #    
# "make_package.bat"; though you may have to change         #
# the paths.                                                #
#############################################################

# set working directory; may change depending on your OS/settings
setwd("../../../dev")
setwd("OpenMeta/R/src/")

# update if more files/modules are added
flist <- c("classes.r", "binary_methods.r", "continuous_methods.r", 
             "data_transform.r", "meta_methods.r", "plotting.r", "meta_reg.r")  
package.skeleton(name="openmetar", path="../", code_files=flist)

