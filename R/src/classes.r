####################################
#                                                                             #
# OpenMeta[Analyst]                                              #
# ----                                                                        #
# classes.r                                                              # 
# contains class definitions for OpenMeta.            #
#                                                                             #    
# (note that classes in R are basically structs)        #
####################################

#
# This is the super (parent) class. All Open Meta data objects should inherit from this
# type.
#
setClass("OMData", representation(studyNames="character", notes="character", years="integer"))

#
# BinaryData type
#
setClass("BinaryData", 
               representation(g1O1="numeric", g1O2="numeric", g2O1="numeric",g2O2="numeric",
               y="numeric", SE="numeric",
               g1Name="character", g2Name="character"), 
               contains="OMData")
               
               
# 
#
# The specificiation class contains parameters, etc., for the method to be run
#
setClass("AnalysisSpecification", 
                representation(parameters="data.frame"))
                