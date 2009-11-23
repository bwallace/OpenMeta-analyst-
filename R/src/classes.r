####################################
#                                                                             #
# OpenMeta[Analyst]                                              #
# ----                                                                        #
# classes.r                                                              # 
# contains class definitions for OpenMeta.            #
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
               g1Name="character", g2Name="character"), 
               contains="OMData")