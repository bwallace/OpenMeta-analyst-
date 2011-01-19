####################################
#                                  #
# OpenMeta[Analyst]                #
# ----                             #
# classes.r                        # 
# contains class definitions for   #
#  OpenMeta.                       #   
#                                  #    
# (note that classes in R are      #
#   basically structs)             #
####################################

#
# This is the super (parent) class. All Open Meta data objects should inherit from this
# type.
setClass("OMData", representation(study.names="character", notes="character", 
         years="integer", covariates="list"))

####
# BinaryData type
#
setClass("BinaryData", 
               representation(g1O1="numeric", g1O2="numeric", g2O1="numeric", g2O2="numeric",
               y="numeric", SE="numeric",
               g1.name="character", g2.name="character"), 
               contains="OMData")
        

####
# DiagnosticData type
#       
setClass("DiagnosticData", 
               representation(TP="numeric", FN="numeric", TN="numeric", FP="numeric", y="numeric", SE="numeric",
               g1.name="character", g2.name="character"), 
               contains="OMData")
               
####
# ContinuousData type
#       
setClass("ContinuousData", 
               representation(N1="numeric", mean1="numeric", sd1="numeric",
               N2="numeric", mean2="numeric", sd2="numeric",
               y="numeric", SE="numeric",
               g1.name="character", g2.name="character"), 
               contains="OMData")
               
               
#
# The specificiation class contains parameters, etc., for the method to be run
#
setClass("AnalysisSpecification", 
                representation(parameters="data.frame"))
                