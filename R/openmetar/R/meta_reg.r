##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  meta-reg.r                                                    #
##################################################################

library(metafor)

binary.meta.regression <- function(binary.data, cov.name){
    cov.vals <- binary.data@covariates$cov.name
    res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@studyNames,
                                level=params$conf.level, digits=params$digits, method="DL", add=params$adjust,
                                to=params$to, mods=cov.vals)
                                
    betas <- res$b
    
    
}