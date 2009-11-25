####################################
#                                                                            #
# OpenMeta[Analyst]                                             #
# ----                                                                       #
# binary_methods.r                                                # 
# Facade module; wraps methods that perform    #
# analysis on binary data in a coherent interface.  #
####################################


library(metafor)

binary.rmh <- function(binaryData){
    
    # assert that the argument is the correct type
    if ("BinaryData" %in% class(binaryData)) stop("Binary data expected.")
    
    # call out to the metafor package
    res<-rma.mh(binaryData@g1O1, binaryData@g1O2, 
                                binaryData@g2O1, binaryData@g2O2)
    
    #
    # generate forest plot (should we do this here?)
    #
    pdf("forest.pdf")
    forest.rma(res)
    dev.off()
    
    res
}