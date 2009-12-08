####################################
# OpenMeta[Analyst]                                             #
# ----                                                                       #
# binary_methods.r                                                # 
# Facade module; wraps methods that perform    #
# analysis on binary data in a coherent interface.  #
####################################


library(metafor)

binary.rmh <- function(binaryData, params){
    
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binaryData))) stop("Binary data expected.")
    
    print(binaryData@g1O1)
    
    # call out to the metafor package
    res<-rma.uni(measure="OR", ai=binaryData@g1O1, bi=binaryData@g1O2, 
                                ci=binaryData@g2O1, di=binaryData@g2O2)
    
    #
    # generate forest plot (should we do this here?)
    #
    getwd()
    pdf("forest.pdf")
    forest.rma(res)
    dev.off()
    
    res
}


binary.rmh.parameters <- function(){
    #params <- data.frame(rm.method=c("ENUM", "HE", "DL", "SJ", "ML", "REML", "EB"), conf.level="FLOAT", digits="FLOAT")
    rm_method_ls <- c("HE", "DL", "SJ", "ML", "REML", "EB")
    #rm_method_ls <- list("ENUM", rm_method_ls)

    params <- list("rm.method"=rm_method_ls, "conf.level"="FLOAT", "digits"="FLOAT")
}