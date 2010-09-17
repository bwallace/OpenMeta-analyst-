##################################################################
#                                                                #
#  Byron C. Wallace                                              #
#  Tufts Medical Center                                          #
#  OpenMeta[analyst]                                             #
#  ---                                                           #
#  We refer to methods that operate on estimates                 #
#  of subsets as `meta' methods. These include                   #
#  cumulative meta-analysis, leave-one-out meta-analysis         #
#  and all-subsets meta-analysis.                                #
#                                                                #
#  Any base meta-analytic                                        #
#  method can be used as a basis for these methods,              #
#  so long as the associated *.overall function                  #
#  is implemented.                                               #
##################################################################


##################################
#  cumulative MA.                #
##################################
cum.ma.binary <- function(fname, binary_data, params){
    # assert that the argument is the correct type
    #if (!("BinaryData" %in% class(binary_data))) stop("Binary data expected.")
    
    # iterate over the binaryData elements, adding one study at a time
    cum_results <- list()
    cum_labels <- list()
    
    ####
    # TODO need to compute CI for first study using est and SE;
    # this should be added to the cum_results here.
    ####
    for (i in 2:length(binary_data@studyNames)){
        # build a BinaryData object with the 
        # ith study removed.  
        y_tmp <- binary_data@y[1:i]
        SE_tmp <- binary_data@SE[1:i]
        names_tmp <- binary_data@studyNames[1:i]
        bin_data_tmp <- NULL
        if (length(binary_data@g1O1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            g1O1_tmp <- binary_data@g1O1[1:i]
            g1O2_tmp <- binary_data@g1O2[1:i]
            g2O1_tmp <- binary_data@g2O1[1:i]
            g2O2_tmp <- binary_data@g2O2[1:i]
            bin_data_tmp <- new('BinaryData', g1O1=g1O1_tmp, 
                               g1O2=g1O2_tmp , g2O1=g2O1_tmp, 
                               g2O2=g2O2_tmp, y=y_tmp, SE=SE_tmp, studyNames=names_tmp)
        }
        else{
            bin_data_tmp <- new('BinaryData', y=y_tmp, SE=SE_tmp, studyNames=names_tmp)
        }
        cur_res <- eval(call(fname, bin_data_tmp, params))
        cur_overall <- eval(call(paste(fname, ".overall", sep=""), cur_res))
        cum_results <- c(cum_results, cur_overall)
        cum_labels <- c(cum_labels, paste("+ Study", i))
    }
    
    
    #res <- cumul(res)
    
    ### @TODO 
    # generate cum MA plot
    forest_path <- "./r_tmp/cum_forest.png"
    #png(forest_path)
    #forest(res)
    #dev.off()

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("cumulative forest plot"=forest_path)
    plot_names <- c("cumulative forest plot"="cumulative forest_plot")
    
    results <- list("images"=images, "cum_results"=cum_results, "cum_labels"=cum_labels, "plot_names"=plot_names)
    results
}




