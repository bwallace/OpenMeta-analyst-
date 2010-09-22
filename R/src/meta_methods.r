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
#  binary cumulative MA          #
##################################
cum.ma.binary <- function(fname, binary_data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary_data))) stop("Binary data expected.")
    
    # iterate over the binaryData elements, adding one study at a time
    cum_results <- list()
    cum_labels <- list()
    
    for (i in 1:length(binary_data@studyNames)){
        # build a BinaryData object including studies
        # 1 through i
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
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
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



loo.ma.binary <- function(fname, binary_data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary_data))) stop("Binary data expected.")
    
    loo_results <- list()
    loo_labels <- list()
    N <- length(binary_data@studyNames)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index_ls <- 1:N
        if (i == 1){
            index_ls <- index_ls[2:N]
        }
        else if (i==N){
            index_ls <- index_ls[1:N-1]
        }
        else{
            index_ls <- c(index_ls[1:i-1], index_ls[(i+1):N])  
        }
        
        # build a BinaryData object with the 
        # ith study removed.  
        y_tmp <- binary_data@y[index_ls]
        SE_tmp <- binary_data@SE[index_ls]
        names_tmp <- binary_data@studyNames[index_ls]
        bin_data_tmp <- NULL
        
        if (length(binary_data@g1O1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            g1O1_tmp <- binary_data@g1O1[index_ls]
            g1O2_tmp <- binary_data@g1O2[index_ls]
            g2O1_tmp <- binary_data@g2O1[index_ls]
            g2O2_tmp <- binary_data@g2O2[index_ls]
            bin_data_tmp <- new('BinaryData', g1O1=g1O1_tmp, 
                               g1O2=g1O2_tmp , g2O1=g2O1_tmp, 
                               g2O2=g2O2_tmp, y=y_tmp, SE=SE_tmp, studyNames=names_tmp)
        }
        else{
            bin_data_tmp <- new('BinaryData', y=y_tmp, SE=SE_tmp, studyNames=names_tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur_res <- eval(call(fname, bin_data_tmp, params))
        cur_overall <- eval(call(paste(fname, ".overall", sep=""), cur_res))
        loo_results <- c(loo_results, cur_overall)
        loo_labels <- c(loo_labels, paste("- Study", i))
    }
    
    ### @TODO 
    # generate loo MA plot
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
    images <- c("loo forest plot"=forest_path)
    plot_names <- c("loo forest plot"="loo_forest_plot")
    
    results <- list("images"=images, "cum_results"=loo_results, "cum_labels"=loo_labels, "plot_names"=plot_names)
    results
}



##################################
#  continuous cumulative MA      #
##################################
cum.ma.continuous <- function(fname, cont_data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont_data))) stop("Continuous data expected.")
    
    # iterate over the continuousData elements, adding one study at a time
    cum_results <- list()
    cum_labels <- list()
    
    for (i in 1:length(cont_data@studyNames)){
        # build a ContinuousData object including studies
        # 1 through i
        y_tmp <- cont_data@y[1:i]
        SE_tmp <- cont_data@SE[1:i]
        names_tmp <- cont_data@studyNames[1:i]
        cont_data_tmp <- NULL
        if (length(cont_data@N1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            N1_tmp <- cont_data@N1[1:i]
            mean1_tmp <- cont_data@mean1[1:i]
            sd1_tmp <- cont_data@sd1[1:i]
            N2_tmp <- cont_data@N2[1:i]
            mean2_tmp <- cont_data@mean2[1:i]
            sd2_tmp <- cont_data@sd2[1:i]
            cont_data_tmp <- new('ContinuousData', 
                               N1=N1_tmp, mean1=mean1_tmp , sd1=sd1_tmp, 
                               N2=N2_tmp, mean2=mean2_tmp, sd2=sd2_tmp,
                               y=y_tmp, SE=SE_tmp, 
                               studyNames=names_tmp)
        }
        else{
            cont_data_tmp <- new('ContinuousData', 
                                y=y_tmp, SE=SE_tmp, 
                                studyNames=names_tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur_res <- eval(call(fname, cont_data_tmp, params))
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

