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
cum.ma.binary <- function(fname, binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    
    # iterate over the binaryData elements, adding one study at a time
    cum.results <- array(dim=c(length(binary.data@study.names),3))
    params$create.plot <- FALSE
    for (i in 1:length(binary.data@study.names)){
        # build a BinaryData object including studies
        # 1 through i
        y.tmp <- binary.data@y[1:i]
        SE.tmp <- binary.data@SE[1:i]
        names.tmp <- binary.data@study.names[1:i]
        bin.data.tmp <- NULL
        if (length(binary.data@g1O1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            g1O1.tmp <- binary.data@g1O1[1:i]
            g1O2.tmp <- binary.data@g1O2[1:i]
            g2O1.tmp <- binary.data@g2O1[1:i]
            g2O2.tmp <- binary.data@g2O2[1:i]
            bin.data.tmp <- new('BinaryData', g1O1=g1O1.tmp, 
                               g1O2=g1O2.tmp , g2O1=g2O1.tmp, 
                               g2O2=g2O2.tmp, y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        } else {
            bin.data.tmp <- new('BinaryData', y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, bin.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[i,] <- cur.overall
    }
    cum.results.display <- binary.transform.f(params$measure)$display.scale(cum.results)
    study.names <- binary.data@study.names[1] 
    for (count in 2:length(binary.data@study.names)) {
        study.names <- c(study.names, paste("+ ",binary.data@study.names[count], sep=""))
    }
    
    cum.disp <- create.overall.display(cum.results.display, study.names, params)
    forest.path <- "./r_tmp/cum_forest.png"
    params$fp_show_summary_line <- TRUE
    # temporarily hard-coding this param
    plot.data <- create.plot.data.overall(params, res=cum.results, data.type="binary", study.names, addRow1Space=TRUE)
    forest.plot(plot.data, outpath=forest.path)

    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative_forest_plot")
    
    results <- list("images"=images, "Cumulative Summary"=cum.disp, "plot_names"=plot.names)
    results
}


##################################
#  binary leave-one-out MA       #
##################################
loo.ma.binary <- function(fname, binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    
    loo.results <- array(dim=c(length(binary.data@study.names),3))
    params$create.plot <- FALSE
    N <- length(binary.data@study.names)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index.ls <- 1:N
        if (i == 1){
            index.ls <- index.ls[2:N]
        }
        else if (i==N){
            index.ls <- index.ls[1:N-1]
        }
        else{
            index.ls <- c(index.ls[1:i-1], index.ls[(i+1):N])  
        }
        
        # build a BinaryData object with the 
        # ith study removed.  
        y.tmp <- binary.data@y[index.ls]
        SE.tmp <- binary.data@SE[index.ls]
        names.tmp <- binary.data@study.names[index.ls]
        bin.data.tmp <- NULL
        
        if (length(binary.data@g1O1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            g1O1.tmp <- binary.data@g1O1[index.ls]
            g1O2.tmp <- binary.data@g1O2[index.ls]
            g2O1.tmp <- binary.data@g2O1[index.ls]
            g2O2.tmp <- binary.data@g2O2[index.ls]
            bin.data.tmp <- new('BinaryData', g1O1=g1O1.tmp, 
                               g1O2=g1O2.tmp , g2O1=g2O1.tmp, 
                               g2O2=g2O2.tmp, y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        } else{
            bin.data.tmp <- new('BinaryData', y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, bin.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[i,] <- cur.overall
    }
    loo.results.display <- binary.transform.f(params$measure)$display.scale(loo.results)
    study.names <- NULL 
    for (count in 1:length(binary.data@study.names)) {
        study.names <- c(study.names, paste("- ",binary.data@study.names[count], sep=""))
    }
    loo.disp <- create.overall.display(loo.results.display, study.names, params)
    forest.path <- "./r_tmp/loo_forest.png"
    params$fp_show_summary_line <- FALSE
    # temporarily hard-coding this param as FALSE because we haven't computed an overall estimate,
    # so there's no value for a summary line. This could be changed.
    plot.data <- create.plot.data.overall(params, res=loo.results, data.type="binary", study.names, addRow1Space=FALSE)
    forest.plot(plot.data, outpath=forest.path)
    

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     

    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    results <- list("images"=images, "Leave-one-out Summary"=loo.disp, "plot_names"=plot.names)

    results
}

##################################
#  diagnostic cumulative MA      #
##################################
cum.ma.diagnostic <- function(fname, diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    
    # iterate over the diagnosticData elements, adding one study at a time
    cum.results <- array(dim=c(length(diagnostic.data@study.names),3))
    params$create.plot <- FALSE
    for (i in 1:length(diagnostic.data@study.names)){
        # build a DiagnosticData object including studies
        # 1 through i
        y.tmp <- diagnostic.data@y[1:i]
        SE.tmp <- diagnostic.data@SE[1:i]
        names.tmp <- diagnostic.data@study.names[1:i]
        diag.data.tmp <- NULL
        if (length(diagnostic.data@TP) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            TP.tmp <- diagnostic.data@TP[1:i]
            FN.tmp <- diagnostic.data@FN[1:i]
            TN.tmp <- diagnostic.data@TN[1:i]
            FP.tmp <- diagnostic.data@FP[1:i]
            diag.data.tmp <- new('DiagnosticData', TP=TP.tmp, 
                               FN=FN.tmp , TN=TN.tmp, 
                               FP=FP.tmp, y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        } else {
            diag.data.tmp <- new('DiagnosticData', y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, diag.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[i,] <- cur.overall
    }
    cum.results.display <- diagnostic.transform.f(params$measure)$display.scale(cum.results)
    study.names <- diagnostic.data@study.names[1] 
    for (count in 2:length(diagnostic.data@study.names)) {
        study.names <- c(study.names, paste("+ ",diagnostic.data@study.names[count], sep=""))
    }
    
    cum.disp <- create.overall.display(cum.results.display, study.names, params)
    forest.path <- "./r_tmp/cum_forest.png"
    params$fp_show_summary_line <- TRUE
    # temporarily hard-coding this param
    plot.data <- create.plot.data.overall(params, res=cum.results, data.type="diagnostic", study.names, addRow1Space=TRUE)
    forest.plot(plot.data, outpath=forest.path)

    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative_forest_plot")
    
    results <- list("images"=images, "Cumulative Summary"=cum.disp, "plot_names"=plot.names)
    results
}


##################################
#  diagnostic leave-one-out MA   #
##################################
loo.ma.diagnostic <- function(fname, diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    
    loo.results <- array(dim=c(length(diagnostic.data@study.names),3))
    params$create.plot <- FALSE
    N <- length(diagnostic.data@study.names)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index.ls <- 1:N
        if (i == 1){
            index.ls <- index.ls[2:N]
        }
        else if (i==N){
            index.ls <- index.ls[1:N-1]
        }
        else{
            index.ls <- c(index.ls[1:i-1], index.ls[(i+1):N])  
        }
        
        # build a DiagnosticData object with the 
        # ith study removed.  
        y.tmp <- diagnostic.data@y[index.ls]
        SE.tmp <- diagnostic.data@SE[index.ls]
        names.tmp <- diagnostic.data@study.names[index.ls]
        diag.data.tmp <- NULL
        
        if (length(diagnostic.data@TP) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            TP.tmp <- diagnostic.data@TP[index.ls]
            FN.tmp <- diagnostic.data@FN[index.ls]
            TN.tmp <- diagnostic.data@TN[index.ls]
            FP.tmp <- diagnostic.data@FP[index.ls]
            diag.data.tmp <- new('DiagnosticData', TP=TP.tmp, 
                               FN=FN.tmp , TN=TN.tmp, 
                               FP=FP.tmp, y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        } else{
            diag.data.tmp <- new('DiagnosticData', y=y.tmp, SE=SE.tmp, study.names=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, diag.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[i,] <- cur.overall
    }
    loo.results.display <- diagnostic.transform.f(params$measure)$display.scale(loo.results)
    study.names <- NULL 
    for (count in 1:length(diagnostic.data@study.names)) {
        study.names <- c(study.names, paste("- ",diagnostic.data@study.names[count], sep=""))
    }
    loo.disp <- create.overall.display(loo.results.display, study.names, params)
    forest.path <- "./r_tmp/loo_forest.png"
    params$fp_show_summary_line <- FALSE
    # temporarily hard-coding this param as FALSE because we haven't computed an overall estimate,
    # so there's no value for a summary line. This could be changed.
    plot.data <- create.plot.data.overall(params, res=loo.results, data.type="diagnostic", study.names, addRow1Space=FALSE)
    forest.plot(plot.data, outpath=forest.path)
    

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     

    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    results <- list("images"=images, "Leave-one-out Summary"=loo.disp, "plot_names"=plot.names)

    results
}

##################################
#  continuous cumulative MA      #
##################################
cum.ma.continuous <- function(fname, cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    # iterate over the continuousData elements, adding one study at a time
    cum.results <- array(dim=c(length(cont.data@study.names),3))
    params$create.plot <- FALSE
    for (i in 1:length(cont.data@study.names)){
        # build a ContinuousData object including studies
        # 1 through i
        y.tmp <- cont.data@y[1:i]
        SE.tmp <- cont.data@SE[1:i]
        names.tmp <- cont.data@study.names[1:i]
        cont.data.tmp <- NULL
        if (length(cont.data@N1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            N1.tmp <- cont.data@N1[1:i]
            mean1.tmp <- cont.data@mean1[1:i]
            sd1.tmp <- cont.data@sd1[1:i]
            N2.tmp <- cont.data@N2[1:i]
            mean2.tmp <- cont.data@mean2[1:i]
            sd2.tmp <- cont.data@sd2[1:i]
            cont.data.tmp <- new('ContinuousData', 
                               N1=N1.tmp, mean1=mean1.tmp , sd1=sd1.tmp, 
                               N2=N2.tmp, mean2=mean2.tmp, sd2=sd2.tmp,
                               y=y.tmp, SE=SE.tmp, 
                               study.names=names.tmp)
        }
        else{
            cont.data.tmp <- new('ContinuousData', 
                                y=y.tmp, SE=SE.tmp, 
                                study.names=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, cont.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[i,] <- cur.overall
    }
    cum.results.display <- diagnostic.transform.f(params$measure)$display.scale(cum.results)
    study.names <- cont.data@study.names[1] 
    for (count in 2:length(cont.data@study.names)) {
        study.names <- c(study.names, paste("+ ",cont.data@study.names[count], sep=""))
    }
    cum.disp <- create.overall.display(cum.results.display, study.names, params)
    forest.path <- "./r_tmp/cum_forest.png"
    params$fp_show_summary_line <- FALSE
    # temporarily hard-coding this param
    plot.data <- create.plot.data.overall(params, res=cum.results, data.type="continuous", study.names, addRow1Space=TRUE)
    forest.plot(plot.data, outpath=forest.path)
    
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative forest_plot")
    
    results <- list("images"=images, "Cumulative Summary"=cum.disp, "plot_names"=plot.names)
    results
}

##################################
#  continuous leave-one-out MA   #
##################################
loo.ma.continuous <- function(fname, cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    loo.results <- array(dim=c(length(cont.data@study.names),3))
    params$create.plot <- FALSE
    N <- length(cont.data@study.names)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index.ls <- 1:N
        if (i == 1){
            index.ls <- index.ls[2:N]
        }
        else if (i==N){
            index.ls <- index.ls[1:N-1]
        }
        else{
            index.ls <- c(index.ls[1:i-1], index.ls[(i+1):N])  
        }
        
        # build a BinaryData object with the 
        # ith study removed.  
        y.tmp <- cont.data@y[index.ls]
        SE.tmp <- cont.data@SE[index.ls]
        names.tmp <- cont.data@study.names[index.ls]
        cont.data.tmp <- NULL
        
        if (length(cont.data@N1) > 0){
            # if we have group level data for 
            # group 1, outcome 1, then we assume
            # we have it for all groups
            N1.tmp <- cont.data@N1[index.ls]
            mean1.tmp <- cont.data@mean1[index.ls]
            sd1.tmp <- cont.data@sd1[index.ls]
            N2.tmp <- cont.data@N2[index.ls]
            mean2.tmp <- cont.data@mean2[index.ls]
            sd2.tmp <- cont.data@sd2[index.ls]
            cont.data.tmp <- new('ContinuousData', 
                               N1=N1.tmp, mean1=mean1.tmp , sd1=sd1.tmp, 
                               N2=N2.tmp, mean2=mean2.tmp, sd2=sd2.tmp,
                               y=y.tmp, SE=SE.tmp, 
                               study.names=names.tmp)
        }
        else{
            cont.data.tmp <- new('ContinuousData', 
                                y=y.tmp, SE=SE.tmp, 
                                study.names=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, cont.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[i,] <- cur.overall
    }
    loo.results.display <- diagnostic.transform.f(params$measure)$display.scale(loo.results)
    study.names <- NULL
    for (count in 1:length(cont.data@study.names)) {
        study.names <- c(study.names, paste("- ",cont.data@study.names[count], sep=""))
    }
    loo.disp <- create.overall.display(loo.results.display, study.names, params)

    forest.path <- "./r_tmp/loo_forest.png"
    params$fp_show_summary_line <- FALSE
    # temporarily hard-coding this param as FALSE because we haven't computed an overall estimate,
    # so there's no value for a summary line. This could be changed.
    plotData <- create.plot.data.overall(params, res=loo.results, study.names, addRow1Space=FALSE)
    forest.plot(plotData, outpath=forest.path)
    
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Leave-one-out Forest Plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    
    results <- list("images"=images, "Leave-one-out Summary"=loo.disp, "plot_names"=plot.names)
    results
}