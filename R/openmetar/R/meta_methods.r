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
    cum.results <- array(dim=c(length(binary.data@studyNames),3))
    params$createPlot <- FALSE
    for (i in 1:length(binary.data@studyNames)){
        # build a BinaryData object including studies
        # 1 through i
        y.tmp <- binary.data@y[1:i]
        SE.tmp <- binary.data@SE[1:i]
        names.tmp <- binary.data@studyNames[1:i]
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
                               g2O2=g2O2.tmp, y=y.tmp, SE=SE.tmp, studyNames=names.tmp)
        }
        else{
            bin.data.tmp <- new('BinaryData', y=y.tmp, SE=SE.tmp, studyNames=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, bin.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[i,] <- cur.overall
    }
    
    studyNames <- binary.data@studyNames[1] 
    for (count in 2:length(binary.data@studyNames)) {
        studyNames <- c(studyNames, paste("+ ",binary.data@studyNames[count], sep=""))
    }
    cumDisp <- createOverallDisp(cum.results, studyNames, params)
    cumDisp
    forest_path <- "./r_tmp/cum_forest.png"
    plotData <- create.plot.data.overall(binary.data, params, cum.results, studyNames)
    forest.plot(plotData, outpath=forest_path)

    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest_path)
    plot.names <- c("cumulative forest plot"="cumulative_forest_plot")
    
    results <- list("images"=images, "Summary"=cumDisp, "plot_names"=plot.names)
    results
}


##################################
#  binary leave-one-out MA       #
##################################
loo.ma.binary <- function(fname, binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    
    loo.results <- array(dim=c(length(binary.data@studyNames),3))
    params$createPlot <- FALSE
    N <- length(binary.data@studyNames)
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
        names.tmp <- binary.data@studyNames[index.ls]
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
                               g2O2=g2O2.tmp, y=y.tmp, SE=SE.tmp, studyNames=names.tmp)
        }
        else{
            bin.data.tmp <- new('BinaryData', y=y.tmp, SE=SE.tmp, studyNames=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, bin.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[i,] <- cur.overall
    }
    
    studyNames <- binary.data@studyNames[1] 
    for (count in 2:length(binary.data@studyNames)) {
        studyNames <- c(studyNames, paste("- ",binary.data@studyNames[count], sep=""))
    }
    looDisp <- createOverallDisp(loo.results, studyNames, params)
    looDisp
    forest.path <- "./r_tmp/loo_forest.png"
    plotData <- create.plot.data.overall(binary.data, params, loo.results, studyNames)
    forest.plot(plotData, outpath=forest.path)
    

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     

    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    #loo.results <- print.Table(LOO.display.frame(extractDataLOO(loo.results, params)))
    results <- list("images"=images, "loo_results"=looDisp, "plot_names"=plot.names)

    results
}

extractDataLOO <- function(results , params, title = "Leave-one-out sensitivity analysis") { 
    #loo.results, loo.labels
    # Extracts data from loo.results
    # Compute bounds on confidence intervals.
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    LL <- round(exp(results$loo_results$y - mult*results$loo_results$SE), digits = params$digits)
    UL <- round(exp(results$loo_results$y + mult*results$loo_results$SE), digits = params$digits)
    running.summary.es <- round(exp(results$loo_results$y), digits = params$digits)
    # Extract labels
    removed.labels <- results$loo_labels
    data.to.present <- list(excluded.study = removed.labels, summary.estimates = running.summary.es, 
                       lower.bound = LL, upper.bound = UL)
    data.to.present
}

LOO.display.frame <- function(data.to.present) {
    LOO.estimates <- NULL
    for (i in 1:length(data.to.present$summary.estimates) ) {
        LOO.estimates[i] <- paste(data.to.present$summary.estimates[i] ,"(", 
                                  data.to.present$lower.bound[i]," to " , 
                                  data.to.present$upper.bound[i] , ")") 
            }
    estimate.title <- eval(paste("Summary estimate " , "(" , params$conf.level , "% CI)" , sep = "" )  )
    table.data <- array(data = c("Excluded study" , data.to.present$excluded.study , 
                      estimate.title, c(LOO.estimates)), dim = c(length(LOO.estimates)+1,2))
    table.data  
}

##################################
#  continuous cumulative MA      #
##################################
cum.ma.continuous <- function(fname, cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    # iterate over the continuousData elements, adding one study at a time
    cum.results <- array(dim=c(length(cont.data@studyNames),3))
    params$createPlot <- FALSE
    for (i in 1:length(cont.data@studyNames)){
        # build a ContinuousData object including studies
        # 1 through i
        y.tmp <- cont.data@y[1:i]
        SE.tmp <- cont.data@SE[1:i]
        names.tmp <- cont.data@studyNames[1:i]
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
                               studyNames=names.tmp)
        }
        else{
            cont.data.tmp <- new('ContinuousData', 
                                y=y.tmp, SE=SE.tmp, 
                                studyNames=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, cont.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[i,] <- cur.overall
    }
    
    studyNames <- cont.data@studyNames[1] 
    for (count in 2:length(cont.data@studyNames)) {
        studyNames <- c(studyNames, paste("+ ",cont.data@studyNames[count], sep=""))
    }
    cumDisp <- createOverallDisp(cum.results, studyNames, params)
    cumDisp
    forest.path <- "./r_tmp/cum_forest.png"
    plotData <- create.plot.data.overall(cont.data, params, cum.results, studyNames)
    forest.plot(plotData, outpath=forest.path)
    
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative forest_plot")
    
    results <- list("images"=images, "Summary"=cumDisp, "plot_names"=plot.names)
    results
}

##################################
#  continuous leave-one-out MA   #
##################################
loo.ma.continuous <- function(fname, cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    loo.results <- array(dim=c(length(cont.data@studyNames),3))
    params$createPlot <- FALSE
    N <- length(cont.data@studyNames)
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
        names.tmp <- cont.data@studyNames[index.ls]
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
                               studyNames=names.tmp)
        }
        else{
            cont.data.tmp <- new('ContinuousData', 
                                y=y.tmp, SE=SE.tmp, 
                                studyNames=names.tmp)
        }
        # call the parametric function by name, passing along the 
        # data and parameters. Notice that this method knows
        # neither what method its calling nor what parameters
        # it's passing!
        cur.res <- eval(call(fname, cont.data.tmp, params))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[i,] <- cur.overall
    }
    
    studyNames <- cont.data@studyNames[1] 
    for (count in 2:length(cont.data@studyNames)) {
        studyNames <- c(studyNames, paste("- ",cont.data@studyNames[count], sep=""))
    }
    looDisp <- createOverallDisp(loo.results, studyNames, params)
    looDisp

    forest.path <- "./r_tmp/loo_forest.png"
    plotData <- create.plot.data.overall(cont.data, params, loo.results, studyNames)
    forest.plot(plotData, outpath=forest.path)
    
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    
    results <- list("images"=images, "Summary"=looDisp, "plot_names"=plot.names)
    results
}