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
    
    res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
    
    plot.data <- create.plot.data.binary(binary.data, params, res)
    # data for standard forest plot
    
    # iterate over the binaryData elements, adding one study at a time
    cum.results <- array(list(NULL), dim=c(length(binary.data@study.names)))
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
        cum.results[[i]] <- cur.overall 
    }
    study.names <- binary.data@study.names[1] 
    for (count in 2:length(binary.data@study.names)) {
        study.names <- c(study.names, paste("+ ",binary.data@study.names[count], sep=""))
    }
    
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "binary.fixed.inv.var") {
        model.title <- paste("Binary Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "binary.fixed.mh") {
        model.title <- paste("Binary Fixed-effect Model - Mantel Haenszel\n\nMetric: ", metric.name, sep="")
    } else if (fname == "binary.fixed.peto") {
        model.title <- paste("Binary Fixed-effect Model - Peto\n\nMetric: ", metric.name, sep="")
    } else if (fname == "binary.random") {
        model.title <- paste("Binary Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    cum.disp <- create.overall.display(res=cum.results, study.names, params, model.title, data.type="binary")
    forest.path <- paste(params$fp_outpath, sep="")
    
    params.cum <- params
    params.cum$fp_col1_str <- "Cumulative Studies"
    params.cum$fp_col2_str <- "Cumulative Estimate"
    # column labels for the cumulative (right-hand) plot
    plot.data.cum <- create.plot.data.overall(res=cum.results, study.names, params.cum, data.type="binary", addRow1Space=TRUE)
    two.forest.plots(forest.data1=plot.data, forest.data2=plot.data.cum, outpath=forest.path)

    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Cumulative Summary"=cum.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}


##################################
#  binary leave-one-out MA       #
##################################
loo.ma.binary <- function(fname, binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    res<-rma.uni(yi=binary.data@y, sei=binary.data@SE, slab=binary.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
    
    loo.results <- array(list(NULL), dim=c(length(binary.data@study.names)))
    params$create.plot <- FALSE
    N <- length(binary.data@study.names)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index.ls <- setdiff(1:N, i)
        
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
        loo.results[[i]] <- cur.overall
    }
    study.names <- NULL 
    for (count in 1:length(binary.data@study.names)) {
        study.names <- c(study.names, paste("- ",binary.data@study.names[count], sep=""))
    }
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "binary.fixed.inv.var") {
        model.title <- paste("Binary Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "binary.fixed.mh") {
        model.title <- paste("Binary Fixed-effect Model - Mantel Haenszel\n\nMetric: ", metric.name, sep="")
    } else if (fname == "binary.fixed.peto") {
        model.title <- paste("Binary Fixed-effect Model - Peto\n\nMetric: ", metric.name, sep="")
    } else if (fname == "binary.random") {
        model.title <- paste("Binary Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    
    loo.disp <- create.overall.display(res=loo.results, study.names, params, model.title, data.type="binary")
    forest.path <- paste(params$fp_outpath, sep="")
    
    plot.data <- create.plot.data.overall(res=loo.results, study.names, params, data.type="binary", addRow1Space=FALSE)
    forest.plot(forest.data=plot.data, outpath=forest.path)

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     

    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Leave-one-out Summary"=loo.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

##################################
#  diagnostic cumulative MA      #
##################################
cum.ma.diagnostic <- function(fname, diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    
    res<-rma.uni(yi=diagnostic.data@y, sei=diagnostic.data@SE, slab=diagnostic.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
    
    plot.data <- create.plot.data.diagnostic(diagnostic.data, params, res)
    
    # iterate over the diagnosticData elements, adding one study at a time
    cum.results <- array(list(NULL), dim=c(length(diagnostic.data@study.names)))
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
        cum.results[[i]] <- cur.overall
    }
    study.names <- diagnostic.data@study.names[1] 
    for (count in 2:length(diagnostic.data@study.names)) {
        study.names <- c(study.names, paste("+ ",diagnostic.data@study.names[count], sep=""))
    }
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "diagnostic.fixed") {
        model.title <- paste("Diagnostic Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "diagnostic.random") {
        model.title <- paste("Diagnostic Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    
    cum.disp <- create.overall.display(res=cum.results, study.names, params, model.title, data.type="diagnostic")
    forest.path <- paste(params$fp_outpath, sep="")
    params$fp_col1_str <- "Cumulative Studies"
    # label for the cumulative (right-hand) plot
    plot.data.cum <- create.plot.data.overall(res=cum.results, study.names, params, data.type="diagnostic", addRow1Space=TRUE)
    two.forest.plots(plot.data, plot.data.cum, outpath=forest.path)

    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Cumulative Summary"=cum.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}


##################################
#  diagnostic leave-one-out MA   #
##################################
loo.ma.diagnostic <- function(fname, diagnostic.data, params){
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    
    loo.results <- array(list(NULL), dim=c(length(diagnostic.data@study.names)))
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
        loo.results[[i]] <- cur.overall
    }
    study.names <- NULL 
    for (count in 1:length(diagnostic.data@study.names)) {
        study.names <- c(study.names, paste("- ",diagnostic.data@study.names[count], sep=""))
    }
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "diagnostic.fixed") {
        model.title <- paste("Diagnostic Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "diagnostic.random") {
        model.title <- paste("Diagnostic Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    
    loo.disp <- create.overall.display(res=loo.results, study.names, params, model.title, data.type="diagnostic")
    forest.path <- paste(params$fp_outpath, sep="")
    
    plot.data <- create.plot.data.overall(res=loo.results, study.names, params, data.type="diagnostic", addRow1Space=FALSE)
    forest.plot(forest.data=plot.data, outpath=forest.path)
    

    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     

    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Leave-one-out Summary"=loo.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

##################################
#  continuous cumulative MA      #
##################################
cum.ma.continuous <- function(fname, cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    res<-rma.uni(yi=cont.data@y, sei=cont.data@SE, slab=cont.data@study.names,
                                level=params$conf.level, digits=params$digits, method="FE", add=params$adjust,
                                to=params$to)
    
    plot.data <- create.plot.data.continuous(cont.data, params, res)
    
    params$fp_show_col3 <- FALSE
    params$fp_show_col4 <- FALSE
    # cumulative plot does not display raw data
    params$fp_col1_str <- "Cumulative Studies"
    
    # iterate over the continuousData elements, adding one study at a time
    cum.results <- array(list(NULL), dim=c(length(cont.data@study.names)))
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
        cum.results[[i]] <- cur.overall
    }
    study.names <- cont.data@study.names[1] 
    for (count in 2:length(cont.data@study.names)) {
        study.names <- c(study.names, paste("+ ",cont.data@study.names[count], sep=""))
    }
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "continuous.fixed") {
        model.title <- paste("Continuous Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "continuous.random") {
        model.title <- paste("Continuous Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    cum.disp <- create.overall.display(res=cum.results, study.names, params, model.title, data.type="continuous")
    forest.path <- paste(params$fp_outpath, sep="")
    params$fp_col1_str <- "Cumulative Studies"
    # label for the cumulative (right-hand) plot
    plot.data.cum <- create.plot.data.overall(res=cum.results, study.names, params, data.type="continuous", addRow1Space=TRUE)
    two.forest.plots(plot.data, plot.data.cum, outpath=forest.path)
    
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Cumulative Summary"=cum.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

##################################
#  continuous leave-one-out MA   #
##################################
loo.ma.continuous <- function(fname, cont.data, params){
    # assert that the argument is the correct type
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    
    loo.results <- array(list(NULL), dim=c(length(cont.data@study.names)))
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
        loo.results[[i]] <- cur.overall
    }
    study.names <- NULL
    for (count in 1:length(cont.data@study.names)) {
        study.names <- c(study.names, paste("- ",cont.data@study.names[count], sep=""))
    }
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "continuous.fixed") {
        model.title <- paste("Continuous Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "continuous.random") {
        model.title <- paste("Continuous Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    loo.disp <- create.overall.display(res=loo.results, study.names, params, model.title, data.type="continuous")

    forest.path <- paste(params$fp_outpath, sep="")

    plot.data <- create.plot.data.overall(res=loo.results, study.names, params, data.type="continuous", addRow1Space=FALSE)
    forest.plot(forest.data=plot.data, outpath=forest.path)
    
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Leave-one-out Forest Plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Leave-one-out Summary"=loo.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

########################
#  binary subgroup MA  #
########################

subgroup.ma.binary <- function(fname, binary.data, params){
    # assert that the argument is the correct type
    if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
    cov.name <- as.character(params$cov_name)
    selected.cov <- get.cov(binary.data, cov.name)
    cov.vals <- selected.cov@cov.vals
    params$create.plot <- FALSE
    subgroup.list <- unique(cov.vals)
    grouped.data <- array(list(NULL),c(length(subgroup.list)+1))
    subgroup.results <- array(list(NULL), c(length(subgroup.list)+1))
    col3.nums <- NULL
    col3.denoms <- NULL
    col4.nums <- NULL
    col4.denoms <- NULL
    count <- 1
    for (i in subgroup.list){
      # build a BinaryData object for each subgroup
      bin.data.tmp <- get.subgroup.data.binary(binary.data, i, cov.vals)
      grouped.data[[count]] <- bin.data.tmp
      # collect raw data columns
      col3.nums <- c(col3.nums, bin.data.tmp@g1O1, sum(bin.data.tmp@g1O1)) 
      col3.denoms <- c(col3.denoms, bin.data.tmp@g1O1 + bin.data.tmp@g1O2, sum(bin.data.tmp@g1O1 + bin.data.tmp@g1O2)) 
      col4.nums <- c(col4.nums, bin.data.tmp@g2O1, sum(bin.data.tmp@g2O1)) 
      col4.denoms <- c(col4.denoms, bin.data.tmp@g2O1 + bin.data.tmp@g2O2, sum(bin.data.tmp@g2O1 + bin.data.tmp@g2O2)) 
      cur.res <- eval(call(fname, bin.data.tmp, params))
      cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
      subgroup.results[[count]] <- cur.overall
      count <- count + 1
    }
    res <- eval(call(fname, binary.data, params))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    grouped.data[[count]] <- binary.data
    subgroup.results[[count]] <- res.overall
    subgroup.names <- paste("Subgroup ", subgroup.list, sep="")
    subgroup.names <- c(subgroup.names, "Overall")
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "binary.fixed.inv.var") {
        model.title <- paste("Binary Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "binary.fixed.mh") {
        model.title <- paste("Binary Fixed-effect Model - Mantel Haenszel\n\nMetric: ", metric.name, sep="")
    } else if (fname == "binary.fixed.peto") {
        model.title <- paste("Binary Fixed-effect Model - Peto\n\nMetric: ", metric.name, sep="")
    } else if (fname == "binary.random") {
        model.title <- paste("Binary Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    subgroup.disp <- create.overall.display(subgroup.results, subgroup.names, params, model.title, data.type="binary")
    forest.path <- paste(params$fp_outpath, sep="")
    # pack up the data for forest plot.
    subgroup.data <- list("subgroup.list"=subgroup.list, "grouped.data"=grouped.data, "results"=subgroup.results, 
                          "col3.nums"=col3.nums, "col3.denoms"=col3.denoms, "col4.nums"=col4.nums, "col4.denoms"=col4.denoms)
    plot.data <- create.subgroup.plot.data.binary(subgroup.data, params)
    forest.plot(forest.data=plot.data, outpath=forest.path)

    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Subgroup Forest Plot"=forest.path)
    plot.names <- c("subgroups forest plot"="subgroups_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Subgroup Summary"=subgroup.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

get.subgroup.data.binary <- function(binary.data, cov.val, cov.vals) {
  # returns the subgroup data corresponding to a categorical covariant 
  # for value cov.val
  if (!("BinaryData" %in% class(binary.data))) stop("Binary data expected.")
  y.tmp <- binary.data@y[cov.vals == cov.val]
  SE.tmp <- binary.data@SE[cov.vals == cov.val]
  names.tmp <- binary.data@study.names[cov.vals == cov.val]
  if (length(binary.data@g1O1) > 0){
    g1O1.tmp <- binary.data@g1O1[cov.vals == cov.val]
    g1O2.tmp <- binary.data@g1O2[cov.vals == cov.val]
    g2O1.tmp <- binary.data@g2O1[cov.vals == cov.val]
    g2O2.tmp <- binary.data@g2O2[cov.vals == cov.val]
    subgroup.data <- new('BinaryData', g1O1=g1O1.tmp, 
                          g1O2=g1O2.tmp, g2O1=g2O1.tmp, 
                          g2O2=g2O2.tmp, y=y.tmp, SE=SE.tmp, study.names=names.tmp)
  } else {
    subgroup.data <- new('BinaryData', y=y.tmp, SE=SE.tmp, study.names=names.tmp)
  }
  subgroup.data
}

############################
#  diagnostic subgroup MA  #
############################

subgroup.ma.diagnostic <- function(fname, diagnostic.data, params){
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    cov.name <- as.character(params$cov_name)
    selected.cov <- get.cov(diagnostic.data, cov.name)
    cov.vals <- selected.cov@cov.vals
    params$create.plot <- FALSE
    subgroup.list <- unique(cov.vals)
    grouped.data <- array(list(NULL),c(length(subgroup.list) + 1))
    subgroup.results <- array(list(NULL), c(length(subgroup.list) + 1))
    col3.nums <- NULL
    col3.denoms <- NULL
    col4.nums <- NULL
    col4.denoms <- NULL
    count <- 1
    for (i in subgroup.list){
      # build a DiagnosticData object 
      diag.data.tmp <- get.subgroup.data.diagnostic(diagnostic.data, i, cov.vals)
      grouped.data[[count]] <- diag.data.tmp
      # collect raw data columns
      raw.data <- list("TP"=diag.data.tmp@TP, "FN"=diag.data.tmp@FN, "TN"=diag.data.tmp@TN, "FP"=diag.data.tmp@FP)
      terms <- compute.diagnostic.terms(raw.data, params)
      col3.nums <- c(col3.nums, terms$numerator, sum(terms$numerator))
      col3.denoms <- c(col3.denoms, terms$denominator, sum(terms$denominator))
      cur.res <- eval(call(fname, diag.data.tmp, params))
      cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
      subgroup.results[[count]] <- cur.overall
      count <- count + 1
    }
    res <- eval(call(fname, diagnostic.data, params))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    grouped.data[[count]] <- diagnostic.data
    subgroup.results[[count]] <- res.overall
    subgroup.names <- paste("Subgroup ", subgroup.list, sep="")
    subgroup.names <- c(subgroup.names, "Overall")
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "diagnostic.fixed") {
        model.title <- paste("Diagnostic Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "diagnostic.random") {
        model.title <- paste("Diagnostic Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    subgroup.disp <- create.overall.display(subgroup.results, subgroup.names, params, model.title, data.type="diagnostic")
    forest.path <- paste(params$fp_outpath, sep="")
    # pack up the data for forest plot.
    subgroup.data <- list("subgroup.list"=subgroup.list, "grouped.data"=grouped.data, "results"=subgroup.results, 
                          "col3.nums"=col3.nums, "col3.denoms"=col3.denoms, "col4.nums"=col4.nums, "col4.denoms"=col4.denoms)
    plot.data <- create.subgroup.plot.data.diagnostic(subgroup.data, params)
    forest.plot(forest.data=plot.data, outpath=forest.path)
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Subgroups Forest Plot"=forest.path)
    plot.names <- c("subgroups forest plot"="subgroups_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Subgroup Summary"=subgroup.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

get.subgroup.data.diagnostic <- function(diagnostic.data, cov.val, cov.vals) {
  # returns the subgroup data corresponding to a categorical covariant cov.name
  # and value cov.val
  if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
  y.tmp <- diagnostic.data@y[cov.vals == cov.val]
  SE.tmp <- diagnostic.data@SE[cov.vals == cov.val]
  names.tmp <- diagnostic.data@study.names[cov.vals == cov.val]
  if (length(diagnostic.data@TP) > 0){
    TP.tmp <- diagnostic.data@TP[cov.vals==cov.val]
    FN.tmp <- diagnostic.data@FN[cov.vals==cov.val]
    TN.tmp <- diagnostic.data@TN[cov.vals==cov.val]
    FP.tmp <- diagnostic.data@FP[cov.vals==cov.val]
    subgroup.data <- new('DiagnosticData', TP=TP.tmp, 
                          FN=FN.tmp , TN=TN.tmp, 
                          FP=FP.tmp, y=y.tmp, SE=SE.tmp, study.names=names.tmp)
  } else {
    subgroup.data <- new('DiagnosticData', y=y.tmp, SE=SE.tmp, study.names=names.tmp)
  }
  subgroup.data
}

#############################
#  continuous subgroup MA  #
#############################

subgroup.ma.continuous <- function(fname, cont.data, params){
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    cov.name <- as.character(params$cov_name)
    selected.cov <- get.cov(cont.data, cov.name)
    cov.vals <- selected.cov@cov.vals
    params$create.plot <- FALSE
    subgroup.list <- unique(cov.vals)
    grouped.data <- array(list(NULL),c(length(subgroup.list)+1))
    subgroup.results <- array(list(NULL), c(length(subgroup.list)+1))
    col3.nums <- NULL
    col3.denoms <- NULL
    col4.nums <- NULL
    col4.denoms <- NULL
    count <- 1
    for (i in subgroup.list){
      # build a ContinuousData object 
      cont.data.tmp <- get.subgroup.data.cont(cont.data, i, cov.vals) 
      grouped.data[[count]] <- cont.data.tmp
      cur.res <- eval(call(fname, cont.data.tmp, params))
      cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
      subgroup.results[[count]] <- cur.overall
      count <- count + 1
    }
    res <- eval(call(fname, cont.data, params))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    grouped.data[[count]] <- cont.data
    subgroup.results[[count]] <- res.overall
    subgroup.names <- paste("Subgroup ", subgroup.list, sep="")
    subgroup.names <- c(subgroup.names, "Overall")
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "continuous.fixed") {
        model.title <- paste("Continuous Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "continuous.random") {
        model.title <- paste("Continuous Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    subgroup.disp <- create.overall.display(subgroup.results, subgroup.names, params, model.title, data.type="continuous")
    forest.path <- paste(params$fp_outpath, sep="")
    # pack up the data for forest plot.
    subgroup.data <- list("subgroup.list"=subgroup.list, "grouped.data"=grouped.data, "results"=subgroup.results, 
                          "col3.nums"=col3.nums, "col3.denoms"=col3.denoms, "col4.nums"=col4.nums, "col4.denoms"=col4.denoms)
    plot.data <- create.subgroup.plot.data.cont(subgroup.data, params)
    forest.plot(forest.data=plot.data, outpath=forest.path)
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    images <- c("Subgroups Forest Plot"=forest.path)
    plot.names <- c("subgroups forest plot"="subgroups_forest_plot")
    
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.plot.data(plot.data)
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    results <- list("images"=images, "Subgroup Summary"=subgroup.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    results
}

get.subgroup.data.cont <- function(cont.data, cov.val, cov.vals) {
  # returns the subgroup data corresponding to a categorical covariant cov.name
  # and value cov.val
  if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
      y.tmp <- cont.data@y[cov.vals == cov.val]
      SE.tmp <- cont.data@SE[cov.vals == cov.val]
      names.tmp <- cont.data@study.names[cov.vals == cov.val]
  if (length(cont.data@N1) > 0){
      N1.tmp <- cont.data@N1[cov.vals == cov.val]
      mean1.tmp <- cont.data@mean1[cov.vals == cov.val]
      sd1.tmp <- cont.data@sd1[cov.vals == cov.val]
      N2.tmp <- cont.data@N2[cov.vals == cov.val]
      mean2.tmp <- cont.data@mean2[cov.vals == cov.val]
      sd2.tmp <- cont.data@sd2[cov.vals == cov.val]
      subgroup.data <- new('ContinuousData', 
                          N1=N1.tmp, mean1=mean1.tmp , sd1=sd1.tmp, 
                          N2=N2.tmp, mean2=mean2.tmp, sd2=sd2.tmp,
                          y=y.tmp, SE=SE.tmp, 
                          study.names=names.tmp)
    } else {
    subgroup.data <- new('ContinuousData', 
                          y=y.tmp, SE=SE.tmp, 
                          study.names=names.tmp)
    }
    subgroup.data
}

get.cov <- function(om.data, cov.name) {
    # extracts the covariate with specified name from om.data
    covariate <- NULL
    count <- 1
    while ((count <= length(om.data@covariates)) & (is.null(covariate))) {
        if (om.data@covariates[[count]]@cov.name == cov.name) {
            covariate <- om.data@covariates[[count]]
        }
        count <- count + 1
    }
    covariate
}

update.plot.data.multiple <- function(binary.data, params, results) {

    scale.str <- "standard"
    if (metric.is.log.scale(as.character(params$measure))){
        scale.str <- "log"
    }
    transform.name <- "binary.transform.f"
    data.type <- "binary"
    plot.options <- extract.plot.options(params)
    if (!is.null(params$fp_display.lb)) {
        plot.options$display.lb <- eval(call(transform.name, params$measure))$calc.scale(params$fp_display.lb)
    }
    if (!is.null(params$fp_display.ub)) {
        plot.options$display.ub <- eval(call(transform.name, params$measure))$calc.scale(params$fp_display.ub)
    }
    if (!is.null(params$fp_show.summary.line)) {
        plot.options$show.summary.line <- params$fp_show_summary_line
    } else {
        plot.options$show.summary.line <- TRUE
    }
    # plot options passed in via params
    plot.data <- list(label = c(paste(params$fp_col1_str, sep = ""), binary.data@study.names, "Overall"),
                    types = c(3, rep(0, length(binary.data@study.names)), 2),
                    scale = scale.str,
                    data.type = data.type,
                    overall =FALSE,
                    options = plot.options)
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    y.overall <- res$b[1]
    lb.overall <- res$ci.lb[1]
    ub.overall <- res$ci.ub[1]
     y <- binary.data@y
    lb <- y - mult*binary.data@SE
    ub <- y + mult*binary.data@SE

    y <- c(y, y.overall)
    lb <- c(lb, lb.overall)
    ub <- c(ub, ub.overall)

    # transform entries to display scale
    y.disp <- eval(call(transform.name, params$measure))$display.scale(y)
    lb.disp <- eval(call(transform.name, params$measure))$display.scale(lb)
    ub.disp <- eval(call(transform.name, params$measure))$display.scale(ub)

    if (params$fp_show_col2=='TRUE') {
        # format entries for text column in forest plot
        effect.size.col <- format.effect.size.col(y.disp, lb.disp, ub.disp, params)
        plot.data$additional.col.data$es <- effect.size.col
    }
    if (scale.str == "log") {
        # if metric is log scale, pass effect sizes in log scale.
        effects <- list(ES = y,
                    LL = lb,
                    UL = ub)
    } else {
        # otherwise pass effect sizes in standard scale
        effects <- list(ES = y.disp,
                    LL = lb.disp,
                    UL = ub.disp)
    }
    plot.data$effects <- effects
    # covariates
    if (!is.null(selected.cov)){
        cov.val.str <- paste("binary.data@covariates$", selected.cov, sep="")
        cov.values <- eval(parse(text=cov.val.str))
        plot.data$covariate <- list(varname = selected.cov,
                                   values = cov.values)
    }
    plot.data$fp_xlabel <- paste(params$fp_xlabel, sep = "")
    plot.data$fp_xticks <- params$fp_xticks
    plot.data
}
    