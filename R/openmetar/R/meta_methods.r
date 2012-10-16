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
    
    params.tmp <- params
    # These temporarily turn off creating plots and writing results to file
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
    res <- eval(call(fname, binary.data, params.tmp))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    # parse out the overall estimate
    plot.data <- create.plot.data.binary(binary.data, params, res.overall)
    # data for standard forest plot
    
    # iterate over the binaryData elements, adding one study at a time
    cum.results <- array(list(NULL), dim=c(length(binary.data@study.names)))
    
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
        cur.res <- eval(call(fname, bin.data.tmp, params.tmp))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[[i]] <- cur.overall 
    }
    study.names <- binary.data@study.names[1] 
    for (count in 2:length(binary.data@study.names)) {
        study.names <- c(study.names, paste("+ ",binary.data@study.names[count], sep=""))
    }
    metric.name <- pretty.metric.name(as.character(params.tmp$measure))
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
    plot.data.cum <- create.plot.data.cum(om.data=binary.data, params.cum, res=cum.results)
    two.plot.data <- list("left"=plot.data, "right"=plot.data.cum)
    changed.params <- plot.data$changed.params
    # List of changed params values for standard (left) plot - not cumulative plot!
    # Currently plot edit can't handle two sets of params values for xticks or plot bounds.
    # Could be changed in future.
    params.changed.in.forest.plot <- two.forest.plots(two.plot.data, outpath=forest.path)
    changed.params <- c(changed.params, params.changed.in.forest.plot)
    # Update params changed in two.forest.plots
    params[names(changed.params)] <- changed.params
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.data(binary.data, res, params, two.plot.data)
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #    
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative_forest_plot")
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
    
    loo.results <- array(list(NULL), dim=c(length(binary.data@study.names)))
    params.tmp <- params
    
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
    # don't create plots when calling individual binary methods
    res <- eval(call(fname, binary.data, params.tmp))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
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
        cur.res <- eval(call(fname, bin.data.tmp, params.tmp))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[[i]] <- cur.overall
    }
    loo.results <- c(list(res.overall), loo.results)
    # Add overall results
    study.names <- c("Overall", paste("- ",binary.data@study.names, sep=""))
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
    plot.data <- create.plot.data.loo(binary.data, params, res=loo.results)
    changed.params <- plot.data$changed.params
    # list of changed params values
    params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
    changed.params <- c(changed.params, params.changed.in.forest.plot)
    params[names(changed.params)] <- changed.params
    # update params values
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.data(binary.data, res=loo.results, params, plot.data)
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    images <- c("Leave-one-out Forest plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
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
    
    params.tmp <- params
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
    res <- eval(call(fname, cont.data, params.tmp))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    # parse out the overall estimate
    plot.data <- create.plot.data.continuous(cont.data, params, res=res.overall)
    # data for standard forest plot
    
    params$fp_show_col3 <- FALSE
    params$fp_show_col4 <- FALSE
    # cumulative plot does not display raw data
    params$fp_col1_str <- "Cumulative Studies"
    
    # iterate over the continuousData elements, adding one study at a time
    cum.results <- array(list(NULL), dim=c(length(cont.data@study.names)))
    
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
        cur.res <- eval(call(fname, cont.data.tmp, params.tmp))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        cum.results[[i]] <- cur.overall
    }
    study.names <- c()
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
    params.cum <- params
    params.cum$fp_col1_str <- "Cumulative Studies"
    params.cum$fp_col2_str <- "Cumulative Estimate"
    plot.data.cum <- create.plot.data.cum(om.data=cont.data, params.cum, res=cum.results)
    two.plot.data <- list("left"=plot.data, "right"=plot.data.cum)
    changed.params <- plot.data$changed.params
    # List of changed params values for standard (left) plot - not cumulative plot!
    # Currently plot edit can't handle two sets of params values for xticks or plot bounds.
    # Could be changed in future.
    params.changed.in.forest.plot <- two.forest.plots(two.plot.data, outpath=forest.path)
    changed.params <- c(changed.params, params.changed.in.forest.plot)
    # Update params changed in two.forest.plots
    params[names(changed.params)] <- changed.params
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.data(cont.data, res=cum.results, params, two.plot.data)
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    images <- c("Cumulative Forest Plot"=forest.path)
    plot.names <- c("cumulative forest plot"="cumulative forest_plot")
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
    params.tmp <- params
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
    res <- eval(call(fname, cont.data, params.tmp))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    N <- length(cont.data@study.names)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index.ls <- setdiff(1:N, i)
        
        # build a ContinuousData object with the 
        # ith study removed.  
        y.tmp <- cont.data@y[index.ls]
        SE.tmp <- cont.data@SE[index.ls]
        names.tmp <- cont.data@study.names[index.ls]
        bin.data.tmp <- NULL
        
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
        cur.res <- eval(call(fname, cont.data.tmp, params.tmp))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[[i]] <- cur.overall
    }
    loo.results <- c(list(res.overall), loo.results)
    # Add overall results
    study.names <- c("Overall", paste("- ", cont.data@study.names, sep=""))
    params$data.type <- "continuous"
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "continuous.fixed") {
        model.title <- paste("Continuous Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "continuous.random") {
        model.title <- paste("Continuous Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    loo.disp <- create.overall.display(res=loo.results, study.names, params, model.title, data.type="continuous")
    forest.path <- paste(params$fp_outpath, sep="")
    plot.data <- create.plot.data.loo(cont.data, params, res=loo.results)
    changed.params <- plot.data$changed.params
    # list of changed params values
    params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
    changed.params <- c(changed.params, params.changed.in.forest.plot)
    params[names(changed.params)] <- changed.params
    # update params values
     # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.data(cont.data, res=loo.results, params, plot.data)
    #
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    images <- c("Leave-one-out Forest Plot"=forest.path)
    plot.names <- c("loo forest plot"="loo_forest_plot")
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
    params.tmp <- params
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
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
      cur.res <- eval(call(fname, bin.data.tmp, params.tmp))
      cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
      subgroup.results[[count]] <- cur.overall
      count <- count + 1
    }
    res <- eval(call(fname, binary.data, params.tmp))
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
    subgroup.disp <- create.subgroup.display(subgroup.results, subgroup.names, params, model.title, data.type="binary")
    forest.path <- paste(params$fp_outpath, sep="")
    # pack up the data for forest plot.
    subgroup.data <- list("subgroup.list"=subgroup.list, "grouped.data"=grouped.data, "results"=subgroup.results, 
                          "col3.nums"=col3.nums, "col3.denoms"=col3.denoms, "col4.nums"=col4.nums, "col4.denoms"=col4.denoms)
    plot.data <- create.subgroup.plot.data.binary(subgroup.data, params)
    changed.params <- plot.data$changed.params
    # list of changed params values
    params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
    changed.params <- c(changed.params, params.changed.in.forest.plot)
    params[names(changed.params)] <- changed.params
    # update params values
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.data(binary.data, res, params, plot.data)
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #     
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    images <- c("Subgroup Forest Plot"=forest.path)
    plot.names <- c("subgroups forest plot"="subgroups_forest_plot")
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

#############################
#  continuous subgroup MA  #
#############################

subgroup.ma.continuous <- function(fname, cont.data, params){
    if (!("ContinuousData" %in% class(cont.data))) stop("Continuous data expected.")
    params.tmp <- params
    cov.name <- as.character(params$cov_name)
    selected.cov <- get.cov(cont.data, cov.name)
    cov.vals <- selected.cov@cov.vals
    params$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
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
    changed.params <- plot.data$changed.params
    # list of changed params values
    params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
    changed.params <- c(changed.params, params.changed.in.forest.plot)
    params[names(changed.params)] <- changed.params
    # update params values
    # we use the system time as our unique-enough string to store
    # the params object
    forest.plot.params.path <- save.data(cont.data, res, params, plot.data)
    # Now we package the results in a dictionary (technically, a named 
    # vector). In particular, there are two fields that must be returned; 
    # a dictionary of images (mapping titles to image paths) and a list of texts
    # (mapping titles to pretty-printed text). In this case we have only one 
    # of each. 
    #    
    plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
    images <- c("Subgroups Forest Plot"=forest.path)
    plot.names <- c("subgroups forest plot"="subgroups_forest_plot")
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
    
###################################################
#     leave-one-out diagnostic methods            #
###################################################
multiple.loo.diagnostic <- function(fnames, params.list, diagnostic.data) {

    # wrapper for applying leave-one-out method to multiple diagnostic functions and metrics    

    ####
    # fnames -- names of diagnostic meta-analytic functions to call
    # params.list -- parameter lists to be passed along to the functions in
    #              fnames
    # diagnostic.data -- the (diagnostic data) that is to be analyzed 
    ###
    metrics <- c()
    results <- list()
    pretty.names <- diagnostic.fixed.inv.var.pretty.names()
    sens.spec.outpath <- c()
    for (count in 1:length(params.list)) {
        metrics <- c(metrics, params.list[[count]]$measure)
        if (params.list[[count]]$measure=="Sens") {
            sens.index <- count
            #sens.spec.outpath <- params.list[[count]]$fp_outpath
        }
        if (params.list[[count]]$measure=="Spec") {
            spec.index <- count
            #sens.spec.outpath <- params.list[[count]]$fp_outpath
        }
        if (params.list[[count]]$measure=="PLR") {
            plr.index <- count
            #if (params.list[[count]]$fp_outpath==sens.spec.outpath) {
            # for future use - check that path names are distinct.    
            #    params.list[[count]]$fp_outpath <- paste(sub(".png","",sens.spec.outpath), "1.png", sep="")   
                # if fp_outpath is the same as for sens or spec, append a 1.
            #}
        }
        if (params.list[[count]]$measure=="NLR") {
            nlr.index <- count
            #if (params.list[[count]]$fp_outpath==sens.spec.outpath) {
            #    params.list[[count]]$fp_outpath <- paste(sub(".png","",sens.spec.outpath), "1.png", sep="")   
            #    # if fp_outpath is the same as for sens or spec, append a 1.
            #}
        }
    }
    
    images <- c()
    plot.names <- c()
    plot.params.paths <- c()
    remove.indices <- c()

    if (("Sens" %in% metrics) & ("Spec" %in% metrics)) {
        # create side-by-side forest plots for sens and spec.
        params.sens <- params.list[[sens.index]]
        params.spec <- params.list[[spec.index]]
        params.sens$create.plot <- FALSE
        params.sens$write.to.file <- FALSE
        params.spec$create.plot <- FALSE
        params.spec$write.to.file <- FALSE
        params.tmp <- list("left"=params.sens, "right"=params.spec)
        
        fname <- fnames[sens.index]
        diagnostic.data.sens <- compute.diag.point.estimates(diagnostic.data, params.sens)
        diagnostic.data.spec <- compute.diag.point.estimates(diagnostic.data, params.spec)
        
        results.sens <- loo.ma.diagnostic(fname, diagnostic.data.sens, params.sens)
        results.spec <- loo.ma.diagnostic(fname, diagnostic.data.spec, params.spec)

        diagnostic.data.sens.spec <- list("left"=diagnostic.data.sens, "right"=diagnostic.data.spec)
        
        summary.sens <- list("Summary"=results.sens$Summary)
        names(summary.sens) <- paste(eval(parse(text=paste("pretty.names$measure$", params.sens$measure,sep=""))), " Summary", sep="")
        summary.spec <- list("Summary"=results.spec$Summary)
        names(summary.spec) <- paste(eval(parse(text=paste("pretty.names$measure$", params.spec$measure,sep=""))), " Summary", sep="")
        results <- c(results, summary.sens, summary.spec)
        
        res.sens.spec <- list("left"=results.sens$res, "right"=results.spec$res)
        plot.data <- create.loo.side.by.side.plot.data(diagnostic.data.sens.spec, params.tmp, res=res.sens.spec)
        
        forest.path <- paste(params.sens$fp_outpath, sep="")
        two.forest.plots(plot.data, outpath=forest.path)
           
        forest.plot.params.path <- save.data(om.data=diagnostic.data.sens.spec, res.sens.spec, params=params.tmp, plot.data)
        plot.params.paths.tmp <- c("Sensitivity and Specificity Forest Plot"=forest.plot.params.path)
        plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
               
        images.tmp <- c("Sensitivity and Specificity Forest Plot"=forest.path)
        images <- c(images, images.tmp)
        
        plot.names.tmp <- c("forest plot"="forest.plot")
        plot.names <- c(plot.names, plot.names.tmp)
     
        remove.indices <- c(sens.index, spec.index)
    }
    
    if (("NLR" %in% metrics) & ("PLR" %in% metrics)) {
        # create side-by-side forest plots for NLR and PLR.
        params.nlr <- params.list[[nlr.index]]
        params.plr <- params.list[[plr.index]]
        params.nlr$create.plot <- FALSE
        params.nlr$write.to.file <- FALSE
        params.plr$create.plot <- FALSE
        params.plr$write.to.file <- FALSE
        params.tmp <- list("left"=params.nlr, "right"=params.plr)
        
        fname <- fnames[nlr.index]
        diagnostic.data.nlr <- compute.diag.point.estimates(diagnostic.data, params.nlr)
        diagnostic.data.plr <- compute.diag.point.estimates(diagnostic.data, params.plr)
        results.nlr <- loo.ma.diagnostic(fname, diagnostic.data.nlr, params.nlr)
        results.plr <- loo.ma.diagnostic(fname, diagnostic.data.plr, params.plr)
        diagnostic.data.nlr.plr <- list("left"=diagnostic.data.nlr, "right"=diagnostic.data.plr)
        
        summary.nlr <- list("Summary"=results.nlr$Summary)
        names(summary.nlr) <- paste(eval(parse(text=paste("pretty.names$measure$", params.nlr$measure,sep=""))), " Summary", sep="")
        summary.plr <- list("Summary"=results.plr$Summary)
        names(summary.plr) <- paste(eval(parse(text=paste("pretty.names$measure$", params.plr$measure,sep=""))), " Summary", sep="")
        results <- c(results, summary.nlr, summary.plr)
        
        res.nlr.plr <- list("left"=results.nlr$res, "right"=results.plr$res)
        plot.data <- create.loo.side.by.side.plot.data(diagnostic.data.nlr.plr, params.tmp, res=res.nlr.plr)
        
        forest.path <- paste(params.nlr$fp_outpath, sep="")
        two.forest.plots(plot.data, outpath=forest.path)
           
        forest.plot.params.path <- save.data(diagnostic.data.nlr.plr, res.nlr.plr, params=params.tmp, plot.data)
        # @TODO: If you want to edit the plot, need to also 
        plot.params.paths.tmp <- c("NLR and PLR Forest Plot"=forest.plot.params.path)
        plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
               
        images.tmp <- c("NLR and PLR Forest Plot"=forest.path)
        images <- c(images, images.tmp)
        
        plot.names.tmp <- c("forest plot"="forest.plot")
        plot.names <- c(plot.names, plot.names.tmp)
        
        remove.indices <- c(remove.indices, nlr.index, plr.index)
    }

    # remove fnames and params for side-by-side plots
    fnames <- fnames[setdiff(1:length(fnames), remove.indices)]
    params.list <- params.list[setdiff(1:length(params.list), remove.indices)]

    if (length(params.list) > 0) {
        for (count in 1:length(params.list)) {
            # create ma summaries and single (not side-by-side) forest plots.
            #pretty.names <- eval(call(paste(fnames[count],".pretty.names",sep="")))
            diagnostic.data.tmp <- compute.diag.point.estimates(diagnostic.data, params.list[[count]])
            results.tmp <- loo.ma.diagnostic(fnames[[count]], diagnostic.data.tmp, params.list[[count]])
            #if (is.null(params.list[[count]]$create.plot)) {
            # create plot
            images.tmp <- results.tmp$images
            names(images.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$",params.list[[count]]$measure,sep=""))), " Forest Plot", sep="")
            images <- c(images, images.tmp)
            plot.params.paths.tmp <- results.tmp$plot_params_paths
            names(plot.params.paths.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$", params.list[[count]]$measure,sep=""))), " Forest Plot", sep="")
            plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
            plot.names <- c(plot.names, results.tmp$plot.names)
            #}
            summary.tmp <- list("Summary"=results.tmp$Summary)
            names(summary.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$",params.list[[count]]$measure,sep=""))), " Summary", sep="")
            results <- c(results, summary.tmp)
        }
    }
    results <- c(results, list("images"=images, "plot_names"=plot.names, 
                               "plot_params_paths"=plot.params.paths))
    #results$images <- images
    #results$plot.names <- plot.names
    #results$plot.params.paths <- plot.params.paths
    results
}

loo.ma.diagnostic <- function(fname, diagnostic.data, params){
    # performs a single leave-one-out meta-analysis for diagnostic.data
    # assert that the argument is the correct type
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    loo.results <- array(list(NULL), dim=c(length(diagnostic.data@study.names)))
    params.tmp <- params
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
    res <- eval(call(fname, diagnostic.data, params.tmp))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    N <- length(diagnostic.data@study.names)
    for (i in 1:N){
        # get a list of indices, i.e., the subset
        # that is 1:N with i left out
        index.ls <- setdiff(1:N, i)
        
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
        cur.res <- eval(call(fname, diag.data.tmp, params.tmp))
        cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
        loo.results[[i]] <- cur.overall
    }
    loo.results <- c(list(res.overall), loo.results)
    # Add overall results
    study.names <- c("Overall", paste("- ", diagnostic.data@study.names, sep=""))
    metric.name <- pretty.metric.name(as.character(params$measure))
    model.title <- ""
    if (fname == "diagnostic.fixed") {
        model.title <- paste("Diagnostic Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "diagnostic.random") {
        model.title <- paste("Diagnostic Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    
    loo.disp <- create.overall.display(res=loo.results, study.names, params, model.title, data.type="diagnostic")
        
    if (is.null(params$create.plot)) {
        plot.data <- create.plot.data.loo(diagnostic.data, params, res=loo.results)
        forest.path <- paste(params$fp_outpath, sep="")
        changed.params <- plot.data$changed.params
        # list of changed params values
        params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
        changed.params <- c(changed.params, params.changed.in.forest.plot)
        params[names(changed.params)] <- changed.params
        # update params values
        # we use the system time as our unique-enough string to store
        # the params object
        forest.plot.params.path <- save.data(diagnostic.data, res=loo.results, params, plot.data)
        #
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #     
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Leave-one-out Forest plot"=forest.path)
        plot.names <- c("loo forest plot"="loo_forest_plot")
        results <- list("images"=images, "Summary"=loo.disp, 
                        "plot_names"=plot.names, 
                        "plot_params_paths"=plot.params.paths)
    } else {
        results <- list(res=loo.results, res.overall=res.overall, Summary=loo.disp) 
    }  
    results
}

create.loo.side.by.side.plot.data <- function(diagnostic.data, params, res) {    
    # creates data for two side-by-side leave-one-out forest plots
    params.left <- params$left
    params.right <- params$right
    params.left$fp_show_col1 <- 'TRUE'
    params.right$fp_show_col1 <- 'FALSE'
    # only show study names on the left plot
    res.left <- res$left
    res.right <- res$right 
    diagnostic.data.left <- diagnostic.data$left
    diagnostic.data.right <- diagnostic.data$right
    study.names <- c("Overall", paste("- ", diagnostic.data.left@study.names, sep=""))
    plot.data.left <- create.plot.data.loo(diagnostic.data.left, params.left, res.left)
    plot.data.left$options$fp.title <- pretty.metric.name(as.character(params.left$measure))
    plot.data.right <- create.plot.data.loo(diagnostic.data.right, params.right, res.right)
    plot.data.right$options$fp.title <- pretty.metric.name(as.character(params.right$measure))
    plot.data <- list("left"=plot.data.left, "right"=plot.data.right)
    plot.data
}

#################################
#  subgroup diagnostic methods  #
#################################

multiple.subgroup.diagnostic <- function(fnames, params.list, diagnostic.data) {

    # wrapper for applying subgroup method to multiple diagnostic functions and metrics    

    ####
    # fnames -- list of names of diagnostic meta-analytic functions to call
    # params.list -- list parameter lists to be passed along to the functions in
    #              fnames
    # diagnostic.data -- the (diagnostic data) that is to be analyzed 
    ###
    metrics <- c()
    results <- list()
    pretty.names <- diagnostic.fixed.inv.var.pretty.names()
    sens.spec.outpath <- c()
    for (count in 1:length(params.list)) {
        metrics <- c(metrics, params.list[[count]]$measure)
        if (params.list[[count]]$measure=="Sens") {
            sens.index <- count
            #sens.spec.outpath <- params.list[[count]]$fp_outpath
        }
        if (params.list[[count]]$measure=="Spec") {
            spec.index <- count
            #sens.spec.outpath <- params.list[[count]]$fp_outpath
        }
        if (params.list[[count]]$measure=="PLR") {
            plr.index <- count
            #if (params.list[[count]]$fp_outpath==sens.spec.outpath) {
            # for future use - check that path names are distinct.    
            #    params.list[[count]]$fp_outpath <- paste(sub(".png","",sens.spec.outpath), "1.png", sep="")   
                # if fp_outpath is the same as for sens or spec, append a 1.
            #}
        }
        if (params.list[[count]]$measure=="NLR") {
            nlr.index <- count
            #if (params.list[[count]]$fp_outpath==sens.spec.outpath) {
            #    params.list[[count]]$fp_outpath <- paste(sub(".png","",sens.spec.outpath), "1.png", sep="")   
            #    # if fp_outpath is the same as for sens or spec, append a 1.
            #}
        }
    }
    cov.name <- as.character(params.list[[1]]$cov_name)
    selected.cov <- get.cov(diagnostic.data, cov.name)
    images <- c()
    plot.names <- c()
    plot.params.paths <- c()
    remove.indices <- c()

    if (("Sens" %in% metrics) & ("Spec" %in% metrics)) {
        # create side-by-side subgroup forest plots for sens and spec.
       
        params.sens <- params.list[[sens.index]]
        params.spec <- params.list[[spec.index]]
        params.sens$create.plot <- FALSE
        params.spec$create.plot <- FALSE
        params.tmp <- list("left"=params.sens, "right"=params.spec)
        
        fname <- fnames[sens.index]
        diagnostic.data.sens <- compute.diag.point.estimates(diagnostic.data, params.sens)
        diagnostic.data.spec <- compute.diag.point.estimates(diagnostic.data, params.spec)
        
        results.sens <- subgroup.ma.diagnostic(fname, diagnostic.data.sens, params.sens, selected.cov)
        results.spec <- subgroup.ma.diagnostic(fname, diagnostic.data.spec, params.spec, selected.cov)
        subgroup.data.sens <- results.sens$subgroup.data
        subgroup.data.spec <- results.spec$subgroup.data
        subgroup.data.all <- list("left"=subgroup.data.sens, "right"=subgroup.data.spec)
      
        summary.sens <- list("Summary"=results.sens$Summary)
        names(summary.sens) <- paste(eval(parse(text=paste("pretty.names$measure$", params.sens$measure,sep=""))), " Summary", sep="")
        summary.spec <- list("Summary"=results.spec$Summary)
        names(summary.spec) <- paste(eval(parse(text=paste("pretty.names$measure$", params.spec$measure,sep=""))), " Summary", sep="")
        results <- c(results, summary.sens, summary.spec)
        
        #res.sens <- results.sens$res
        #res.spec <- results.spec$res
        #res <- list("left"=res.sens, "right"=res.spec)
        
        plot.data <- create.subgroup.side.by.side.plot.data(subgroup.data.all, params=params.tmp)
        
        forest.path <- paste(params.sens$fp_outpath, sep="")
        two.forest.plots(plot.data, outpath=forest.path)
           
        forest.plot.params.path <- save.data(subgroup.data.all, params=params.tmp)
        plot.params.paths.tmp <- c("Sensitivity and Specificity Forest Plot"=forest.plot.params.path)
        plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
               
        images.tmp <- c("Sensitivity and Specificity Forest Plot"=forest.path)
        images <- c(images, images.tmp)
        
        plot.names.tmp <- c("forest plot"="forest.plot")
        plot.names <- c(plot.names, plot.names.tmp)
     
        remove.indices <- c(sens.index, spec.index)
    }
    
    if (("NLR" %in% metrics) & ("PLR" %in% metrics)) {
        # create side-by-side forest plots for NLR and PLR.
        params.nlr <- params.list[[nlr.index]]
        params.plr <- params.list[[plr.index]]
        params.nlr$create.plot <- FALSE
        params.plr$create.plot <- FALSE
        params.tmp <- list("left"=params.nlr, "right"=params.plr)
        
        fname <- fnames[nlr.index]
        diagnostic.data.nlr <- compute.diag.point.estimates(diagnostic.data, params.nlr)
        diagnostic.data.plr <- compute.diag.point.estimates(diagnostic.data, params.plr)
        
        results.nlr <- subgroup.ma.diagnostic(fname, diagnostic.data.nlr, params.nlr, selected.cov)
        results.plr <- subgroup.ma.diagnostic(fname, diagnostic.data.plr, params.plr, selected.cov)
        subgroup.data.nlr <- results.nlr$subgroup.data
        subgroup.data.plr <- results.plr$subgroup.data
        subgroup.data.all <- list("left"=subgroup.data.nlr, "right"=subgroup.data.plr)
        
        summary.nlr <- list("Summary"=results.nlr$Summary)
        names(summary.nlr) <- paste(eval(parse(text=paste("pretty.names$measure$", params.nlr$measure,sep=""))), " Summary", sep="")
        summary.plr <- list("Summary"=results.plr$Summary)
        names(summary.plr) <- paste(eval(parse(text=paste("pretty.names$measure$", params.plr$measure,sep=""))), " Summary", sep="")
        results <- c(results, summary.nlr, summary.plr)
        
        #res.nlr <- results.nlr$res
        #res.plr <- results.plr$res
        #res <- list("left"=res.nlr, "right"=res.plr)
        
        plot.data <- create.subgroup.side.by.side.plot.data(subgroup.data.all, params.tmp)
        
        forest.path <- paste(params.nlr$fp_outpath, sep="")
        two.forest.plots(plot.data, outpath=forest.path)
           
        forest.plot.params.path <- save.data(subgroup.data.all, params=params.tmp)
        plot.params.paths.tmp <- c("NLR and PLR Forest Plot"=forest.plot.params.path)
        plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
               
        images.tmp <- c("NLR and PLR Forest Plot"=forest.path)
        images <- c(images, images.tmp)
        
        plot.names.tmp <- c("forest plot"="forest.plot")
        plot.names <- c(plot.names, plot.names.tmp)
        
        remove.indices <- c(remove.indices, nlr.index, plr.index)
    }

    # remove fnames and params for side-by-side plots
    fnames <- fnames[setdiff(1:length(fnames), remove.indices)]
    params.list <- params.list[setdiff(1:length(params.list), remove.indices)]

    if (length(params.list) > 0) {
        for (count in 1:length(params.list)) {
            # create ma summaries and single (not side-by-side) forest plots.
            #pretty.names <- eval(call(paste(fnames[count],".pretty.names",sep="")))
            diagnostic.data.tmp <- compute.diag.point.estimates(diagnostic.data, params.list[[count]])
            results.tmp <- subgroup.ma.diagnostic(fnames[[count]], diagnostic.data.tmp, params.list[[count]], selected.cov)
            if (is.null(params.list[[count]]$create.plot)) {
                # create plot
                images.tmp <- results.tmp$images
                names(images.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$",params.list[[count]]$measure,sep=""))), " Forest Plot", sep="")
                images <- c(images, images.tmp)
                plot.params.paths.tmp <- results.tmp$plot_params_paths
                names(plot.params.paths.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$", params.list[[count]]$measure,sep=""))), " Forest Plot", sep="")
                plot.params.paths <- c(plot.params.paths, plot.params.paths.tmp)
                plot.names <- c(plot.names, results.tmp$plot_names)
            }
            summary.tmp <- list("Summary"=results.tmp$Summary)
            names(summary.tmp) <- paste(eval(parse(text=paste("pretty.names$measure$",params.list[[count]]$measure,sep=""))), " Summary", sep="")
            results <- c(results, summary.tmp)
        }
    }
    results <- c(results, list("images"=images, "plot_names"=plot.names, 
                               "plot_params_paths"=plot.params.paths))
    #results$images <- images
    #results$plot.names <- plot.names
    #results$plot.params.paths <- plot.params.paths
    results
}

subgroup.ma.diagnostic <- function(fname, diagnostic.data, params, selected.cov){
    # performs a single subgroup meta-analysis for diagnostic.data
    if (!("DiagnosticData" %in% class(diagnostic.data))) stop("Diagnostic data expected.")
    #cov.name <- as.character(params$cov_name)
    #selected.cov <- get.cov(diagnostic.data, cov.name)
    cov.vals <- selected.cov@cov.vals
    params.tmp <- params
    params.tmp$create.plot <- FALSE
    params.tmp$write.to.file <- FALSE
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
      terms <- compute.diagnostic.terms(raw.data, params.tmp)
      col3.nums <- c(col3.nums, terms$numerator, sum(terms$numerator))
      col3.denoms <- c(col3.denoms, terms$denominator, sum(terms$denominator))
      cur.res <- eval(call(fname, diag.data.tmp, params.tmp))
      cur.overall <- eval(call(paste(fname, ".overall", sep=""), cur.res))
      subgroup.results[[count]] <- cur.overall
      count <- count + 1
    }
    res <- eval(call(fname, diagnostic.data, params.tmp))
    res.overall <- eval(call(paste(fname, ".overall", sep=""), res))
    grouped.data[[count]] <- diagnostic.data
    subgroup.results[[count]] <- res.overall
    subgroup.names <- paste("Subgroup ", subgroup.list, sep="")
    subgroup.names <- c(subgroup.names, "Overall")
    
    metric.name <- pretty.metric.name(params.tmp$measure)
    model.title <- ""
    if (fname == "diagnostic.fixed") {
        model.title <- paste("Diagnostic Fixed-effect Model - Inverse Variance\n\nMetric: ", metric.name, sep="") 
    } else if (fname == "diagnostic.random") {
        model.title <- paste("Diagnostic Random-Effects Model\n\nMetric: ", metric.name, sep="")
    }
    
    subgroup.disp <- create.subgroup.display(subgroup.results, subgroup.names, params, model.title, data.type="diagnostic")
    forest.path <- paste(params$fp_outpath, sep="")
    # pack up the data for forest plot.
    subgroup.data <- list("subgroup.list"=subgroup.list, "grouped.data"=grouped.data, "results"=subgroup.results, 
                          "col3.nums"=col3.nums, "col3.denoms"=col3.denoms, "col4.nums"=col4.nums, "col4.denoms"=col4.denoms)
    if (is.null(params$create.plot)) {
        plot.data <- create.subgroup.plot.data.diagnostic(subgroup.data, params)
        changed.params <- plot.data$changed.params
        # list of changed params values
        params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
        changed.params <- c(changed.params, params.changed.in.forest.plot)
        params[names(changed.params)] <- changed.params
        # update params values
        # we use the system time as our unique-enough string to store
        # the params object
        forest.plot.params.path <- save.data(diagnostic.data, res, params, plot.data)
        # Now we package the results in a dictionary (technically, a named 
        # vector). In particular, there are two fields that must be returned; 
        # a dictionary of images (mapping titles to image paths) and a list of texts
        # (mapping titles to pretty-printed text). In this case we have only one 
        # of each. 
        #    
        plot.params.paths <- c("Forest Plot"=forest.plot.params.path)
        images <- c("Subgroups Forest Plot"=forest.path)
        plot.names <- c("subgroups forest plot"="subgroups_forest_plot")
        results <- list("images"=images, "Summary"=subgroup.disp, 
                    "plot_names"=plot.names, 
                    "plot_params_paths"=plot.params.paths)
    } else {
        results <- list(subgroup.data=subgroup.data, Summary=subgroup.disp, "cov.list"=subgroup.list)
    }
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

create.subgroup.side.by.side.plot.data <- function(subgroup.data, params) {    
    # creates data for two side-by-side forest plots
    params.left <- params$left
    params.right <- params$right
    params.left$fp_show_col1 <- 'TRUE'
    params.right$fp_show_col1 <- 'FALSE'
    # only show study names on the left plot
    subgroup.data.left <- subgroup.data$left
    subgroup.data.right <- subgroup.data$right
    
    plot.data.left <- create.subgroup.plot.data.diagnostic(subgroup.data.left, params.left)
    plot.data.left$options$fp.title <- pretty.metric.name(as.character(params.left$measure))
      
    plot.data.right <- create.subgroup.plot.data.diagnostic(subgroup.data.right, params.right)
    plot.data.right$options$fp.title <- pretty.metric.name(as.character(params.right$measure))
    
    plot.data <- list("left"=plot.data.left, "right"=plot.data.right)
    plot.data
}