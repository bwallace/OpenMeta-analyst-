####################################
#                                  #
# OpenMeta[Analyst]                #
# ----                             #
# utilities.r                      #
#                                  #
# Utilities for pretty-printing    #
# results.                         #
####################################


print.summary.display <- function(summary.disp,...) {
    #
    # Prints a summary results
    # summary.disp is a list containing the following named items
    # - model.title - a string that appears at the top of the summary.
    # - table.titles - a vector of titles for the results tables
    #   Setting a table title to NA prevents the table from being printed.
    # - arrays - a list of arrays, of the same length as table.titles,
    #   which are pretty-printed by print.summary.data 
    #
    cat(summary.disp$model.title)
    cat("\n\n")
    arrays <- summary.disp$arrays
    count = 1
    for (name in arrays) {
        if (!is.na(summary.disp$table.titles[count])) {
            cat(summary.disp$table.titles[count])
            cat("\n")
            print.summary.data(name)
            cat("\n")
        }
        count = count + 1
   }
}

print.summary.data <- function(table.data) {
    # Prints an array table.data with lines separating rows and columns.
    num.rows <- length(table.data[,1])
    num.cols <- length(table.data[1,])
    # Compute column widths
    extra.col.spaces <- 2
    col.widths <- NULL
    for (col.index in 1:num.cols) {
      col.widths <- c(col.widths, max(nchar(table.data[,col.index])) + extra.col.spaces)
    }
    table.width <- sum(col.widths) + num.cols + 1
    # Create line of dashes of length table.width - 2
    dash.line <- NULL
    dash.line <- create.repeat.string("-", table.width - 2)
    top.line <- paste("+", dash.line, "+", sep="")
    middle.line <- paste("|", dash.line, "|", sep="")

    # Build table
    cat(top.line)
    cat("\n")
    for (row.index in 1:num.rows) {
        table.row <- "|"
        for (col.index in 1:num.cols) {
            col.width <- col.widths[col.index]
            entry <- table.data[row.index,col.index]
            # pad entries with spaces to align columns.
            begin.num <- floor((col.width - nchar(entry))/2)
            end.num <- ceiling((col.width - nchar(entry))/2) 
            padded.entry <- pad.with.spaces(entry, begin.num, end.num)
            table.row <- paste(table.row, padded.entry, "|", sep="")
        }
        cat(table.row)
        cat("\n")
        if (row.index < num.rows) {
            cat(middle.line)
            cat("\n")
        }
    }
    cat(top.line)
    cat("\n")
}

pad.with.spaces <- function(entry, begin.num, end.num) {
    # Adds spaces to beginning and end of entry
    repeat.string.begin <- ""
    if (begin.num > 0) {
        repeat.string.begin <- create.repeat.string(" ", begin.num)
    }
    repeat.string.end <- ""
    if (end.num > 0) {
        repeat.string.end <- create.repeat.string(" ", end.num)
    }
    padded.entry <- paste(repeat.string.begin, entry, repeat.string.end, sep="")
    padded.entry
}

create.repeat.string <- function(symbol, num.repeats) {
    # creates a string in which symbol is repeated num.repeats times
    repeat.string <- NULL
    for (count in 1:num.repeats) {
        repeat.string <- paste(repeat.string, symbol, sep="")
    }
    repeat.string
}
 
round.display <- function(x, digits) {
    # Prints "< 10^(-digits)" if x.rounded == 0 or "x" otherwise
    x.rounded <- round(x, digits)
    cutoff <- sprintf(paste("%.", digits,"f", sep=""), 10^(-digits))
    x.rounded[x.rounded == 0] <- paste("< ", cutoff, sep = "", collapse = "")
    x.rounded
}

create.summary.disp <- function(res, params, model.title, data.type) {
    # create table for diplaying summary of ma results
    if (data.type == "continuous") {
        transform.name <- "continuous.transform.f"
    } else if (data.type == "diagnostic") {
        transform.name <- "diagnostic.transform.f"
    }  else {  
        transform.name <- "binary.transform.f"
    }
    scale.str <- "standard"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    } else if (metric.is.logit.scale(params$measure)) {
        scale.str <- "logit"
    }
    tau2 <- round(res$tau2, digits=params$digits)
    degf <- res$k - 1
    QLabel =  paste("Q(df=", degf, ")", sep="")
    if (!is.null(res$QE)) {
      I2 <- max(0, (res$QE - degf)/res$QE)
      I2 <- paste(100 * round(I2, digits = 2), "%")
      QE <- round(res$QE, digits=params$digits)
    } else {
      I2 <- "NA"
      QE <- "NA"
    }
    if (!is.null(res$QEp)) {
      QEp <- round.display(x=res$QEp, digits=params$digits)
    } else {
      QEp <- "NA"
    }
    if (!is.null(res$pval)) {
      pVal <- round.display(res$pval, digits=params$digits)
    } else {
      pVal <- "NA"
    }
    if (!is.null(res$zval)) {
      zVal <- round.display(res$zval, digits=params$digits)
    } else {
      zVal <- "NA"
    }
    
    res.title <- "  Model Results"
    y.disp <- round(eval(call(transform.name, params$measure))$display.scale(res$b), digits=params$digits)
    lb.disp <- round(eval(call(transform.name, params$measure))$display.scale(res$ci.lb), digits=params$digits)
    ub.disp <- round(eval(call(transform.name, params$measure))$display.scale(res$ci.ub), digits=params$digits)
    se <- round(res$se, digits=params$digits)
   
    res.array <- array(c("Estimate", y.disp, "Lower bound", lb.disp,
                     "Upper bound", ub.disp, "Std. error", se, "p-Value", pVal, "Z-Value", zVal),  
                     dim=c(2,6))
    if (res$method=="FE") {
        het.array <-  array(c(QLabel, QE, "Het. p-Value", QEp, "I^2", I2), dim=c(2,3)) 
    } else {    
        het.array <-  array(c("tau^2", tau2, QLabel, QE, "Het. p-Value", QEp, "I^2", I2), dim=c(2,4))
    }
    class(het.array) <- "summary.data"
    het.title <- "  Heterogeneity"
   
    if (scale.str == "log" || scale.str == "logit") {
         # display and calculation scales are different - create two tables for results
         estCalc <- round(res$b, digits=params$digits)
         lbCalc <- round(res$ci.lb, digits=params$digits)
         ubCalc <- round(res$ci.ub, digits=params$digits)
         alt.array <- array(c("Estimate", estCalc, "Lower bound", lbCalc, "Upper bound", ubCalc), dim=c(2,3))
         alt.title <- paste("  Point Estimates (", scale.str, " scale)", sep="")
         arrays <- list(arr1=res.array, arr2=het.array, arr3=alt.array)
         table.titles <- c(res.title, het.title, alt.title)
    } else {
        # display and calculation scales are the same - create one table for results
        arrays = list(arr1=res.array, arr2=het.array)
        table.titles <- c(res.title, het.title)
    }
    summary.disp <- list("model.title" = model.title, "table.titles" = table.titles, "arrays" = arrays,
                         "MAResults" = res)
    class(summary.disp) <- "summary.display"
    summary.disp
}


save.plot.data <- function(plot.data) {
  forest.plot.params.path <- paste("r_tmp/", 
                              as.character(as.numeric(Sys.time())), sep="")
  save(plot.data, file=forest.plot.params.path)
  forest.plot.params.path
}

create.regression.disp <- function(res, params, cov.names) {
    # create table for diplaying summary of regression ma results
    coeffs <- round(res$b, digits=params$digits)
    pvals <- round.display(res$pval, digits=params$digits)
    lbs <- round(res$ci.lb, digits=params$digits)
    ubs <- round(res$ci.ub, digits=params$digits)
    dimnames(coeffs) <- NULL
    dimnames(pvals) <- NULL
    dimnames(lbs) <- NULL
    dimnames(ubs) <- NULL
    cov.names <- c("", "Intercept", cov.names)
    estimates <- c("Estimates", coeffs)
    p.values <- c("p-Values", pvals)
    l.bounds <- c("Lower bounds", lbs)
    u.bounds <- c("Upper bounds", ubs)
    reg.array <- cbind(cov.names, estimates, p.values, l.bounds, u.bounds)
    arrays <- list(arr1=reg.array)
    reg.disp <- list("model.title" = "Meta-Regression", "table.titles" = c("Model Results"), "arrays" = arrays)

    class(reg.disp) <-  "summary.display"
    return(reg.disp)
}

create.overall.display <- function(res, study.names, params, model.title, data.type) {
    # create table for diplaying summary of overall ma results
    if (data.type == "continuous") {
        transform.name <- "continuous.transform.f"
    } else if (data.type == "diagnostic") {
        transform.name <- "diagnostic.transform.f"
    }  else {  
        transform.name <- "binary.transform.f"
    }
    scale.str <- "standard"
    if (metric.is.log.scale(params$measure)){
        scale.str <- "log" 
    } else if (metric.is.logit.scale(params$measure)) {
        scale.str <- "logit"
    }
    overall.array <- array(dim=c(length(study.names) + 1, 10))
    
    #QLabel =  paste("Q(df = ", degf, ")", sep="")
    
    overall.array[1,] <- c("Studies", "Estimate", "Lower bound", "Upper bound", 
                           "Std. error", "p-Val", "Z-Val", "Q (df)",
                           "Het. p-Val", "I^2")
    if (scale.str == "log" || scale.str == "logit") {
        # display and calculation scales are different - create second table for point estimates in calc scale. 
        overall.array.calc <- array(dim=c(length(study.names) + 1, 4))
        overall.array.calc[1,] <- c("Studies", "Estimate", "Lower bound", "Upper bound")
    }
    # unpack the data
    for (count in 1:length(study.names)) {
      y <- res[[count]]$b
      lb <- res[[count]]$ci.lb
      ub <- res[[count]]$ci.ub
      se <- res[[count]]$se
      y.disp <- round(eval(call(transform.name, params$measure))$display.scale(y), digits=params$digits)
      lb.disp <- round(eval(call(transform.name, params$measure))$display.scale(lb), digits=params$digits)
      ub.disp <- round(eval(call(transform.name, params$measure))$display.scale(ub), digits=params$digits)
      se.disp <- round(se, digits=params$digits)
      if (!is.null(res[[count]]$QE)) {
        degf <- res[[count]]$k - 1
        I2 <- max(0, (res[[count]]$QE - degf)/res[[count]]$QE)
        I2 <- paste(100 * round(I2, digits = 2), "%")
        QE <- round(res[[count]]$QE, digits=params$digits)
        QE <- paste(QE, " (", degf,")", sep="")
      } else {
        I2 <- "NA"
        QE <- "NA"
      }
      if (!is.null(res[[count]]$QEp)) {
        QEp <- round.display(x=res[[count]]$QEp, digits=params$digits)
      } else {
        QEp <- "NA"
      }
      if (!is.null(res[[count]]$pval)) {
        pVal <- round.display(res[[count]]$pval, digits=params$digits)
      } else {
        pVal <- "NA"
      }
      if (!is.null(res[[count]]$zval)) {
        zVal <- round.display(res[[count]]$zval, digits=params$digits)
      } else {
        zVal <- "NA"
      }
      overall.array[count+1,] <- c(study.names[count], y.disp, lb.disp, ub.disp, se.disp, pVal, zVal, QE, QEp, I2)
      if (scale.str == "log" || scale.str == "logit") {
        # create data for second table for point estimates in calc scale.
        y.calc <- round(y, digits=params$digits)
        lb.calc <- round(lb, digits=params$digits)
        ub.calc <- round(ub, digits=params$digits)
        overall.array.calc[count+1, ] <- c(study.names[count], y.calc, lb.calc, ub.calc)
      }
    }
    
    if (scale.str == "log" || scale.str == "logit") {
        # display and calculation scales are different - create second table data    
       table.titles <- c("Model Results", paste("  Point Estimates (", scale.str, " scale)", sep=""))
       arrays <- list(arr1=overall.array, arr2=overall.array.calc)
       
    } else {
       # only one table
       table.titles <- c("  Model Results")
       arrays <- list(arr1=overall.array)
    }
    overall.disp <- list("model.title" = model.title, "table.titles" = table.titles, "arrays" = arrays,
                         "MAResults" = res )
    class(overall.disp) <- "summary.display"
    overall.disp
}


results.short.list <- function(res) {
    # extracts res$b, res$ci.lb, and res$ci.ub from res
    res.short <- list("b"=res$b[1], "ci.lb"=res$ci.lb, "ci.ub"=res$ci.ub)
}    
