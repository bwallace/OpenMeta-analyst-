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
    # Prints "< 5*10^(-digits-1)" if x.rounded == 0 or "x" otherwise
    x.rounded <- round(x, digits)
    zero.display <- function(x.rounded, digits) {
        cutoff <- 5 * 10^(-digits-1)
        if (x.rounded == 0) {
          x.rounded <- paste("< ", cutoff, sep = "", collapse = "")
        }
        return(x.rounded)
    }
    x.rounded <- mapply(zero.display, x.rounded, digits)
    x.rounded
}

create.summary.disp <- function(res, params, degf, model.title, data.type) {
    # create table for diplaying summary of ma results
    if (data.type == "diagnostic") {
        transform.name <- "diagnostic.transform.f"
    }  else {  
       # data is binary or cont
        transform.name <- "binary.transform.f"
    }
    QLabel =  paste("Q(df = ", degf, ")", sep="")
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
    #het.array <-  array(c(QLabel, QE, "p-Value", QEp, "I^2", I2), dim=c(2,3))
    #class(het.array) <- "summary.data"
    #het.title <- "  Test for Heterogeneity"
    res.title <- "  Model Results (reporting scale)"
    y.disp <- round(eval(call(transform.name, params$measure))$display.scale(res$b), digits=params$digits)
    lb.disp <- round(eval(call(transform.name, params$measure))$display.scale(res$ci.lb), digits=params$digits)
    ub.disp <- round(eval(call(transform.name, params$measure))$display.scale(res$ci.ub), digits=params$digits)
    se <- round(res$se, digits=params$digits)

   if ((metric.is.log.scale(params$measure)) | (metric.is.logit.scale(params$measure))) {
         # display and calculation scales are different - create two tables for results
         res.array <- array(c("Estimate", y.disp, "Lower bound", lb.disp,
                              "Upper bound", ub.disp, "p-Value", pVal, "Z-Value", zVal,  
                              QLabel, QE, "Het. p-Value", QEp, "I^2", I2), dim=c(2,8))
         
         estCalc <- round(res$b, digits=params$digits)
         lbCalc <- round(res$ci.lb, digits=params$digits)
         ubCalc <- round(res$ci.ub, digits=params$digits)
         alt.array <- array(c("Estimate", estCalc, "SE", se, "Lower bound", lbCalc, "Upper bound", ubCalc), dim=c(2,4))
         alt.title <- "  Point Estimates (calculation scale)"
         #arrays = list(arr1=het.array, arr2=res.array, arr3=alt.array)
         arrays <- list(arr1=res.array, arr2=alt.array)
    }

    else {
        # display and calculation scales are the same - create one table
        res.array <- array(c("Estimate", y.disp, "SE", se, "p-Value", pVal, "Z-Value", zVal, "Lower bound", lb.disp,
                                        "Upper bound", ub.disp), dim=c(2,6))
        #arrays = list(arr1=het.array, arr2=res.array)
        arrays = list(arr1="res.array")
        alt.title <- NA
    }
    summary.disp <- list("model.title" = model.title, "table.titles" = c(res.title, alt.title), "arrays" = arrays,
                         "MAResults" = res)
    class(summary.disp) <- "summary.display"
    return(summary.disp)
}

create.regression.disp <- function(res, params) {
    # create table for diplaying summary of regression ma results
    coeffs <- round(res$b, digits=params$digits)
    pvals <- round.display(res$pval, digits=params$digits)
    lbs <- round(res$ci.lb, digits=params$digits)
    ubs <- round(res$ci.ub, digits=params$digits)
    reg.array <- array(c("", "Intercept", "Slope", "Estimates", coeffs[1], coeffs[2], "p-Values", pvals[1], pvals[2],
                      "Lower bounds", lbs[1], lbs[2], "Upper bounds", ubs[1], ubs[2]), dim=c(3, 5))
    arrays <- list(arr1=reg.array)
    reg.disp <- list("model.title" = "", "table.titles" = c(""), "arrays" = arrays)
    class(reg.disp) <-  "summary.display"
    return(reg.disp)
}

create.overall.display <- function(res, study.names, params, data.type) {
    # create table for diplaying summary of overall ma results
    if (data.type == "diagnostic") {
        transform.name <- "diagnostic.transform.f"
    }  else {  
       # data is binary or cont
        transform.name <- "binary.transform.f"
    } 
    degf <- length(study.names) - 1
    overall.array <- array(dim=c(length(study.names) + 1, 9))
    
    QLabel =  paste("Q(df = ", degf, ")", sep="")
    
    overall.array[1,] <- c("Studies", "Estimate", "Lower bound", "Upper bound", "p-Val", "Z-Val", QLabel,
                           "Het. p-Val", "I^2")
    if ((metric.is.log.scale(params$measure)) | (metric.is.logit.scale(params$measure))) {
        # display and calculation scales are different - create second table for point estimates in calc scale. 
        overall.array.calc <- array(dim=c(length(study.names) + 1, 4))
        overall.array.calc[1,] <- c("Studies", "Estimate", "Lower bound", "Upper bound")
    }
    # unpack the data
    for (count in 1:length(study.names)) {
      y <- res[[count]]$b
      lb <- res[[count]]$ci.lb
      ub <- res[[count]]$ci.ub
      y.disp <- round(eval(call(transform.name, params$measure))$display.scale(y), digits=params$digits)
      lb.disp <- round(eval(call(transform.name, params$measure))$display.scale(lb), digits=params$digits)
      ub.disp <- round(eval(call(transform.name, params$measure))$display.scale(ub), digits=params$digits)
      if (!is.null(res[[count]]$QE)) {
        I2 <- max(0, (res[[count]]$QE - degf)/res[[count]]$QE)
        I2 <- paste(100 * round(I2, digits = 2), "%")
        QE <- round(res[[count]]$QE, digits=params$digits)
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
      overall.array[count+1,] <- c(study.names[count], y.disp, lb.disp, ub.disp, pVal, zVal, QE, QEp, I2)
      if ((metric.is.log.scale(params$measure)) | (metric.is.logit.scale(params$measure))) {
        # create data for second table for point estimates in calc scale.
        y.calc <- round(y, digits=params$digits)
        lb.calc <- round(lb, digits=params$digits)
        ub.calc <- round(ub, digits=params$digits)  
        overall.array.calc[count+1, ] <- c(study.names[count], y.disp, lb.disp, ub.disp)
      }
    }
    
    if ((metric.is.log.scale(params$measure)) | (metric.is.logit.scale(params$measure))) {
        # display and calculation scales are different - create second table data    
       table.titles <- c("  Model Results (reporting scale)", "  Point Estimates (calculation scale)")
       arrays <- list(arr1=overall.array, arr2=overall.array.calc)
       
    } else {
       # only one table
       table.titles <- c("  Model Results")
       arrays <- list(arr1=overall.array)
    }
    overall.disp <- list("model.title" = "", "table.titles" = table.titles, "arrays" = arrays)
    class(overall.disp) <- "summary.display"
    overall.disp
}


results.short.list <- function(res) {
    # extracts res$b, res$ci.lb, and res$ci.ub from res
    res.short <- list("b"=res$b[1], "ci.lb"=res$ci.lb, "ci.ub"=res$ci.ub)
}    
