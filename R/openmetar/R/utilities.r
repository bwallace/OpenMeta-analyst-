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

print.summary.data <- function(table.data,...) {
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
            end.num <- ceiling((col.width - nchar(entry))/2)
            if ((row.index>1) & (regexpr("-", entry)!=1) & (regexpr("<", entry)!=1)) {
                 # entry is a positive number so add extra space to align decimal sign.
                 entry <- paste(" ", entry, sep="")
            } 
            begin.num <- floor((col.width - nchar(entry))/2)
            end.num <- col.width - begin.num - nchar(entry)
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
    digits.str <- paste("%.", digits, "f", sep="")
    x.disp <- c()
    x.disp[x < 10^(-digits)] <- paste("< ", 10^(-digits), sep="")
    x.disp[x >= 10^(-digits)] <- sprintf(digits.str, x[x>=10^(-digits)])
    x.disp
}

create.summary.disp <- function(om.data, params, res, model.title) {
    # create table for diplaying summary of ma results
    digits.str <- paste("%.", params$digits, "f", sep="")
    transform.name <- get.transform.name(om.data)
    scale.str <- get.scale(params)
    tau2 <- sprintf(digits.str, res$tau2)
    degf <- res$k - 1
    QLabel =  paste("Q(df=", degf, ")", sep="")
    if (!is.null(res$QE)) {
      I2 <- max(0, (res$QE - degf)/res$QE)
      I2 <- paste(100 * round(I2, digits = 2), "%", sep="")
      QE <- sprintf(digits.str, res$QE)
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
        
    res.title <- "  Model Results"
    y.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(res$b))
    lb.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(res$ci.lb))
    ub.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(res$ci.ub))
    se <- sprintf(digits.str, res$se)
   
    
    if (res$method=="FE") {
        het.col.labels <- c(QLabel, "Het. p-Value", "I^2")
        het.col.vals <-  c(QE, QEp, I2)
        het.array <- rbind(het.col.labels, het.col.vals)
    } else {    
        het.col.labels <- c("tau^2", QLabel, "Het. p-Value", "I^2")
        het.col.vals <-  c(tau2, QE, QEp, I2)
        het.array <- rbind(het.col.labels, het.col.vals)
    }
    class(het.array) <- "summary.data"
    het.title <- "  Heterogeneity"
   
    if (scale.str == "log" || scale.str == "logit" || scale.str == "arcsine") {
         # display and calculation scales are different - create two tables for results
         res.col.labels <- c("Estimate", "Lower bound", "Upper bound","p-Value")
         res.col.vals <- c(y.disp, lb.disp, ub.disp, pVal)
         res.array <- rbind(res.col.labels, res.col.vals)
         estCalc <- sprintf(digits.str, res$b)
         lbCalc <- sprintf(digits.str, res$ci.lb)
         ubCalc <- sprintf(digits.str, res$ci.ub)
         alt.col.labels <- c("Estimate", "Lower bound", "Upper bound", "Std. error")
         alt.col.vals <- c(estCalc, lbCalc, ubCalc, se)
         alt.array <- rbind(alt.col.labels, alt.col.vals)
         alt.title <- paste("  Results (", scale.str, " scale)", sep="")
         arrays <- list(arr1=res.array, arr2=het.array, arr3=alt.array)
         table.titles <- c(res.title, het.title, alt.title)
    } else {
        # display and calculation scales are the same - create one table for results
        col.labels <- c("Estimate", "Lower bound", "Upper bound", "Std. error", "p-Value")
        col.vals <- c(y.disp, lb.disp, ub.disp, se, pVal)
        res.array <- rbind(col.labels, col.vals)
        arrays = list(arr1=res.array, arr2=het.array)
        table.titles <- c(res.title, het.title)
    }
    
    if (transform.name == "binary.transform.f") {
      # Add raw data title and array 
      raw.data.array <- create.binary.data.array(om.data, params, res)
      table.titles <- c("  Study Data", table.titles)
      raw.data.list <- list("arr0"=raw.data.array)
      arrays <- c(raw.data.list, arrays)
    } else if (transform.name == "continuous.transform.f") {
      raw.data.array <- create.cont.data.array(om.data, params, res)
      table.titles <- c("  Study Data", table.titles)
      raw.data.list <- list("arr0"=raw.data.array)
      arrays <- c(raw.data.list, arrays)
    }
  
    summary.disp <- list("model.title" = model.title, "table.titles" = table.titles, "arrays" = arrays,
                         "MAResults" = res)
    class(summary.disp) <- "summary.display"
    summary.disp
}

# @TODO should merge this with below
save.plot.data <- function(plot.data, out.path=NULL) {
    # saves plot data to the r_tmp directory
    if (is.null(out.path)){
      # by default, we use thecurrent system time as a 'unique enough' filename
      out.path <- paste("r_tmp/", 
                                as.character(as.numeric(Sys.time())), sep="")
    }
    ### save plot data *only*
    save(plot.data, file=paste(out.path, ".plotdata", sep=""))
    out.path
}

save.data <- function(om.data, res, params, plot.data, out.path=NULL) {
    # this saves *all* the data for certain types of plots, in contrast
    # to the above method (save.plot.data), which saves only the plot.data
    # object.
    #
    # save the data, result and plot parameters to a tmp file on disk
    if (is.null(out.path)){
      # by default, we use thecurrent system time as a 'unique enough' filename
      out.path <- paste("r_tmp/", 
                              as.character(as.numeric(Sys.time())), sep="")
    }

    save(om.data, file=paste(out.path, ".data", sep=""))
    save(res, file=paste(out.path, ".res", sep=""))
    save(plot.data, file=paste(out.path, ".plotdata", sep=""))
    save(params, file=paste(out.path, ".params", sep=""))
    out.path
}

create.regression.display <- function(res, params, display.data) {
    # create table for diplaying summary of regression ma results
    cov.display.col <- display.data$cov.display.col
    levels.display.col <- display.data$levels.display.col
    studies.display.col <- display.data$studies.display.col
    # first two columns of table
    factor.n.levels <- display.data$factor.n.levels
    n.cont.covs <- display.data$n.cont.covs
    n.cont.rows <- n.cont.covs + 1 # extra row for intercept
    n.factor.covs <- length(factor.n.levels)
    n.rows <- length(cov.display.col) + 1
    # extra row for col. labels
    if (n.factor.covs==0) {
        col.labels <- c("Covariate", "Coefficients", "Lower bound", "Upper bound", "Std. error", "p-Value")
    } else {
        col.labels <- c("Covariate", "Level", "Studies", "Coefficients", "Lower bound", "Upper bound", "Std. error", "p-Value")
    }
        
    reg.array <- array(dim=c(length(cov.display.col)+1, length(col.labels)), dimnames=list(NULL, col.labels))
    reg.array[1,] <- col.labels
    digits.str <- paste("%.", params$digits, "f", sep="")
    coeffs <- sprintf(digits.str, res$b)
    se <- round.display(res$se, digits=params$digits)
    pvals <- round.display(res$pval, digits=params$digits)
    lbs <- sprintf(digits.str, res$ci.lb)
    ubs <- sprintf(digits.str, res$ci.ub)
    
    coeffs.tmp <- coeffs[1:n.cont.rows]
    # extra row for intercept
    se.tmp <- se[1:n.cont.rows]
    pvals.tmp <- pvals[1:n.cont.rows]
    lbs.tmp <- lbs[1:n.cont.rows]
    ubs.tmp <- ubs[1:n.cont.rows]
    if (n.factor.covs > 0) {
      # there are factor covariants - insert spaces for reference var. row.
      insert.row <- n.cont.rows + 1
      for (count in 1:n.factor.covs) {
        n.levels <- factor.n.levels[count] 
        coeffs.tmp <- c(coeffs.tmp,"", coeffs[insert.row:(insert.row + n.levels - 2)])
        se.tmp <- c(se.tmp,"", se[insert.row:(insert.row + n.levels - 2)])
        pvals.tmp <- c(pvals.tmp,"",pvals[insert.row:(insert.row + n.levels - 2)])
        lbs.tmp <- c(lbs.tmp,"",lbs[insert.row:(insert.row + n.levels - 2)])
        ubs.tmp <- c(ubs.tmp,"",ubs[insert.row:(insert.row + n.levels - 2)])
        insert.row <- insert.row + n.levels
      }   
      reg.array[2:n.rows, "Level"] <- levels.display.col
      reg.array[2:n.rows, "Studies"] <- studies.display.col
    }

    # add data to array
    reg.array[2:n.rows,"Covariate"] <- cov.display.col
    reg.array[2:n.rows,"Coefficients"] <- coeffs.tmp 
    reg.array[2:n.rows,"Std. error"] <- se.tmp
    reg.array[2:n.rows, "p-Value"] <- pvals.tmp
    reg.array[2:n.rows, "Lower bound"] <- lbs.tmp
    reg.array[2:n.rows, "Upper bound"] <- ubs.tmp
    
    arrays <- list(arr1=reg.array)
    
    metric.name <- pretty.metric.name(as.character(params$measure)) 
    model.title <- paste("Meta-Regression\n\nMetric: ", metric.name, sep="")
    reg.disp <- list("model.title" = model.title, "table.titles" = c("Model Results"), "arrays" = arrays)

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
    overall.array <- array(dim=c(length(study.names) + 1, 6))
        #QLabel =  paste("Q(df = ", degf, ")", sep="")
    
    overall.array[1,] <- c("Studies", "Estimate", "Lower bound", "Upper bound", "Std. error", "p-Val")
    
    # unpack the data
    for (count in 1:length(study.names)) {
      y <- res[[count]]$b
      lb <- res[[count]]$ci.lb
      ub <- res[[count]]$ci.ub
      se <- res[[count]]$se
      digits.str <- paste("%.", params$digits, "f", sep="")
      y.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(y))
      lb.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(lb))
      ub.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(ub))
      se.disp <- sprintf(digits.str, se)
      
      if (!is.null(res[[count]]$pval)) {
        pVal <- round.display(res[[count]]$pval, digits=params$digits)
      } else {
        pVal <- "NA"
      }
      overall.array[count+1,] <- c(study.names[count], y.disp, lb.disp, ub.disp, se.disp, pVal)
    }

    table.titles <- c("  Model Results")
    arrays <- list(arr1=overall.array)
    overall.disp <- list("model.title" = model.title, "table.titles" = table.titles, "arrays" = arrays,
                         "MAResults" = res )
    class(overall.disp) <- "summary.display"
    overall.disp
}

create.subgroup.display <- function(res, study.names, params, model.title, data.type) {
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
    subgroup.array <- array(dim=c(length(study.names) + 1, 7))
    het.array <- array(dim=c(length(study.names) + 1, 4))
    #QLabel =  paste("Q(df = ", degf, ")", sep="")
    
    subgroup.array[1,] <- c("Subgroups", "Studies", "Estimate", "Lower bound", "Upper bound", "Std. error", "p-Val")
    het.array[1,] <- c("Studies", "Q (df)",
                           "Het. p-Val", "I^2")
    # unpack the data
    for (count in 1:length(study.names)) {
      num.studies <- res[[count]]$k
      y <- res[[count]]$b
      lb <- res[[count]]$ci.lb
      ub <- res[[count]]$ci.ub
      se <- res[[count]]$se
      digits.str <- paste("%.", params$digits, "f", sep="")
      y.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(y))
      lb.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(lb))
      ub.disp <- sprintf(digits.str, eval(call(transform.name, params$measure))$display.scale(ub))
      se.disp <- sprintf(digits.str, se)
      if (!is.null(res[[count]]$QE)) {
        degf <- res[[count]]$k - 1
        I2 <- max(0, (res[[count]]$QE - degf)/res[[count]]$QE)
        I2 <- paste(100 * round(I2, digits = 2), "%")
        QE <- sprintf(digits.str, res[[count]]$QE)
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
      subgroup.array[count+1,] <- c(study.names[count], num.studies, y.disp, lb.disp, ub.disp, se.disp, pVal)
      het.array[count+1,] <- c(study.names[count], QE, QEp, I2)
    }

    table.titles <- c("  Model Results", "  Heterogeneity")
    arrays <- list(arr1=subgroup.array, arr2=het.array)
    #}
    subgroup.disp <- list("model.title" = model.title, "table.titles" = table.titles, "arrays" = arrays,
                         "MAResults" = res )
    class(subgroup.disp) <- "summary.display"
    subgroup.disp
}


results.short.list <- function(res) {
    # extracts res$b, res$ci.lb, and res$ci.ub from res
    res.short <- list("b"=res$b[1], "ci.lb"=res$ci.lb, "ci.ub"=res$ci.ub)
}

calc.ci.bounds <- function(om.data, params, res) {
    y <- om.data@y
    se <- om.data@SE
    alpha <- 1.0-(params$conf.level/100.0)
    mult <- abs(qnorm(alpha/2.0))
    lb <- y - mult*om.data@SE
    ub <- y + mult*om.data@SE
    res$study.lb <- lb
    res$study.ub <- ub
    res
}

write.results.to.file <- function(om.data, params, res) {
    # write results to file
    transform.name <- get.transform.name(om.data) 
    results.df <- data.frame("Summary.estimate" = eval(call(transform.name, params$measure))$display.scale(res$b),
                             "Lower.bound" = eval(call(transform.name, params$measure))$display.scale(res$ci.lb),
                             "Upper.bound" = eval(call(transform.name, params$measure))$display.scale(res$ci.ub),
                             "p-Value" = res$pval)
    write.csv(results.df, file="./r_tmp/results.csv", append=FALSE, row.names=FALSE)
}

get.transform.name <- function(om.data) { 
    # Get transform name for converting between display and calculation scales 
    if ("ContinuousData" %in% class(om.data)) {
      transform.name <-"continuous.transform.f"
      data.type <- "continuous"
    } else if ("DiagnosticData" %in% class(om.data)) {
      transform.name <- "diagnostic.transform.f"
      data.type <- "diagnostic"
    } else if ("BinaryData" %in% class(om.data)) {
      transform.name <- "binary.transform.f"
      data.type <- "binary"
    }
    transform.name
}

get.scale <- function(params) {
    # Get the transformation scale
    if (metric.is.log.scale(params$measure)){
        scale <- "log" 
    } else if (metric.is.logit.scale(params$measure)) {
        scale <- "logit"
    } else if (metric.is.arcsine.scale(params$measure)) {
        scale <- "arcsine"
    } else {
      scale <- "standard"
    }
    scale
}

metric.is.log.scale <- function(metric){
  metric %in% c(binary.log.metrics, diagnostic.log.metrics)    
}

metric.is.logit.scale <- function(metric) {
  metric %in% c(binary.logit.metrics, diagnostic.logit.metrics)
}    

metric.is.arcsine.scale <- function(metric) {
  metric %in% c(binary.arcsine.metrics)
}

logit <- function(x) {
    log(x/(1-x))
}

invlogit <- function(x) {
    exp(x) / (1 + exp(x))
}

arcsine.sqrt <- function(x) {
    asin(sqrt(x))
}

invarcsine.sqrt <- function(x) {
    (sin(x))^2
}