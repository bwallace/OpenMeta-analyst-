####################################
#                                  #
# OpenMeta[Analyst]                #
# ----                             #
# utilities.r                      #
#                                  #
# Utilities for pretty-printing    #
# results.                         #
####################################


print.summary.display <- function(results,...) {
    # Prints results summary
    cat(results$model.title)
    cat("\n\n")
    if (!is.na(results$het.data$Title)) {
        cat(results$het.data$Title)
        cat("\n")
        print(results$het.data$het.table)
        cat("\n")
    }
    if (!is.na(results$result.data$Title)) {
        cat(results$result.data$Title)
        cat("\n")
        print(results$result.data$result.table)
        cat("\n")
    }
    if (!is.na(results$alt.data$Title)) {
        cat(results$alt.data$Title)
        cat("\n")
        print(results$alt.data$alt.table)
    }

    #if (length(binary.data@g1O1) > 0) {
    #    raw.data<-extract.data(binary.data, params)
    #    cat("  Data\n")
    #    print(raw.data)
    #}
}

print.reg.display <- function(reg.disp, ...) {
     # Prints regression statistics
     print(reg.disp$reg.data$reg.table)
}

print.cum.display <- function(cum.disp, ...) {
    # Prints cumulative analysis results summary
    print(cum.disp$cum.data$cum.table)
}

print.table <- function(table.data) {
    # Prints an array table.data with lines separating rows and columns.
    #rowLength <- 1
    num.rows <- length(table.data[,1])
    num.cols <- length(table.data[1,])
    # Compute column widths
    col.widths <- NULL
    for (col.index in 1:num.cols) {
      col.widths <- c(col.widths, max(nchar(table.data[,col.index])) + 4)
    }
    table.width <- sum(col.widths) + num.cols + 1
    # Create line of dashes of length table.width - 2
    dash.line <- NULL
    for (count in 1:(table.width - 2)) {
        dash.line <- paste(dash.line, "-", sep="")
    }
    top.line <- paste("+", dash.line, "+", sep="")
    middleLine <- paste("|", dash.line, "|", sep="")

    # Build table
    cat(top.line)
    cat("\n")
    for (row.index in 1:num.rows) {
        table.row <- "|"
        for (col.index in 1:num.cols) {
            col.width <- col.widths[col.index]
            entry <- pad.entry(table.data[row.index,col.index], col.width)
            table.row <- paste(table.row, entry, "|", sep="")
        }
        cat(table.row)
        cat("\n")
        if (row.index < num.rows) {
            cat(middleLine)
            cat("\n")
        }
    }
    cat(top.line)
    cat("\n")
}

pad.entry <- function(entry, col.width) {
    # Adds spaces to entry so that it will be centered in a column of width col.width.
    # Pad a table entry with zeros
    for (i in 1:floor((col.width - nchar(entry))/2)) {
        entry <- paste(" ", entry, sep="")
    }
    for (i in 1:ceiling(col.width - nchar(entry))/2) {
        entry <- paste(entry, " ", sep="")
    }
    return(entry)
}

round.display <- function(x, digits) {
    # Prints "< 10^(-digits)" if x is < 10^(-digits) or "x" otherwise
    x.disp <- round(x, digits)
    for (count in 1:length(x)) {
        if (abs(x.disp[count]) < 10^(-digits)) {
          x.disp[count] <- paste("< ", 10^(-digits), sep = "", collapse = "")
        }
    }
    return(x.disp)
}

round.with.zeros <- function(x, digits) {
    # Rounds a number according to digits and pads  with zeros at the end, if necessary,
    # so that there are digits symbols after the decimal point.
    y <- NULL
    for (i in 1:length(x)) {
      x.rounded <- round(x[i], digits = digits)
      numZeros <- NULL
      if (floor(x[i]) == x[i]) {
        # x is an integer
        if (digits > 0) {
          x.rounded <- paste(x.rounded, ".", sep="")
          for (count in 1:digits) {
            x.rounded <- paste(x.rounded, "0", sep="")
          }
        }
      }
      else {
        pow <- 10**digits * x.rounded
        # Calculate how many zeros should be added on the right.
        while (floor(pow) == pow) {
          pow <- pow/10;
          if (floor(pow) == pow) {
            x.rounded <- paste(x.rounded, "0", sep="")
          }
        }
      }
      y <- c(y, toString(x.rounded))
    }
    return(y)
}

create.summary.disp <- function(res, params, degf, model.title) {
    QLabel =  paste("Q(df = ", degf, ")", sep="")
    I2 <- max(0, (res$QE - degf)/res$QE)
    I2 <- paste(100 * round(I2, digits = 2), "%")
    QE <- round(res$QE, digits=params$digits)
    QEp <- round.display(res$QEp, digits=params$digits)
    het.array <-  array(c(QLabel, QE, "p-Value", QEp, "I^2", I2), dim=c(2,3))
    class(het.array) <- "table"
    het.data <- list("Title" = "  Test for Heterogeneity", "het.table" = het.array)
    est.disp <- round(binary.transform.f(params$measure)$display.scale(res$b), digits=params$digits)
    lb.disp <- round(binary.transform.f(params$measure)$display.scale(res$ci.lb), digits=params$digits)
    ub.disp <- round(binary.transform.f(params$measure)$display.scale(res$ci.ub), digits=params$digits)

    pVal <- round.display(res$pval, digits=params$digits)
    zVal <- round(res$zval, digits=params$digits)
    se <- round(res$se, digits=params$digits)

    if (binary.transform.f(params$measure)$display.scale(1)!= binary.transform.f(params$measure)$calc.scale(1)) {
         res.array <- array(c("Estimate", est.disp, "p-Value", pVal, "Z-Value", zVal, "Lower bound", lb.disp,
                                        "Upper bound", ub.disp), dim=c(2,5))
         #class(res.array) <- "table"
         #result.data <- list("Title" = "  Model Results (reporting scale)", "result.table" = res.array)
         estCalc <- round(res$b, digits=params$digits)
         lbCalc <- round(res$ci.lb, digits=params$digits)
         ubCalc <- round(res$ci.ub, digits=params$digits)
         alt.array <- array(c("Estimate", estCalc, "SE", se, "Lower bound", lbCalc, "Upper bound", ubCalc), dim=c(2,4))
         class(alt.array) <- "table"
         alt.data <- list("Title" = "  Model Results (calculation scale)", "alt.table" = alt.array)
    }

    else {
        res.array <- array(c("Estimate", est.disp, "SE", se, "p-Value", pVal, "Z-Value", zVal, "Lower bound", lb.disp,
                                        "Upper bound", ub.disp), dim=c(2,6))

        alt.data <- list("Title" = NA)
    }
    class(res.array) <- "table"
    result.data <- list("Title" = "  Model Results (reporting scale)", "result.table" = res.array)
    summary.disp <- list("modelTitle" = model.title, "het.data" = het.data, "result.data" = result.data,
                        "alt.data" = alt.data, "rawResults" = res)
    class(summary.disp) <- "summary.display"
    return(summary.disp)
}

create.regression.disp <- function(res, params) {
    coeffs <- round(res$b, digits=params$digits)
    pvals <- round.display(res$pval, digits=params$digits)
    lbs <- round(res$ci.lb, digits=params$digits)
    ubs <- round(res$ci.ub, digits=params$digits)
    reg.array <- array(c("", "Intercept", "Slope", "Estimates", coeffs[1], coeffs[2], "p-Values", pvals[1], pvals[2],
                      "Lower bounds", lbs[1], lbs[2], "Upper bounds", ubs[1], ubs[2]), dim=c(3, 5))
    class(reg.array) <- "table"
    reg.data <- list("Title" = "", "reg.table" = reg.array)
    reg.disp <- list("reg.data" = reg.data)
    class(reg.disp) <-  "reg.display"
    return(reg.disp)
}

create.overall.display <- function(res, study.names, params) {
    res
    params$digits
    res <- round(res, digits = params$digits)
    cum.array <- array(c("", study.names, "Estimates", res[,1], "Lower bounds", res[,2],"Upper bounds", res[,3]),
                    dim=c(length(study.names) + 1, 4))
    class(cum.array) <- "table"
    cum.data <- list("Title" = "", "cum.table" = cum.array)
    cum.disp <- list("cum.data" = cum.data)
    class(cum.disp) <- "cum.display"
    return(cum.disp)
}