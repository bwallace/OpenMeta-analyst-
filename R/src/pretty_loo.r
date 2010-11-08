
#example data
results <- list("images"= NULL, "loo_results"=list(y = c(0,-1,2), SE = c(1, 1, 1)), "loo_labels"=c("mitsos", "kitsos ", "thitsos"), "plot_names"="kitsos")
params <- list(measure="OR", conf.level=95, digits=3)

# pretty priting of LOO results

extractDataLOO <- function(results , title = "Leave-one-out sensitivity analysis") { 
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
    
    LOO.display.frame(extractData(results))
    
    
    
padEntry <- function(entry, colWidth) {
    # Adds spaces to entry so that it will be centered in a column of width colWidth.
    # Pad a table entry with zeros
    for (i in 1:floor((colWidth - nchar(entry))/2)) {
        entry <- paste(" ", entry, sep="")
    }
    for (i in 1:ceiling(colWidth - nchar(entry))/2) {
        entry <- paste(entry, " ", sep="")
    }
    return(entry)
}


print.Table <- function(table.data) {
    # Prints an array tableData with lines separating rows and columns.
    numRows <- length(table.data[,1])
    numCols <- length(table.data[1,])
    # Compute column widths
    colWidths <- NULL
    for (colIndex in 1:numCols) {
      colWidths <- c(colWidths, max(nchar(table.data[,colIndex])) + 4)  # 4 is a arbitrary gap
    }
    tableWidth <- sum(colWidths) + numCols + 1 
    # Create line of dashes of length tableWidth - 2
    dashLine <- NULL
    for (count in 1:(tableWidth - 2)) {
        dashLine <- paste(dashLine, "-", sep="")
    }
    topLine <- paste("+", dashLine, "+", sep="")
    middleLine <- paste("|", dashLine, "|", sep="")
      
    # Build table
    cat(topLine)
    cat("\n")
    for (rowIndex in 1:numRows) {
        tableRow <- "|"
        for (colIndex in 1:numCols) {
            colWidth <- colWidths[colIndex]
            entry <- padEntry(table.data[rowIndex,colIndex], colWidth)
            tableRow <- paste(tableRow, entry, "|", sep="")
        }
        cat(tableRow)
        cat("\n")
        if (rowIndex < numRows) {
            cat(middleLine) 
            cat("\n")
        } 
    } 
    cat(topLine)
    cat("\n")
}    
 

print.Table(LOO.display.frame(extractData(results)))






