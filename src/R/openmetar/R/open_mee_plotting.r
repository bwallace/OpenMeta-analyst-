#forest.path <- paste(params$fp_outpath, sep="")
#plot.data <- create.plot.data.continuous(cont.data, params, res)
#changed.params <- plot.data$changed.params
## list of changed params values
#params.changed.in.forest.plot <- forest.plot(forest.data=plot.data, outpath=forest.path)
#changed.params <- c(changed.params, params.changed.in.forest.plot)
#params[names(changed.params)] <- changed.params
## dump the forest plot params to disk; return path to
## this .Rdata for later use
#forest.plot.params.path <- save.data(cont.data, res, params, plot.data)
