
####################################################################
# examples
#
#   meta.results <- bivariate.dx.test(TP, FP, FN, TN)
#   meta.results
#
#
#   plot.bivariate(meta.results, TP, FP, FN , TN, plot.points = T , 
#                data.range = T, line.color = "red",
#                weights = T)
####################################################################

bivariate.dx.test <- function(TP, FP, FN, TN)  {
    library(lme4)
    n.studies <- length(TP)
    persons <- TP + FP + FN + TN
    true.positive <- NA
    false.positive <- NA
    false.negative <- NA
    true.negative <- NA

    for (i in 1:n.studies) {    
      true.positive <- na.exclude(c(true.positive, rep(1, TP[i])))  
      false.positive <- na.exclude(c( false.positive, rep(0, FP[i])))   
      false.negative <- na.exclude(c(false.negative, rep(0, FN[i])))
      true.negative <- na.exclude(c( true.negative, rep(1, TN[i]))) 
    }
     
    study.id <- NA
    for (i in 1:n.studies) {
        study.id <- na.exclude(c(   study.id , rep(i, TP[i]))) 
    } 
    for (i in 1:n.studies) {
        study.id <- na.exclude(c(   study.id , rep(i, FN[i]))) 
    } 
    for (i in 1:n.studies) {
        study.id <- na.exclude(c(   study.id , rep(i, FP[i]))) 
    } 
    for (i in 1:n.studies) {
        study.id <- na.exclude(c(   study.id , rep(i, TN[i]))) 
     } 

    group.id.disease <- rep(1, length(true.positive) + length(false.negative))
    group.id.nodisease <- rep(0, length(false.positive) + length(true.negative))    
    group.id1 <- c(group.id.disease , group.id.nodisease) 
    group.id2 <- c(group.id.disease -1 , group.id.nodisease + 1)
      
    results <- c(true.positive ,false.negative, false.positive,  true.negative)
    data.reshape <- data.frame(results, group.id1 , group.id2, study.id)
     
    model <- lmer( results ~ 0 +  group.id1 + group.id2 + 
                            ( 0 + group.id1 + group.id2  |study.id) ,
                  family = binomial , data = data.reshape, nAGQ = 3) 

    logit_sens <- model@fixef[1]
    logist_spec <- model@fixef[2]
    stde <- coef(summary(model))[, "Std. Error"]

    se_logit_sens <- stde[1]
    se_logit_spec <- stde[2]

    results <- VarCorr(model)
    var_sens <- results$study.id[1,1]
    var_spec <- results$study.id[2,2]
    covar <- results$study.id[2,1]
    correlation <- covar / (var_sens * var_spec)^0.5

    meta.results <- data.frame(logit_sens, logist_spec, se_logit_sens, se_logit_spec, 
                                var_sens, var_spec, covar, correlation)

    row.names(meta.results) <- "estimates"

    meta.results
}



## plotting an HSROC based on the bivariate model
plot.bivariate <- function(meta.results, TP, FP, FN, TN, plot.points=TRUE, 
                    data.range=TRUE, line.color = "red", weights = TRUE, scale = 0.01,
                    filepath="./r_tmp/bivariate") {
    slope <- (meta.results[1,5] / meta.results[1,6])^0.5
    intercept <-    meta.results[1,1]  +  meta.results[1,2]*(meta.results[1,5] /meta.results[1,6])^0.5

    # note that this assumes counts have been zero-corrected
    sens <- TP / (TP +FN)
    fnr <- 1- TN / (TN + FP)
    total <- TP + FP + FN + TN
        
    if (data.range == TRUE) {
        low = min(fnr)
        high = max(fnr)
    } 
    if (data.range == FALSE)  {
        low = 0
        high = 1    
    }
        
    if (weights == TRUE) {
        total = total
        scale = scale
    }
    else {
        total = 1
        scale = 1
    }
        
    if (plot.points == TRUE) {
        png(file = paste(filepath, ".png", sep=""), height=960, width=960)
        plot(sens ~ fnr , asp=1, ylim = c(0,1) , xlim = c(0,1), ylab = "Sensitivity", xlab              = "1 - Specificity", cex = scale*total) 
        curve(    1/ ( 1 +  exp(-(intercept + slope*log(x/(1-x)  )   )   )  )   
                    , add = TRUE, xlim = c(low, high) , col = line.color)
        dev.off()

        pdf(file = paste(filepath, ".pdf", sep=""))
        plot(sens ~ fnr , asp=1, ylim = c(0,1) , xlim = c(0,1), ylab = "Sensitivity", xlab              = "1 - Specificity", cex = scale*total) 
        curve(    1/ ( 1 +  exp(-(intercept + slope*log(x/(1-x)  )   )   )  )   
                    , add = TRUE, xlim = c(low, high) , col = line.color)
        dev.off()
    }  
    if (plot.points == FALSE) {
        png(file = paste(filepath, ".png", sep=""), height=960, width=960)
        plot(sens ~ fnr , asp=1, ylim = c(0,1) , xlim = c(0,1), ylab = "Sensitivity", xlab              = "1 - Specificity", col = "white" )    
        curve(    1/ ( 1 +  exp(-(intercept + slope*log(x/(1-x)  )   )   )  ), xlim = c(low, high), ylab = "", xlab = "", add = TRUE, col = line.color )
        dev.off()   

        pdf(file = "bivariate_meta.pdf")
        plot(sens ~ fnr , asp=1, ylim = c(0,1) , xlim = c(0,1), ylab = "Sensitivity", xlab              = "1 - Specificity", col = "white" )    
        curve(    1/ ( 1 +  exp(-(intercept + slope*log(x/(1-x)  )   )   )  ), xlim = c(low, high), ylab = "", xlab = "", add = TRUE, col = line.color )
        dev.off()   
    }


}
    
    




