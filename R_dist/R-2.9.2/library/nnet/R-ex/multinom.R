### Name: multinom
### Title: Fit Multinomial Log-linear Models
### Aliases: multinom add1.multinom anova.multinom coef.multinom
###   drop1.multinom extractAIC.multinom predict.multinom print.multinom
###   summary.multinom print.summary.multinom vcov.multinom
###   model.frame.multinom logLik.multinom
### Keywords: neural models

### ** Examples

options(contrasts = c("contr.treatment", "contr.poly"))
library(MASS)
example(birthwt)
(bwt.mu <- multinom(low ~ ., bwt))
## Not run: Call:
##D multinom(formula = low ~ ., data = bwt)
##D 
##D Coefficients:
##D  (Intercept)         age         lwt raceblack raceother
##D     0.823477 -0.03724311 -0.01565475  1.192371 0.7406606
##D      smoke      ptd        ht        ui       ftv1     ftv2+
##D   0.7555234 1.343648 1.913213 0.6802007 -0.4363238 0.1789888
##D 
##D Residual Deviance: 195.4755
##D AIC: 217.4755
## End(Not run)



