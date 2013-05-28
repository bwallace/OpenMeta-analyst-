M_H2_IND2 <-
function (r, y, a, N, low.beta, up.beta, x) 
{
    acc_rate = 0
    alpha.G = sum(0.5 * (1 - y))
    beta.G = 0.5 * sum((1 - y) * (r + 0.5 * a)^2)
    alpha.IG = sum(0.5 * y)
    beta.IG = 0.5 * sum(y * (r - 0.5 * a)^2)
    can <- 1/truncgamma(n = 1, shape = alpha.IG, scale = 1/beta.IG, 
        l = low.beta, u = up.beta)
    ratio = dgamma(can, alpha.G, rate = beta.G)/dgamma(x, alpha.G, 
        rate = beta.G)
    if (is.na(ratio) == TRUE) {
        files.remove()
    }
    if (is.na(ratio) == TRUE) {
        cat(paste("Unsuitable initial values were provided. "))
        stop("Please respecify and call HSROC() again.\n  If you're using 'init=NULL' you need just to run the 'HSROC' function again.\n")
    }
    aprob = min(1, ratio)
    u = runif(1)
    if (u < aprob) {
        x = can
        acc_rate = 1
    }
    return(c(x, acc_rate, aprob, can))
}
