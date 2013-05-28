truncgamma2 <-
function (n = 1, l, u, r, y, a, e) 
{
    alpha.G = sum(y)/2
    beta.G = 0.5 * sum(y * (r - a - e)^2)
    l1 <- pgamma(q = l, shape = alpha.G, rate = beta.G)
    u1 <- pgamma(q = u, shape = alpha.G, rate = beta.G)
    x <- runif(n, l1, u1)
    if (x != 0 & x < 1e-16) {
        x = 1e-16
    }
    else {
        x = x
    }
    if (x == 0) {
        y = u
    }
    else {
        if (x == 1) {
            y = l
        }
        else {
            y = qgamma(p = x, shape = alpha.G, rate = beta.G)
        }
    }
    results = c(y, alpha.G, beta.G)
    return(results)
}
