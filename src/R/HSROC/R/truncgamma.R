truncgamma <-
function (n = 1, shape, scale, l, u) 
{
    l1 <- pgamma(l, shape, scale = scale)
    u1 <- pgamma(u, shape, scale = scale)
    x <- runif(n, l1, u1)
    if (x == 0) {
        y = u
    }
    else {
        if (x == 1) {
            y = l
        }
        else {
            y = qgamma(p = x, shape = shape, scale = scale)
        }
    }
    return(y)
}
