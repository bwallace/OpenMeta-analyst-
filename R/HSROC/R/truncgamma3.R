truncgamma3 <-
function (n = 1, p, shape, scale, l, u) 
{
    l1 <- pgamma(l, shape, scale = scale)
    u1 <- pgamma(u, shape, scale = scale)
    x <- dunif(p, l1, u1)
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
