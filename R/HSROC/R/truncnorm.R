truncnorm <-
function (t, n, m, sd, limit) 
{
    l = limit[, 1] * t + limit[, 2] * (1 - t)
    u = limit[, 1] * (1 - t) + limit[, 3] * t
    l1 <- pnorm((l - m)/sd)
    u1 <- pnorm((u - m)/sd)
    x <- runif(n, l1, u1)
    y = qnorm(x) * sd + m
    results = cbind(y, l, u)
    return(results)
}
