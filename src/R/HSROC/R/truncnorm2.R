truncnorm2 <-
function (l, u, m, sd, n) 
{
    l1 <- pnorm((l - m)/sd)
    u1 <- pnorm((u - m)/sd)
    x <- runif(n, l1, u1)
    if (x == 0) {
        y = u
    }
    else {
        if (x == 1) {
            y = l
        }
        else {
            y <- qnorm(x) * sd + m
        }
    }
    return(y)
}
