exp.b <-
function (b, y, a, d1, e, d0, r) 
{
    f.b = exp(b * (-1 + sum(0.5 * y))) * exp(-0.5 * exp(b) * 
        sum(y * (r + 0.5 * (a + d1 * e))^2)) * exp(-b * (1 + 
        sum(0.5 * (1 - y)))) * exp(-0.5 * exp(-b) * sum((1 - 
        y) * (r - 0.5 * (a + d0 * e))^2))
    return(f.b)
}
