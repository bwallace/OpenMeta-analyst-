B.fonction.Ind <-
function (a, b) 
{
    result = sum((a[, 2] * (2 * a[, 1] - 1))/exp(b * (a[, 1] - 
        0.5)))
    return(result)
}
