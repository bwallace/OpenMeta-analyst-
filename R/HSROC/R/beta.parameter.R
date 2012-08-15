beta.parameter <-
function (low, up) 
{
    if (any(low < 0) | any(low > 1) | any(up < 0) | any(up > 
        1)) 
        stop("Range limits must fall within [0, 1]")
    if (all(low < up) == FALSE) 
        stop("minimum argument 'low' must be < maximum argument 'up'")
    mu = (up + low)/2
    s = (up - low)/4
    a = (-mu * (s^2 + mu^2 - mu))/s^2
    b = ((mu - 1) * (s^2 + mu^2 - mu))/s^2
    results = mapply(beta.condition, a, b)
    rownames(results) = c("alpha", "beta")
    colnames(results) = 1:length(low)
    return(results)
}
