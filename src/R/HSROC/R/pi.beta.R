pi.beta <-
function (a, b) 
{
    n = length(a)
    result = n - sum(a) + b
    return(result)
}
