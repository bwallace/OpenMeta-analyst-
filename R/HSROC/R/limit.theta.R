limit.theta <-
function (x, x.lim) 
{
    if (is.na(x)) {
        x = x.lim
    }
    else {
        x = x
    }
    return(x)
}
