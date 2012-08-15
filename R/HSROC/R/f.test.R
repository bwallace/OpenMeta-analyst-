f.test <-
function (x, m1, v1, m2, v2) 
{
    if (x == 1) {
        y = rnorm(1, m1, v1)
    }
    else {
        y = rnorm(1, m2, v2)
    }
    return(y)
}
