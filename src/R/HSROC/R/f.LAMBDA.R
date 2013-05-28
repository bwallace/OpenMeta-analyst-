f.LAMBDA <-
function (a, borne1, borne2) 
{
    if (a > borne2) {
        b = borne2
    }
    else {
        if (a < borne1) {
            b = borne1
        }
        else {
            b = a
        }
    }
    return(b)
}
