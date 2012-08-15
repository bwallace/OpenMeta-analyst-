f <-
function (a, borne) 
{
    low = borne[1]
    up = borne[2]
    if (a > up) {
        b = up
    }
    else {
        if (a < low) {
            b = low
        }
        else {
            b = a
        }
    }
    return(b)
}
