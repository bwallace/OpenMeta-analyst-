f.rij <-
function (l, u, r) 
{
    if ((is.na(l) == TRUE) | (is.na(u) == TRUE) | (is.na(r) == 
        TRUE)) {
        files.remove()
    }
    if ((is.na(l) == TRUE) | (is.na(u) == TRUE) | (is.na(r) == 
        TRUE)) {
        cat(paste("Unsuitable initial values were provided. "))
        stop("Please respecify and call HSROC() again.\n  If you're using 'init=NULL' you need just to run the 'HSROC' function again.\n")
    }
    if (r < l) {
        b = l
    }
    else {
        if (r > u) {
            b = u
        }
        else {
            b = r
        }
    }
    return(b)
}
