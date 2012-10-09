Which_data <-
function (RANDOM, data, init, GS) 
{
    if (GS == FALSE) {
        if (RANDOM == TRUE) {
            sv = numeric()
            sv2 = numeric()
            svrs = numeric()
            result = list(sv, sv2, svrs)
            return(result)
        }
        else {
            sv = init[[1]]
            sv2 = init[[2]]
            svrs = init[[3]]
            result = list(sv, sv2, svrs)
            return(result)
        }
    }
    else {
        if (RANDOM == TRUE) {
            sv = numeric()
            sv2 = numeric()
            svrs = numeric()
            result = list(sv, sv2, svrs)
            return(result)
        }
        else {
            sv = init[[1]]
            sv2 = init[[2]]
            svrs = NULL
            result = list(sv, sv2, svrs)
            return(result)
        }
    }
}
