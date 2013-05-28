REFSTD_6_SP <-
function (refstd, N.refstd, A.Sp2, B.Sp2) 
{
    if (refstd == TRUE) {
        sp = 1
    }
    else {
        sp = rbeta(n = N.refstd, shape1 = A.Sp2, shape2 = B.Sp2)
    }
    results = list(sp)
    names(results) = list("SP")
    return(results)
}
