REFSTD_6_SE <-
function (refstd, N.refstd, A.Se2, B.Se2) 
{
    if (refstd == TRUE) {
        se = 1
    }
    else {
        se = rbeta(n = N.refstd, shape1 = A.Se2, shape2 = B.Se2)
    }
    results = list(se)
    names(results) = list("SE")
    return(results)
}
