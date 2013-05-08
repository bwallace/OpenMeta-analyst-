REFSTD_4 <-
function (rs, n.sample, n_rs) 
{
    if (rs[[1]] == 1) {
        x = rep(1, n_rs)
    }
    else {
        x = rep(1:rs[[1]], n_rs)
    }
    return(x)
}
