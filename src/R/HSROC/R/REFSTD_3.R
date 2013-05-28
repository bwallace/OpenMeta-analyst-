REFSTD_3 <-
function (rs, n.sample) 
{
    if (rs[[1]] == 1) {
        n_rs = sum(n.sample)
    }
    else {
        n.rs = rs[[1]]
        n_rs = numeric()
        for (i in 1:n.rs) {
            n_rs = c(n_rs, sum(n.sample[rs[[i + 1]]]))
        }
    }
    return(n_rs)
}
