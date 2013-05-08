REFSTD_2 <-
function (n_rs, likelihood, prior) 
{
    if (n_rs[[1]] != 1) {
        x = n_rs[[1]]
        a.Se = likelihood[[1]] + likelihood[[4]] + prior[[1]]
        b.Se = likelihood[[2]] + likelihood[[3]] + prior[[2]]
        a.Sp = likelihood[[6]] + likelihood[[7]] + prior[[3]]
        b.Sp = likelihood[[5]] + likelihood[[8]] + prior[[4]]
    }
    else {
        x = 1
        a.Se = sum(likelihood[[1]] + likelihood[[4]]) + prior[[1]]
        b.Se = sum(likelihood[[2]] + likelihood[[3]]) + prior[[2]]
        a.Sp = sum(likelihood[[6]] + likelihood[[7]]) + prior[[3]]
        b.Sp = sum(likelihood[[5]] + likelihood[[8]]) + prior[[4]]
    }
    return(list(x, a.Se, b.Se, a.Sp, b.Sp))
}
