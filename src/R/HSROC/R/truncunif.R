truncunif <-
function (bornes, prior.l, prior.u) 
{
    l = bornes[1]
    u = bornes[2]
    ll = numeric()
    uu = numeric()
    if (l < prior.l) {
        ll = prior.l
    }
    else {
        ll = l
    }
    if (u > prior.u) {
        uu = prior.u
    }
    else {
        uu = u
    }
    return(cbind(ll, uu))
}
