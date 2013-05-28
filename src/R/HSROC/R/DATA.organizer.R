DATA.organizer <-
function (d, m) 
{
    n = rowSums(d)
    all.studies = c()
    for (i in 1:m) {
        all.studies = c(all.studies, c(rep(1, d[i, 1]), rep(2, 
            d[i, 2]), rep(3, d[i, 3]), rep(0, d[i, 4])))
    }
    results = list(n, all.studies)
}
