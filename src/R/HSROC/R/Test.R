Test <-
function (data) 
{
    t1 = t2 = numeric()
    if (data == 0) {
        t1 = c(t1, 0)
        t2 = c(t2, 0)
    }
    else {
        if (data == 1) {
            t1 = c(t1, 1)
            t2 = c(t2, 1)
        }
        else {
            if (data == 2) {
                t1 = c(t1, 1)
                t2 = c(t2, 0)
            }
            else {
                if (data == 3) {
                  t1 = c(t1, 0)
                  t2 = c(t2, 1)
                }
            }
        }
    }
    t = cbind(t1, t2)
    return(t)
}
