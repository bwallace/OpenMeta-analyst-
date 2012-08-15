REFSTD_5 <-
function (rs, yy1, yy2, yy3, yy4, stu.gr, t1, t2, yij) 
{
    if (rs[[1]] == 1) {
        y1 = yy1
        y2 = yy2
        y3 = yy3
        y4 = yy4
    }
    else {
        yy = cbind(stu.gr, t1, t2, yij)
        y1 = by(yy[, 2:4], yy[, 1], FUN = XY.function, b = 1)
        y2 = by(yy[, 2:4], yy[, 1], FUN = XY.function, b = 2)
        y3 = by(yy[, 2:4], yy[, 1], FUN = XY.function, b = 3)
        y4 = by(yy[, 2:4], yy[, 1], FUN = XY.function, b = 4)
    }
    result = list(y1, y2, y3, y4)
    return(result)
}
