XY.function2 <-
function (a, b) 
{
    if (b == 1) {
        result = sum(a[, 1] * a[, 2] * (a[, 3] + 0.5))
    }
    else {
        if (b == 2) {
            result = sum((1 - a[, 1]) * (1 - a[, 2]) * (a[, 3] + 
                0.5))
        }
        else {
            if (b == 3) {
                result = sum(a[, 1] * (1 - a[, 2]) * (a[, 3] + 
                  0.5))
            }
            else {
                if (b == 4) {
                  result = sum((1 - a[, 1]) * a[, 2] * (a[, 3] + 
                    0.5))
                }
            }
        }
    }
    return(result)
}
