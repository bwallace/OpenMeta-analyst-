beta.condition <-
function (a, b) 
{
    if (a == 1.5 & b == 1.5) {
        a = b = 1
    }
    else {
        a = a
        b = b
    }
    a_b = rbind(a, b)
    return(a_b)
}
