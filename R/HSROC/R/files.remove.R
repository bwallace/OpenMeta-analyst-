files.remove <-
function (...) 
{
    if (file.exists("alpha.txt") == TRUE) {
        file.remove("alpha.txt")
    }
    if (file.exists("beta.txt") == TRUE) {
        file.remove("beta.txt")
    }
    if (file.exists("C_overall.txt") == TRUE) {
        file.remove("C_overall.txt")
    }
    if (file.exists("capital.THETA.txt") == TRUE) {
        file.remove("capital.THETA.txt")
    }
    if (file.exists("Initial values.txt") == TRUE) {
        file.remove("Initial values.txt")
    }
    if (file.exists("LAMBDA.txt") == TRUE) {
        file.remove("LAMBDA.txt")
    }
    if (file.exists("model.txt") == TRUE) {
        file.remove("model.txt")
    }
    if (file.exists("PI.txt") == TRUE) {
        file.remove("PI.txt")
    }
    if (file.exists("Prior.information.txt") == TRUE) {
        file.remove("Prior.information.txt")
    }
    if (file.exists("S_overall.txt") == TRUE) {
        file.remove("S_overall.txt")
    }
    if (file.exists("Sens1.txt") == TRUE) {
        file.remove("Sens1.txt")
    }
    if (file.exists("Sens2.txt") == TRUE) {
        file.remove("Sens2.txt")
    }
    if (file.exists("sigma.alpha.txt") == TRUE) {
        file.remove("sigma.alpha.txt")
    }
    if (file.exists("sigma.theta.txt") == TRUE) {
        file.remove("sigma.theta.txt")
    }
    if (file.exists("Spec1.txt") == TRUE) {
        file.remove("Spec1.txt")
    }
    if (file.exists("Spec2.txt") == TRUE) {
        file.remove("Spec2.txt")
    }
    if (file.exists("theta.txt") == TRUE) {
        file.remove("theta.txt")
    }
    if (file.exists("C2.txt") == TRUE) {
        file.remove("C2.txt")
    }
    if (file.exists("Random Initial values.txt") == TRUE) {
        file.remove("Random Initial values.txt")
    }
    if (file.exists("S2.txt") == TRUE) {
        file.remove("S2.txt")
    }
    if (file.exists("REstart_values_index.txt") == TRUE) {
        file.remove("REstart_values_index.txt")
    }
    if (file.exists("REstarting REFSTD.txt") == TRUE) {
        file.remove("REstarting REFSTD.txt")
    }
    if (file.exists("REstarting values.txt") == TRUE) {
        file.remove("REstarting values.txt")
    }
    if (file.exists("REstarting values 2.txt") == TRUE) {
        file.remove("REstarting values 2.txt")
    }
}
