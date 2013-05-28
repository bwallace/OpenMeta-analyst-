Restore <-
function (break_point, gold_std) 
{
    if (break_point > 0) {
        if (break_point == 1) {
            alpha = read.table("alpha.txt")
            count = length(alpha[, 1])
            k = count
            x = 0
            while (is.na(x) == FALSE) {
                x = sum(alpha[k, ])
                k = k - 1
            }
        }
        else {
            if (break_point == 2) {
                theta = read.table("theta.txt")
                count = length(theta[, 1])
                k = count
                x = 0
                while (is.na(x) == FALSE) {
                  x = sum(theta[k, ])
                  k = k - 1
                }
            }
            else {
                if (break_point == 3) {
                  s1 = read.table("Sens1.txt")
                  count = length(s1[, 1])
                  k = count
                  x = 0
                  while (is.na(x) == FALSE) {
                    x = sum(s1[k, ])
                    k = k - 1
                  }
                }
                else {
                  if (break_point == 4) {
                    c1 = read.table("Spec1.txt")
                    count = length(c1[, 1])
                    k = count
                    x = 0
                    while (is.na(x) == FALSE) {
                      x = sum(c1[k, ])
                      k = k - 1
                    }
                  }
                  else {
                    if (break_point == 5) {
                      pi = read.table("PI.txt")
                      count = length(pi[, 1])
                      k = count
                      x = 0
                      while (is.na(x) == FALSE) {
                        x = sum(pi[k, ])
                        k = k - 1
                      }
                    }
                    else {
                      if (break_point == 6) {
                        lambda = read.table("LAMBDA.txt")
                        count = length(lambda[, 1])
                        k = count
                        x = 0
                        while (is.na(x) == FALSE) {
                          x = sum(lambda[k, ])
                          k = k - 1
                        }
                      }
                      else {
                        if (break_point == 7) {
                          sig.alph = read.table("sigma.alpha.txt")
                          count = length(sig.alph[, 1])
                          k = count
                          x = 0
                          while (is.na(x) == FALSE) {
                            x = sum(sig.alph[k, ])
                            k = k - 1
                          }
                        }
                        else {
                          if (break_point == 8) {
                            ctheta = read.table("capital_THETA.txt")
                            count = length(ctheta[, 1])
                            k = count
                            x = 0
                            while (is.na(x) == FALSE) {
                              x = sum(ctheta[k, ])
                              k = k - 1
                            }
                          }
                          else {
                            if (break_point == 9) {
                              sig.thet = read.table("sigma.theta.txt")
                              count = length(sig.thet[, 1])
                              k = count
                              x = 0
                              while (is.na(x) == FALSE) {
                                x = sum(sig.thet[k, ])
                                k = k - 1
                              }
                            }
                            else {
                              if (break_point == 10) {
                                beta = read.table("beta.txt")
                                count = length(beta[, 1])
                                k = count
                                x = 0
                                while (is.na(x) == FALSE) {
                                  x = beta[k, 1]
                                  k = k - 1
                                }
                              }
                              else {
                                if (break_point == 11) {
                                  s2 = read.table("Sens2.txt")
                                  count = length(s2[, 1])
                                  k = count
                                  x = 0
                                  while (is.na(x) == FALSE) {
                                    x = sum(s2[k, ])
                                    k = k - 1
                                  }
                                }
                                else {
                                  if (break_point == 11) {
                                    c2 = read.table("Spec2.txt")
                                    count = length(c2[, 1])
                                    k = count
                                    x = 0
                                    while (is.na(x) == FALSE) {
                                      x = sum(c2[k, ])
                                      k = k - 1
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
            }
        }
        vec.alpha = read.table("alpha.txt")[k, ]
        vec.theta = as.vector(read.table("theta.txt")[k, ])
        vec.S1 = read.table("Sens1.txt")[k, ]
        vec.C1 = read.table("Spec1.txt")[k, ]
        vec.PI = read.table("PI.txt")[k, ]
        vec.LAMBDA = read.table("LAMBDA.txt")[k, 1]
        vec.sigma.alpha = read.table("sigma.alpha.txt")[k, 1]
        vec.THETA = read.table("capital_THETA.txt")[k, 1]
        vec.sigma.theta = read.table("sigma.theta.txt")[k, 1]
        vec.beta = read.table("beta.txt")[k, 1]
        columns = length(vec.alpha[k, ])
        write.table(rbind(vec.alpha, vec.theta, vec.S1, vec.C1, 
            vec.PI), file = "Restore.txt", append = TRUE, row.names = FALSE, 
            col.names = FALSE)
        write(c(vec.LAMBDA, vec.sigma.alpha, vec.THETA, vec.sigma.theta, 
            vec.beta), file = "Restore2.txt", append = TRUE)
        write(paste("______________________________________________________"), 
            file = "Restore_index.txt", append = TRUE)
        write(paste("\t   Restore.txt "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Row 1 : alpha parameters for all M = ", 
            columns, " study(ies)\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Row 2 : theta parameters for all M = ", 
            columns, " study(ies)\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Row 3 : sensitivity of test under evaluation (S1) for all M = ", 
            columns, " study(ies)\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Row 4 : specificity of test under evaluation (C1) for all M = ", 
            columns, " study(ies)\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Row 5 : prevalence for all M = ", columns, 
            " study(ies)\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = "Restore_index.txt", append = TRUE)
        write(paste("\t   Restore2.txt "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Column 1 : LAMBDA parameter\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Column 2 : sigma alpha parameter\t    "), 
            file = "Restore_index.txt", append = TRUE)
        write(paste("Column 3 : THETA parameter\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("Column 4 : sigma theta parameter\t    "), 
            file = "Restore_index.txt", append = TRUE)
        write(paste("Column 5 : beta parameter\t    "), file = "Restore_index.txt", 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = "Restore_index.txt", append = TRUE)
        if (gold_std == FALSE) {
            vec.S2 = read.table("Sens2.txt")[k, ]
            vec.C2 = read.table("Spec2.txt")[k, ]
            refstd = length(read.table("Sens2.txt")[k, ])
            write(t(cbind(vec.S2, vec.C2)), file = "Restore3.txt", 
                append = TRUE, ncolumns = refstd)
            write(paste("\t   Restore3.txt "), file = "Restore_index.txt", 
                append = TRUE)
            write(paste("Row 1 : sensitivity of reference test (S2) \t    "), 
                file = "Restore_index.txt", append = TRUE)
            write(paste("Row 2 : specificity of reference test (C2) \t    "), 
                file = "Restore_index.txt", append = TRUE)
        }
    }
    else {
        if (break_point == 0) {
            columns = NULL
        }
    }
}
