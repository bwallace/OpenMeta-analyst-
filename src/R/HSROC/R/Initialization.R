Initialization <-
function (first.run, random, param, cond.Ind, rs, GS_se, GS_sp, 
    Data1, Data2, Data3, Data4, Data5, Data6, path, studies, 
    sco = FALSE, psa, pst) 
{
    setwd(path)
    x = rs[[1]]
    model = read.table("model.txt")
    a.beta = param[1]
    b.beta = param[2]
    a.THETA = param[3]
    b.THETA = param[4]
    a.LAMBDA = param[5]
    b.LAMBDA = param[6]
    a.disp.alpha = param[7]
    b.disp.alpha = param[8]
    a.disp.theta = param[9]
    b.disp.theta = param[10]
    a.pi = param[11]
    b.pi = param[12]
    if (length(param) == 12) {
        GS = TRUE
    }
    else {
        GS = FALSE
    }
    if (cond.Ind == "TRUE" & model == 1) {
        if (first.run == TRUE) {
            if (random == TRUE) {
                file.ini = "Random Initial values.txt"
                if (psa == "sd") {
                  init.sigma.alpha = runif(1, 1/sqrt(b.disp.alpha), 
                    1/sqrt(a.disp.alpha))
                  prec.alpha = 1/(init.sigma.alpha)^2
                }
                else {
                  if (psa == "v") {
                    init.sigma.alpha = runif(1, 1/b.disp.alpha, 
                      1/a.disp.alpha)
                    prec.alpha = 1/(init.sigma.alpha)^2
                  }
                  else {
                    if (psa == "p") {
                      init.sigma.alpha = rgamma(1, shape = a.disp.alpha, 
                        scale = b.disp.alpha)
                      prec.alpha = 1/(init.sigma.alpha)^2
                    }
                  }
                }
                if (pst == "sd") {
                  init.sigma.theta = runif(1, 1/sqrt(b.disp.theta), 
                    1/sqrt(a.disp.theta))
                  prec.theta = 1/(init.sigma.theta)^2
                }
                else {
                  if (pst == "v") {
                    init.sigma.theta = runif(1, 1/b.disp.theta, 
                      1/a.disp.theta)
                    prec.theta = 1/(init.sigma.theta)^2
                  }
                  else {
                    if (pst == "p") {
                      init.sigma.theta = rgamma(1, shape = a.disp.theta, 
                        scale = b.disp.theta)
                      prec.theta = 1/(init.sigma.theta)^2
                    }
                  }
                }
                init.THETA = runif(1, min = a.THETA, max = b.THETA)
                init.LAMBDA = runif(1, min = a.LAMBDA, max = b.LAMBDA)
                init.beta = runif(1, min = a.beta, max = b.beta)
                init.theta = rnorm(n = studies, mean = init.THETA, 
                  sd = init.sigma.theta)
                init.alpha = rnorm(n = studies, mean = init.LAMBDA, 
                  sd = init.sigma.alpha)
                init.S1 = rbeta(studies, shape1 = 1, shape2 = 1)
                init.C1 = rbeta(studies, shape1 = 1, shape2 = 1)
                init.PI = rbeta(studies, shape1 = 1, shape2 = 1)
                if (GS == TRUE) {
                  study.ref_std = "Perfect Reference standard"
                }
                else {
                  if (GS == FALSE) {
                    if (GS_se == TRUE & GS_sp == FALSE) {
                      a.C2 = param[13:(13 + (x - 1))]
                      b.C2 = param[(13 + (x)):(13 + (2 * x - 
                        1))]
                      init.S2 = 1
                      init.C2 = rbeta(x, shape1 = a.C2, shape2 = b.C2)
                      study.ref_std = rbind(init.S2, init.C2)
                      rownames(study.ref_std) = c("Perfect Sensitivity", 
                        "init.C2")
                    }
                    else {
                      if (GS_se == FALSE & GS_sp == TRUE) {
                        a.S2 = param[13:(13 + (x - 1))]
                        b.S2 = param[(13 + (x)):(13 + (2 * x - 
                          1))]
                        init.S2 = rbeta(x, shape1 = a.S2, shape2 = b.S2)
                        init.C2 = 1
                        study.ref_std = rbind(init.S2, init.C2)
                        rownames(study.ref_std) = c("init.S2", 
                          "Perfect Specificity")
                      }
                      else {
                        if (GS_se == FALSE & GS_sp == FALSE) {
                          a.S2 = param[13:(13 + (x - 1))]
                          b.S2 = param[(13 + (x)):(13 + (2 * 
                            x - 1))]
                          a.C2 = param[(13 + (2 * x)):(13 + (3 * 
                            x - 1))]
                          b.C2 = param[(13 + (3 * x)):(13 + (4 * 
                            x - 1))]
                          init.S2 = rbeta(x, shape1 = a.S2, shape2 = b.S2)
                          init.C2 = rbeta(x, shape1 = a.C2, shape2 = b.C2)
                          study.ref_std = rbind(init.S2, init.C2)
                          rownames(study.ref_std) = c("init.S2", 
                            "init.C2")
                        }
                      }
                    }
                  }
                }
                if (sco == TRUE) {
                  study.indep_par = rbind(init.THETA, init.LAMBDA, 
                    init.sigma.alpha, prec.alpha, init.beta)
                  rownames(study.indep_par) = c("init.THETA", 
                    "init.LAMBDA", "init.sigma.alpha", "prec.alpha", 
                    "init.beta")
                  study.dep_par = cbind(init.alpha, init.S1, 
                    init.C1, init.PI)
                  colnames(study.dep_par) = cbind("init.alpha", 
                    "init.S1", "init.C1", "init.PI")
                }
                else {
                  if (sco == FALSE) {
                    study.indep_par = rbind(init.THETA, init.sigma.theta, 
                      prec.theta, init.LAMBDA, init.sigma.alpha, 
                      prec.alpha, init.beta)
                    rownames(study.indep_par) = c("init.THETA", 
                      "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                      "init.sigma.alpha", "prec.alpha", "init.beta")
                    study.dep_par = cbind(init.alpha, init.theta, 
                      init.S1, init.C1, init.PI)
                    colnames(study.dep_par) = cbind("init.alpha", 
                      "init.theta", "init.S1", "init.C1", "init.PI")
                  }
                }
                write.table(study.dep_par, file = file.ini, row.names = FALSE)
                write(paste("______________________________________________________"), 
                  file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write.table(study.indep_par, file = file.ini, 
                  append = TRUE, col.names = FALSE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste("______________________________________________________"), 
                  file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write.table(study.ref_std, file = file.ini, append = TRUE, 
                  col.names = FALSE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                if (sco == TRUE) {
                  write(paste("The same cut-off value was assumed across each study."), 
                    file = file.ini, append = TRUE)
                }
            }
            else {
                file.ini = "Initial values.txt"
                init.LAMBDA = as.vector(Data4[3])
                init.sigma.alpha = as.vector(Data4[4])
                prec.alpha = as.vector((1/(init.sigma.alpha)^2))
                init.THETA = as.vector(Data4[1])
                init.sigma.theta = as.vector(Data4[2])
                prec.theta = as.vector((1/(init.sigma.theta)^2))
                init.beta = as.vector(Data4[5])
                init.alpha = Data1[, 1]
                init.theta = Data1[, 2]
                init.S1 = Data1[, 3]
                init.C1 = Data1[, 4]
                init.PI = Data1[, 5]
                if (GS == TRUE) {
                  study.ref_std = "Perfect Reference standard"
                }
                else {
                  if (GS == FALSE) {
                    if (GS_se == TRUE & GS_sp == FALSE) {
                      init.S2 = 1
                      init.C2 = Data5[2, ]
                      study.ref_std = rbind(init.S2, init.C2)
                      rownames(study.ref_std) = c("Perfect Sensitivity", 
                        "init.C2")
                    }
                    else {
                      if (GS_se == FALSE & GS_sp == TRUE) {
                        init.S2 = Data5[1, ]
                        init.C2 = 1
                        study.ref_std = rbind(init.S2, init.C2)
                        rownames(study.ref_std) = c("init.S2", 
                          "Perfect Specificity")
                      }
                      else {
                        if (GS_se == FALSE & GS_sp == FALSE) {
                          init.S2 = Data5[1, ]
                          init.C2 = Data5[2, ]
                          study.ref_std = rbind(init.S2, init.C2)
                          rownames(study.ref_std) = c("init.S2", 
                            "init.C2")
                        }
                      }
                    }
                  }
                }
                if (sco == TRUE) {
                  study.indep_par = rbind(init.THETA, init.LAMBDA, 
                    init.sigma.alpha, prec.alpha, init.beta)
                  rownames(study.indep_par) = c("init.THETA", 
                    "init.LAMBDA", "init.sigma.alpha", "prec.alpha", 
                    "init.beta")
                  study.dep_par = cbind(init.alpha, init.S1, 
                    init.C1, init.PI)
                  colnames(study.dep_par) = cbind("init.alpha", 
                    "init.S1", "init.C1", "init.PI")
                }
                else {
                  if (sco == FALSE) {
                    study.indep_par = rbind(init.THETA, init.sigma.theta, 
                      prec.theta, init.LAMBDA, init.sigma.alpha, 
                      prec.alpha, init.beta)
                    rownames(study.indep_par) = c("init.THETA", 
                      "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                      "init.sigma.alpha", "prec.alpha", "init.beta")
                    study.dep_par = cbind(init.alpha, init.theta, 
                      init.S1, init.C1, init.PI)
                    colnames(study.dep_par) = cbind("init.alpha", 
                      "init.theta", "init.S1", "init.C1", "init.PI")
                  }
                }
                write.table(study.dep_par, file = file.ini, row.names = FALSE)
                write(paste("______________________________________________________"), 
                  file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write.table(study.indep_par, file = file.ini, 
                  append = TRUE, col.names = FALSE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste("______________________________________________________"), 
                  file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                write.table(study.ref_std, file = file.ini, append = TRUE, 
                  col.names = FALSE)
                write(paste(""), file = file.ini, append = TRUE)
                write(paste(""), file = file.ini, append = TRUE)
                if (sco == TRUE) {
                  write(paste("The same cut-off value was assumed across each study."), 
                    file = file.ini, append = TRUE)
                }
            }
        }
        else {
            if (sco == TRUE) {
                init.sigma.alpha = Data3[2]
                prec.alpha = (1/(init.sigma.alpha)^2)
                init.THETA = Data3[3]
                init.LAMBDA = Data3[1]
                init.beta = Data3[4]
                init.alpha = Data2[, 1]
                init.S1 = Data2[, 2]
                init.C1 = Data2[, 3]
                init.PI = Data2[, 4]
                if (GS == FALSE) {
                  init.S2 = Data6[1, ]
                  init.C2 = Data6[2, ]
                }
            }
            else {
                if (sco == FALSE) {
                  init.sigma.theta = Data3[4]
                  prec.theta = (1/(init.sigma.theta)^2)
                  init.sigma.alpha = Data3[2]
                  prec.alpha = (1/(init.sigma.alpha)^2)
                  init.THETA = Data3[3]
                  init.LAMBDA = Data3[1]
                  init.beta = Data3[5]
                  init.alpha = Data2[, 1]
                  init.theta = Data2[, 2]
                  init.S1 = Data2[, 3]
                  init.C1 = Data2[, 4]
                  init.PI = Data2[, 5]
                  if (GS == FALSE) {
                    init.S2 = Data6[1, ]
                    init.C2 = Data6[2, ]
                  }
                }
            }
        }
        if (sco == TRUE) {
            inits1 = c(0, 0, init.sigma.alpha, prec.alpha, init.THETA, 
                init.LAMBDA, init.beta)
            inits1 = as.matrix(inits1)
            rownames(inits1) = c("", "", "init.sigma.alpha", 
                "prec.alpha", "init.THETA", "init.LAMBDA", "init.beta")
            colnames(inits1) = c("")
        }
        else {
            if (sco == FALSE) {
                inits1 = c(init.sigma.theta, prec.theta, init.sigma.alpha, 
                  prec.alpha, init.THETA, init.LAMBDA, init.beta)
                inits1 = as.matrix(inits1)
                rownames(inits1) = c("init.sigma.theta", "prec.theta", 
                  "init.sigma.alpha", "prec.alpha", "init.THETA", 
                  "init.LAMBDA", "init.beta")
                colnames(inits1) = c("")
            }
        }
        if (GS == TRUE) {
            inits3 = "Perfect Reference standard"
        }
        else {
            inits3 = rbind(init.S2, init.C2)
            rownames(inits3) = c("init.S2", "init.C2")
        }
    }
    else {
        if (cond.Ind == TRUE & model == 2) {
            a.a1 = param[13:(13 + (x - 1))]
            b.a1 = param[(13 + (x)):(13 + (2 * x - 1))]
            a.a0 = param[(13 + (2 * x)):(13 + (3 * x - 1))]
            b.a0 = param[(13 + (3 * x)):(13 + (4 * x - 1))]
            if (first.run == TRUE) {
                if (random == TRUE) {
                  file.ini = "Random Initial values.txt"
                  init.THETA = runif(1, min = a.THETA, max = b.THETA)
                  init.sigma.theta = runif(1, 1/sqrt(b.disp.theta), 
                    1/sqrt(a.disp.theta))
                  prec.theta = 1/(init.sigma.theta)^2
                  init.LAMBDA = runif(1, min = a.LAMBDA, max = b.LAMBDA)
                  init.sigma.alpha = runif(1, 1/sqrt(b.disp.alpha), 
                    1/sqrt(a.disp.alpha))
                  prec.alpha = 1/(init.sigma.alpha)^2
                  init.beta = runif(1, min = a.beta, max = b.beta)
                  init.theta = rnorm(n = studies, mean = init.THETA, 
                    sd = init.sigma.theta)
                  init.alpha = rnorm(n = studies, mean = init.LAMBDA, 
                    sd = init.sigma.alpha)
                  init.S1 = rbeta(studies, shape1 = 1, shape2 = 1)
                  init.C1 = rbeta(studies, shape1 = 1, shape2 = 1)
                  init.PI = rbeta(studies, shape1 = 1, shape2 = 1)
                  if (GS == FALSE) {
                    init.a1 = rnorm(x, mean = a.a1, sd = b.a1)
                    init.a0 = rnorm(x, mean = a.a0, sd = b.a0)
                  }
                  study.dep_par = cbind(init.alpha, init.theta, 
                    init.S1, init.C1, init.PI)
                  colnames(study.dep_par) = cbind("init.alpha", 
                    "init.theta", "init.S1", "init.C1", "init.PI")
                  study.indep_par = rbind(init.THETA, init.sigma.theta, 
                    prec.theta, init.LAMBDA, init.sigma.alpha, 
                    prec.alpha, init.beta)
                  rownames(study.indep_par) = c("init.THETA", 
                    "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                    "init.sigma.alpha", "prec.alpha", "init.beta")
                  if (GS == TRUE) {
                    study.ref_std = "Perfect Reference standard"
                  }
                  else {
                    study.ref_std = rbind(init.a1, init.a0)
                    rownames(study.ref_std) = c("init.a1", "init.a0")
                  }
                  write.table(study.dep_par, file = file.ini, 
                    row.names = FALSE)
                  write(paste("______________________________________________________"), 
                    file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write.table(study.indep_par, file = file.ini, 
                    append = TRUE, col.names = FALSE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write.table(study.ref_std, file = file.ini, 
                    append = TRUE, col.names = FALSE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                }
                else {
                  file.ini = "Initial values.txt"
                  init.LAMBDA = as.vector(Data4[3])
                  init.sigma.alpha = as.vector(Data4[4])
                  prec.alpha = as.vector((1/(init.sigma.alpha)^2))
                  init.THETA = as.vector(Data4[1])
                  init.sigma.theta = as.vector(Data4[2])
                  prec.theta = as.vector((1/(init.sigma.theta)^2))
                  init.beta = as.vector(Data4[5])
                  init.alpha = Data1[, 1]
                  init.theta = Data1[, 2]
                  init.S1 = Data1[, 3]
                  init.C1 = Data1[, 4]
                  init.PI = Data1[, 5]
                  if (GS == FALSE) {
                    init.a1 = Data5[1, ]
                    init.a0 = Data5[2, ]
                  }
                  study.indep_par = rbind(init.THETA, init.sigma.theta, 
                    prec.theta, init.LAMBDA, init.sigma.alpha, 
                    prec.alpha, init.beta)
                  rownames(study.indep_par) = c("init.THETA", 
                    "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                    "init.sigma.alpha", "prec.alpha", "init.beta")
                  study.dep_par = cbind(init.alpha, init.theta, 
                    init.S1, init.C1, init.PI)
                  colnames(study.dep_par) = cbind("init.alpha", 
                    "init.theta", "init.S1", "init.C1", "init.PI")
                  if (GS == TRUE) {
                    study.ref_std = "Perfect Reference standard"
                  }
                  else {
                    study.ref_std = rbind(init.a1, init.a0)
                    rownames(study.ref_std) = c("init.a1", "init.a0")
                  }
                  write.table(study.dep_par, file = file.ini, 
                    row.names = FALSE)
                  write(paste("______________________________________________________"), 
                    file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write.table(study.indep_par, file = file.ini, 
                    append = TRUE, col.names = FALSE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write.table(study.ref_std, file = file.ini, 
                    append = TRUE, col.names = FALSE)
                  write(paste(""), file = file.ini, append = TRUE)
                  write(paste(""), file = file.ini, append = TRUE)
                }
            }
            else {
                init.sigma.theta = Data3[4]
                prec.theta = (1/(init.sigma.theta)^2)
                init.sigma.alpha = Data3[2]
                prec.alpha = (1/(init.sigma.alpha)^2)
                init.THETA = Data3[3]
                init.LAMBDA = Data3[1]
                init.beta = Data3[5]
                init.alpha = Data2[, 1]
                init.theta = Data2[, 2]
                init.S1 = Data2[, 3]
                init.C1 = Data2[, 4]
                init.PI = Data2[, 5]
                if (GS == FALSE) {
                  init.a1 = Data6[1, ]
                  init.a0 = Data6[2, ]
                }
            }
            inits1 = c(init.sigma.theta, prec.theta, init.sigma.alpha, 
                prec.alpha, init.THETA, init.LAMBDA, init.beta)
            inits1 = as.matrix(inits1)
            rownames(inits1) = c("init.sigma.theta", "prec.theta", 
                "init.sigma.alpha", "prec.alpha", "init.THETA", 
                "init.LAMBDA", "init.beta")
            colnames(inits1) = c("")
            if (GS == TRUE) {
                inits3 = "Perfect Reference standard"
            }
            else {
                inits3 = rbind(init.a1, init.a0)
                rownames(inits3) = c("init.a1", "init.a0")
            }
        }
        else {
            if (cond.Ind == FALSE & model == 2) {
                a.d1 = param[13]
                b.d1 = param[14]
                a.d0 = param[15]
                b.d0 = param[16]
                a.a1 = param[17:(17 + (x - 1))]
                b.a1 = param[(17 + (x)):(17 + (2 * x - 1))]
                a.a0 = param[(17 + (2 * x)):(17 + (3 * x - 1))]
                b.a0 = param[(17 + (3 * x)):(17 + (4 * x - 1))]
                a.b1 = param[(17 + (4 * x)):(17 + (5 * x - 1))]
                b.b1 = param[(17 + (5 * x)):(17 + (6 * x - 1))]
                a.b0 = param[(17 + (6 * x)):(17 + (7 * x - 1))]
                b.b0 = param[(17 + (7 * x)):(17 + (8 * x - 1))]
                if (first.run == TRUE) {
                  if (random == TRUE) {
                    file.ini = "Random Initial values.txt"
                    init.THETA = runif(1, min = a.THETA, max = b.THETA)
                    init.sigma.theta = runif(1, 1/sqrt(b.disp.theta), 
                      1/sqrt(a.disp.theta))
                    prec.theta = 1/(init.sigma.theta)^2
                    init.LAMBDA = runif(1, min = a.LAMBDA, max = b.LAMBDA)
                    init.sigma.alpha = runif(1, 1/sqrt(b.disp.alpha), 
                      1/sqrt(a.disp.alpha))
                    prec.alpha = 1/(init.sigma.alpha)^2
                    init.beta = runif(1, min = a.beta, max = b.beta)
                    init.theta = rnorm(n = studies, mean = init.THETA, 
                      sd = init.sigma.theta)
                    init.alpha = rnorm(n = studies, mean = init.LAMBDA, 
                      sd = init.sigma.alpha)
                    init.S1 = rbeta(studies, shape1 = 1, shape2 = 1)
                    init.C1 = rbeta(studies, shape1 = 1, shape2 = 1)
                    init.PI = rbeta(studies, shape1 = 1, shape2 = 1)
                    if (GS == FALSE) {
                      init.a1 = rnorm(x, mean = a.a1, sd = b.a1)
                      init.a0 = rnorm(x, mean = a.a0, sd = b.a0)
                      init.b1 = runif(x, min = a.b1, max = b.b1)
                      init.b0 = runif(x, min = a.b0, max = b.b0)
                      init.d1 = runif(1, min = a.d1, max = b.d1)
                      init.d0 = runif(1, min = a.d0, max = b.d0)
                    }
                    study.dep_par = cbind(init.alpha, init.theta, 
                      init.S1, init.C1, init.PI)
                    colnames(study.dep_par) = cbind("init.alpha", 
                      "init.theta", "init.S1", "init.C1", "init.PI")
                    if (GS == TRUE) {
                      study.ref_std = "Perfect Reference standard"
                      study.indep_par = rbind(init.THETA, init.sigma.theta, 
                        prec.theta, init.LAMBDA, init.sigma.alpha, 
                        prec.alpha, init.beta)
                      rownames(study.indep_par) = c("init.THETA", 
                        "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                        "init.sigma.alpha", "prec.alpha", "init.beta")
                    }
                    else {
                      study.indep_par = rbind(init.THETA, init.sigma.theta, 
                        prec.theta, init.LAMBDA, init.sigma.alpha, 
                        prec.alpha, init.beta, init.d1, init.d0)
                      rownames(study.indep_par) = c("init.THETA", 
                        "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                        "init.sigma.alpha", "prec.alpha", "init.beta", 
                        "init.d1", "init.d0")
                      study.ref_std = rbind(init.a1, init.a0, 
                        init.b1, init.b0)
                      rownames(study.ref_std) = c("init.a1", 
                        "init.a0", "init.b1", "init.b0")
                    }
                    write.table(study.dep_par, file = file.ini, 
                      row.names = FALSE)
                    write(paste("______________________________________________________"), 
                      file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write.table(study.indep_par, file = file.ini, 
                      append = TRUE, col.names = FALSE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste("______________________________________________________"), 
                      file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write.table(study.ref_std, file = file.ini, 
                      append = TRUE, col.names = FALSE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                  }
                  else {
                    file.ini = "Initial values.txt"
                    init.LAMBDA = as.vector(Data4[3])
                    init.sigma.alpha = as.vector(Data4[4])
                    prec.alpha = as.vector((1/(init.sigma.alpha)^2))
                    init.THETA = as.vector(Data4[1])
                    init.sigma.theta = as.vector(Data4[2])
                    prec.theta = as.vector((1/(init.sigma.theta)^2))
                    init.beta = as.vector(Data4[5])
                    init.alpha = Data1[, 1]
                    init.theta = Data1[, 2]
                    init.S1 = Data1[, 3]
                    init.C1 = Data1[, 4]
                    init.PI = Data1[, 5]
                    if (GS == FALSE) {
                      init.d1 = as.vector(Data4[6])
                      init.d0 = as.vector(Data4[7])
                      init.a1 = Data5[1, ]
                      init.a0 = Data5[2, ]
                      init.b1 = Data5[3, ]
                      init.b0 = Data5[4, ]
                    }
                    study.dep_par = cbind(init.alpha, init.theta, 
                      init.S1, init.C1, init.PI)
                    colnames(study.dep_par) = cbind("init.alpha", 
                      "init.theta", "init.S1", "init.C1", "init.PI")
                    if (GS == TRUE) {
                      study.ref_std = "Perfect Reference standard"
                      study.indep_par = rbind(init.THETA, init.sigma.theta, 
                        prec.theta, init.LAMBDA, init.sigma.alpha, 
                        prec.alpha, init.beta)
                      rownames(study.indep_par) = c("init.THETA", 
                        "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                        "init.sigma.alpha", "prec.alpha", "init.beta")
                    }
                    else {
                      study.indep_par = rbind(init.THETA, init.sigma.theta, 
                        prec.theta, init.LAMBDA, init.sigma.alpha, 
                        prec.alpha, init.beta, init.d1, init.d0)
                      rownames(study.indep_par) = c("init.THETA", 
                        "init.sigma.theta", "prec.theta", "init.LAMBDA", 
                        "init.sigma.alpha", "prec.alpha", "init.beta", 
                        "init.d1", "init.d0")
                      study.ref_std = rbind(init.a1, init.a0, 
                        init.b1, init.b0)
                      rownames(study.ref_std) = c("init.a1", 
                        "init.a0", "init.b1", "init.b0")
                    }
                    write.table(study.dep_par, file = file.ini, 
                      row.names = FALSE)
                    write(paste("______________________________________________________"), 
                      file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write.table(study.indep_par, file = file.ini, 
                      append = TRUE, col.names = FALSE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste("______________________________________________________"), 
                      file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write.table(study.ref_std, file = file.ini, 
                      append = TRUE, col.names = FALSE)
                    write(paste(""), file = file.ini, append = TRUE)
                    write(paste(""), file = file.ini, append = TRUE)
                  }
                }
                else {
                  init.sigma.theta = Data3[4]
                  prec.theta = (1/(init.sigma.theta)^2)
                  init.sigma.alpha = Data3[2]
                  prec.alpha = (1/(init.sigma.alpha)^2)
                  init.THETA = Data3[3]
                  init.LAMBDA = Data3[1]
                  init.beta = Data3[5]
                  init.alpha = Data2[, 1]
                  init.theta = Data2[, 2]
                  init.S1 = Data2[, 3]
                  init.C1 = Data2[, 4]
                  init.PI = Data2[, 5]
                  if (GS == FALSE) {
                    init.d1 = Data3[6]
                    init.d0 = Data3[7]
                    init.a1 = Data6[1, ]
                    init.a0 = Data6[2, ]
                    init.b1 = Data6[3, ]
                    init.b0 = Data6[4, ]
                  }
                }
                if (GS == TRUE) {
                  inits1 = c(init.sigma.theta, prec.theta, init.sigma.alpha, 
                    prec.alpha, init.THETA, init.LAMBDA, init.beta)
                  inits1 = as.matrix(inits1)
                  inits3 = "Perfect Reference standard"
                }
                else {
                  inits1 = c(init.sigma.theta, prec.theta, init.sigma.alpha, 
                    prec.alpha, init.THETA, init.LAMBDA, init.beta, 
                    init.d1, init.d0)
                  inits1 = as.matrix(inits1)
                  inits3 = rbind(init.a1, init.a0, init.b1, init.b0)
                  rownames(inits3) = c("init.a1", "init.a0", 
                    "init.b1", "init.b0")
                }
                rownames(inits1) = c("init.sigma.theta", "prec.theta", 
                  "init.sigma.alpha", "prec.alpha", "init.THETA", 
                  "init.LAMBDA", "init.beta", "init.d1", "init.d0")
                colnames(inits1) = c("")
            }
        }
    }
    if (sco == TRUE) {
        inits2 = cbind(init.alpha, 0, init.S1, init.C1, init.PI)
        colnames(inits2) = c("init.alpha", "", "init.S1", "init.C1", 
            "init.PI")
        rownames(inits2) = seq(1, studies)
    }
    else {
        if (sco == FALSE) {
            inits2 = cbind(init.alpha, init.theta, init.S1, init.C1, 
                init.PI)
            colnames(inits2) = c("init.alpha", "init.theta", 
                "init.S1", "init.C1", "init.PI")
            rownames(inits2) = seq(1, studies)
        }
    }
    inits = list(inits1, inits2, inits3)
    return(inits)
}
