HSROCSummary <-
function (data, burn_in = 0, iter.keep = NULL, Thin = 1, sub_rs = NULL, 
    point_estimate = c("median", "mean"), path = getwd(), chain = NULL, 
    tv = NULL, digit = 6, print_plot = FALSE, png.too=TRUE) 
{
    setwd(path)
    # keep a list of image names to png and PDF paths
    image.list <- list()
    if (missing(data)) 
        stop("You must provide a valid 'data' argument", call. = FALSE)
    N = length(data[, 1])
    if (burn_in < 0) {
        cat("The 'burn_in' argument must be greater or equal than zero. \n")
        stop("Please respecify and call HSROCSummary() again.\n")
    }
    if (is.null(iter.keep) == FALSE) {
        if (iter.keep < 0) {
            cat("The 'iter.keep' argument must be greater or equal than zero. \n")
            stop("Please respecify and call HSROCSummary() again.\n")
        }
    }
    if (Thin < 1) {
        cat("The 'Thin' argument must be greater or equal than 1. \n")
        stop("Please respecify and call HSROCSummary() again.\n")
    }
    if (is.null(sub_rs) == TRUE) {
        sub_rs = list(1, 1:N)
    }
    if (sub_rs[[1]] != (length(sub_rs) - 1)) {
        cat(paste("The value of the first element of 'sub_rs' (sub_rs[[1]] = ", 
            sub_rs[[1]], " ) does not match the number of remaining elements (length(sub_rs[[2:", 
            length(sub_rs), "]])) = ", length(2:length(sub_rs)), 
            "\n", sep = ""))
        stop("Please respecify and call HSROCSummary() again.\n")
    }
    if (is.logical(print_plot) == FALSE) {
        cat("The 'print_plot' argument must be a logical object. \n")
        stop("Please respecify and call HSROCSummary() again.\n")
    }
    file.prior = "Prior.information.txt"
    prior_dist_PI = "beta"
    if (is.null(chain) == FALSE) {
        setwd(chain[[1]])
        nb_chains = length(chain)
    }
    else {
        nb_chains = 1
    }
    point_estimate = match.arg(point_estimate)
    prior.exist = file.exists(file.prior)
    if (prior.exist == FALSE) {
        stop(paste("Make sure the \"", file.prior, "\" file created by the 'HSROC' function is in the \"", 
            path, "\" working directory. \n", sep = ""))
    }
    prior = read.table(file.prior, header = TRUE)
    model = read.table("model.txt", header = FALSE)
    Gold_se = (read.table("S2.txt", header = FALSE) == 1)
    Gold_sp = (read.table("C2.txt", header = FALSE) == 1)
    prior_sig_t = read.table("Prior on sigma_theta.txt", header = FALSE)
    prior_sig_a = read.table("Prior on sigma_alpha.txt", header = FALSE)
    if (length(prior[, 1]) == 7) {
        Gold_Std = TRUE
        condInd = TRUE
    }
    else {
        if (length(prior[, 1]) > 7 & length(prior[, 1]) <= 7 + 
            2 * sub_rs[[1]]) {
            Gold_Std = FALSE
            condInd = TRUE
        }
        else {
            if (length(prior[, 1]) > 7 + 2 * sub_rs[[1]]) {
                Gold_Std = FALSE
                condInd = FALSE
            }
        }
    }
    if (is.null(tv) == FALSE) {
        real_life = FALSE
        if (sum(dim(tv[[1]])) != N + 5) {
            cat(paste("The true value for the within-study parameters were misspecified. Make sure the ordering described in the help file is preserved. \n"))
            stop("Please respecify and call HSROCSummary() again.\n")
        }
        if (length(tv[[2]]) != 7) {
            cat(paste("The true value for the between-study parameters were misspecified. Make sure the ordering described in the help file is preserved. \n"))
            stop("Please respecify and call HSROCSummary() again.\n")
        }
        if (Gold_Std == FALSE) {
            if (sum(dim(tv[[3]])) != sub_rs[[1]] + 2) {
                cat(paste("The true value for the test under evaluation were misspecified. Make sure the ordering described in the help file is preserved. \n"))
                stop("Please respecify and call HSROCSummary() again.\n")
            }
        }
    }
    else {
        real_life = TRUE
    }
    beta.a = prior[1, 1]
    beta.b = prior[1, 2]
    prior.THETA.lower = prior[2, 1]
    prior.THETA.upper = prior[2, 2]
    prior.LAMBDA.lower = prior[3, 1]
    prior.LAMBDA.upper = prior[3, 2]
    l.disp.alpha = prior[4, 1]
    u.disp.alpha = prior[4, 2]
    l.disp.theta = prior[5, 1]
    u.disp.theta = prior[5, 2]
    low.pi = prior[6, 1]
    up.pi = prior[6, 2]
    low.rj = prior[7, 1]
    up.rj = prior[7, 2]
    SCO = FALSE
    rs.length = sub_rs[[1]]
    if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
        if (Gold_se == TRUE & Gold_sp == FALSE) {
            low.sp = prior[8:(8 + (rs.length - 1)), 1]
            up.sp = prior[8:(8 + (rs.length - 1)), 2]
            prior_dist_S2 = NULL
            prior_dist_C2 = "beta"
        }
        else {
            if (Gold_sp == TRUE & Gold_se == FALSE) {
                low.se = prior[8:(8 + (rs.length - 1)), 1]
                up.se = prior[8:(8 + (rs.length - 1)), 2]
                prior_dist_C2 = NULL
                prior_dist_S2 = "beta"
            }
            else {
                low.se = prior[8:(8 + (rs.length - 1)), 1]
                up.se = prior[8:(8 + (rs.length - 1)), 2]
                low.sp = prior[(8 + rs.length):(8 + (2 * rs.length - 
                  1)), 1]
                up.sp = prior[(8 + rs.length):(8 + (2 * rs.length - 
                  1)), 2]
                prior_dist_S2 = "beta"
                prior_dist_C2 = "beta"
            }
        }
    }
    else {
        if (condInd == TRUE & Gold_Std == FALSE & model == 2) {
            mean.a1 = prior[8:(8 + (rs.length - 1)), 1]
            sd.a1 = prior[8:(8 + (rs.length - 1)), 2]
            mean.a0 = prior[(8 + rs.length):(8 + (2 * rs.length - 
                1)), 1]
            sd.a0 = prior[(8 + rs.length):(8 + (2 * rs.length - 
                1)), 2]
        }
        else {
            if (condInd == FALSE) {
                low.d1 = prior[8, 1]
                up.d1 = prior[8, 2]
                low.d0 = prior[9, 1]
                up.d0 = prior[9, 2]
                mean.a1 = prior[10:(10 + (rs.length - 1)), 1]
                sd.a1 = prior[10:(10 + (rs.length - 1)), 2]
                mean.a0 = prior[(10 + rs.length):(10 + (2 * rs.length - 
                  1)), 1]
                sd.a0 = prior[(10 + rs.length):(10 + (2 * rs.length - 
                  1)), 2]
                low.b1 = prior[(10 + (2 * rs.length)):(10 + (3 * 
                  rs.length - 1)), 1]
                up.b1 = prior[(10 + (2 * rs.length)):(10 + (3 * 
                  rs.length - 1)), 2]
                low.b0 = prior[(10 + (3 * rs.length)):(10 + (4 * 
                  rs.length - 1)), 1]
                up.b0 = prior[(10 + (3 * rs.length)):(10 + (4 * 
                  rs.length - 1)), 2]
            }
        }
    }
    if (prior_dist_PI == "beta") {
        alpha.PI = beta.parameter(low = low.pi, up = up.pi)[1, 
            ]
        beta.PI = beta.parameter(low = low.pi, up = up.pi)[2, 
            ]
    }
    else {
        if (prior_dist_PI == "uniform") {
            alpha.PI = low.pi
            beta.PI = up.pi
        }
    }
    if (model == 1) {
        if (Gold_se == TRUE) {
            Sens2.alpha = Sens2.beta = NULL
        }
        else {
            Sens2.alpha = beta.parameter(low = low.se, up = up.se)[1, 
                ]
            Sens2.beta = beta.parameter(low = low.se, up = up.se)[2, 
                ]
        }
    }
    if (model == 1) {
        if (Gold_sp == TRUE) {
            Spec2.alpha = Spec2.beta = NULL
        }
        else {
            Spec2.alpha = beta.parameter(low = low.sp, up = up.sp)[1, 
                ]
            Spec2.beta = beta.parameter(low = low.sp, up = up.sp)[2, 
                ]
        }
    }
    file.theta = "theta.txt"
    file.alpha = "alpha.txt"
    file.capital.THETA = "capital_THETA.txt"
    file.LAMBDA = "LAMBDA.txt"
    file.beta = "beta.txt"
    file.PI = "PI.txt"
    file.sigma.alpha = "sigma.alpha.txt"
    file.sigma.theta = "sigma.theta.txt"
    file.Sens2 = "Sens2.txt"
    file.Spec2 = "Spec2.txt"
    file.Sens1 = "Sens1.txt"
    file.Spec1 = "Spec1.txt"
    file.result = "estimate.txt"
    file.C_overall = "C_overall.txt"
    file.S_overall = "S_overall.txt"
    file.d1 = "d1.txt"
    file.d0 = "d0.txt"
    file.a1 = "a1.txt"
    file.a0 = "a0.txt"
    file.b1 = "b1.txt"
    file.b0 = "b0.txt"
    numb.iter = scan("iter.txt", quiet = TRUE)
    if (is.null(iter.keep) == TRUE) {
        iter.num = numb.iter
    }
    else {
        iter.num = iter.keep
    }
    if ((iter.num - burn_in)/Thin < 100) 
        stop("You don't have enough iterations to estimate the MC error.  After taking into account the \"burn in\" and \"thinning interval\", you need at least 100 iterations to proceed.")
    if (is.null(chain) == TRUE) {
        N1 = N * iter.num
        t1 <- file("alpha.txt", "rb")
        t2 <- file("theta.txt", "rb")
        t3 <- file("PI.txt", "rb")
        t4 <- file("Sens1.txt", "rb")
        t5 <- file("Spec1.txt", "rb")
        alpha_bin = readBin(t1, double(), n = N1, endian = "little")
        theta_bin = readBin(t2, double(), n = N1, endian = "little")
        pi_bin = readBin(t3, double(), n = N1, endian = "little")
        Sens1_bin = readBin(t4, double(), n = N1, endian = "little")
        Spec1_bin = readBin(t5, double(), n = N1, endian = "little")
        alpha = matrix(0, ncol = N, nrow = iter.num)
        theta = matrix(0, ncol = N, nrow = iter.num)
        PI = matrix(0, ncol = N, nrow = iter.num)
        S1 = matrix(0, ncol = N, nrow = iter.num)
        C1 = matrix(0, ncol = N, nrow = iter.num)
        for (i in 1:N) {
            sequence = seq(i, iter.num * N, N)
            alpha[, i] = alpha_bin[sequence]
            theta[, i] = theta_bin[sequence]
            PI[, i] = pi_bin[sequence]
            S1[, i] = Sens1_bin[sequence]
            C1[, i] = Spec1_bin[sequence]
        }
        close(t1)
        close(t2)
        close(t3)
        close(t4)
        close(t5)
        t6 <- file("capital_THETA.txt", "rb")
        THETA = readBin(t6, double(), n = iter.num, endian = "little")
        close(t6)
        t7 <- file("LAMBDA.txt", "rb")
        LAMBDA = readBin(t7, double(), n = iter.num, endian = "little")
        close(t7)
        t8 <- file("beta.txt", "rb")
        beta = readBin(t8, double(), n = iter.num, endian = "little")
        close(t8)
        t9 <- file("Sens1_new.txt", "rb")
        S1_new = readBin(t9, double(), n = iter.num, endian = "little")
        close(t9)
        t10 <- file("Spec1_new.txt", "rb")
        C1_new = readBin(t10, double(), n = iter.num, endian = "little")
        close(t10)
        t11 <- file("sigma.alpha.txt", "rb")
        sigma.alpha = readBin(t11, double(), n = iter.num, endian = "little")
        close(t11)
        t12 <- file("sigma.theta.txt", "rb")
        sigma.theta = readBin(t12, double(), n = iter.num, endian = "little")
        close(t12)
        t14 <- file("S_overall.txt", "rb")
        S_overall = readBin(t14, double(), n = iter.num, endian = "little")
        close(t14)
        t15 <- file("C_overall.txt", "rb")
        C_overall = readBin(t15, double(), n = iter.num, endian = "little")
        close(t15)
        total = iter.num
        q = burn_in
        alpha = alpha[(q + 1):total, ]
        THETA = THETA[(q + 1):total]
        LAMBDA = LAMBDA[(q + 1):total]
        beta = beta[(q + 1):total]
        PI = PI[(q + 1):total, ]
        sigma.alpha = sigma.alpha[(q + 1):total]
        S1 = S1[(q + 1):total, ]
        C1 = C1[(q + 1):total, ]
        S1_new = S1_new[(q + 1):total]
        C1_new = C1_new[(q + 1):total]
        C_overall = C_overall[(q + 1):total]
        S_overall = S_overall[(q + 1):total]
        theta = theta[(q + 1):total, ]
        sigma.theta = sigma.theta[(q + 1):total]
        taille = length((q + 1):total)
        thin.interval = Thin
        thin = seq(1, taille, by = thin.interval)
        alpha = as.matrix(alpha)
        alpha = alpha[thin, ]
        THETA = THETA[thin]
        LAMBDA = LAMBDA[thin]
        beta = beta[thin]
        PI = as.matrix(PI)
        PI = PI[thin, ]
        sigma.alpha = sigma.alpha[thin]
        S1 = as.matrix(S1)
        S1 = S1[thin, ]
        C1 = as.matrix(C1)
        C1 = C1[thin, ]
        S1_new = S1_new[thin]
        C1_new = C1_new[thin]
        C_overall = C_overall[thin]
        S_overall = S_overall[thin]
        theta = as.matrix(theta)
        theta = theta[thin, ]
        sigma.theta = sigma.theta[thin]
        if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
            if (Gold_se == TRUE & Gold_sp == FALSE) {
                C2 = read.table(file.Spec2)
                C2 = C2[q:total, ]
                C2 = as.matrix(C2)
                C2 = C2[thin, ]
            }
            else {
                if (Gold_sp == TRUE & Gold_se == FALSE) {
                  S2 = read.table(file.Sens2)
                  S2 = S2[(q + 1):total, ]
                  S2 = as.matrix(S2)
                  S2 = S2[thin, ]
                }
                else {
                  N2 = rs.length * iter.num
                  t16 <- file("Sens2.txt", "rb")
                  t17 <- file("Spec2.txt", "rb")
                  Sens2_bin = readBin(t16, double(), n = N1, 
                    endian = "little")
                  Spec2_bin = readBin(t17, double(), n = N1, 
                    endian = "little")
                  S2 = matrix(0, ncol = rs.length, nrow = iter.num)
                  C2 = matrix(0, ncol = rs.length, nrow = iter.num)
                  for (j in 1:rs.length) {
                    sequence = seq(j, iter.num * rs.length, rs.length)
                    S2[, j] = Sens2_bin[sequence]
                    C2[, j] = Spec2_bin[sequence]
                  }
                  close(t16)
                  close(t17)
                  S2 = S2[(q + 1):total, ]
                  C2 = C2[(q + 1):total, ]
                  S2 = as.matrix(S2)
                  C2 = as.matrix(C2)
                  S2 = S2[thin, ]
                  C2 = C2[thin, ]
                }
            }
        }
        else {
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                2) {
                a1 = read.table(file.a1)
                a0 = read.table(file.a0)
                S2 = read.table(file.Sens2)
                C2 = read.table(file.Spec2)
                a1 = a1[(q + 1):total, ]
                a0 = a0[(q + 1):total, ]
                S2 = S2[(q + 1):total, ]
                C2 = C2[(q + 1):total, ]
                a1 = as.matrix(a1)
                a0 = as.matrix(a0)
                a1 = a1[thin, ]
                a0 = a0[thin, ]
                S2 = as.matrix(S2)
                C2 = as.matrix(C2)
                S2 = S2[thin, ]
                C2 = C2[thin, ]
            }
            else {
                if (condInd == FALSE) {
                  d1 = read.table(file.d1)
                  d0 = read.table(file.d0)
                  a1 = read.table(file.a1)
                  a0 = read.table(file.a0)
                  b1 = read.table(file.b1)
                  b0 = read.table(file.b0)
                  S2 = read.table(file.Sens2)
                  C2 = read.table(file.Spec2)
                  d1 = d1[(q + 1):total, ]
                  d0 = d0[(q + 1):total, ]
                  a1 = a1[(q + 1):total, ]
                  a0 = a0[(q + 1):total, ]
                  b1 = b1[(q + 1):total, ]
                  b0 = b0[(q + 1):total, ]
                  S2 = S2[(q + 1):total, ]
                  C2 = C2[(q + 1):total, ]
                  d1 = as.matrix(d1)
                  d0 = as.matrix(d0)
                  a1 = as.matrix(a1)
                  a0 = as.matrix(a0)
                  b1 = as.matrix(b1)
                  b0 = as.matrix(b0)
                  S2 = as.matrix(S2)
                  C2 = as.matrix(C2)
                  d1 = d1[thin, ]
                  d0 = d0[thin, ]
                  a1 = a1[thin, ]
                  a0 = a0[thin, ]
                  b1 = b1[thin, ]
                  b0 = b0[thin, ]
                  S2 = S2[thin, ]
                  C2 = C2[thin, ]
                }
            }
        }
    }
    else {
        if (is.null(chain) == FALSE) {
            K = length(chain)
            theta = alpha = THETA = LAMBDA = beta = PI = sigma.alpha = sigma.theta = S1 = C1 = S1_new = C1_new = S2 = C2 = S_overall = C_overall = a1 = a0 = b1 = b0 = d1 = d0 = numeric()
            for (k in 1:K) {
                setwd(chain[[k]])
                N1 = N * iter.num
                t1 <- file("alpha.txt", "rb")
                t2 <- file("theta.txt", "rb")
                t3 <- file("PI.txt", "rb")
                t4 <- file("Sens1.txt", "rb")
                t5 <- file("Spec1.txt", "rb")
                alpha_bin = readBin(t1, double(), n = N1, endian = "little")
                theta_bin = readBin(t2, double(), n = N1, endian = "little")
                pi_bin = readBin(t3, double(), n = N1, endian = "little")
                Sens1_bin = readBin(t4, double(), n = N1, endian = "little")
                Spec1_bin = readBin(t5, double(), n = N1, endian = "little")
                a = matrix(0, ncol = N, nrow = iter.num)
                t = matrix(0, ncol = N, nrow = iter.num)
                p = matrix(0, ncol = N, nrow = iter.num)
                S.1 = matrix(0, ncol = N, nrow = iter.num)
                C.1 = matrix(0, ncol = N, nrow = iter.num)
                for (i in 1:N) {
                  sequence = seq(i, iter.num * N, N)
                  a[, i] = alpha_bin[sequence]
                  t[, i] = theta_bin[sequence]
                  p[, i] = pi_bin[sequence]
                  S.1[, i] = Sens1_bin[sequence]
                  C.1[, i] = Spec1_bin[sequence]
                }
                close(t1)
                close(t2)
                close(t3)
                close(t4)
                close(t5)
                t6 <- file("capital_THETA.txt", "rb")
                T = readBin(t6, double(), n = iter.num, endian = "little")
                close(t6)
                t7 <- file("LAMBDA.txt", "rb")
                L = readBin(t7, double(), n = iter.num, endian = "little")
                close(t7)
                t8 <- file("beta.txt", "rb")
                b = readBin(t8, double(), n = iter.num, endian = "little")
                close(t8)
                t9 <- file("Sens1_new.txt", "rb")
                S.1_new = readBin(t9, double(), n = iter.num, 
                  endian = "little")
                close(t9)
                t10 <- file("Spec1_new.txt", "rb")
                C.1_new = readBin(t10, double(), n = iter.num, 
                  endian = "little")
                close(t10)
                t11 <- file("sigma.alpha.txt", "rb")
                sig.a = readBin(t11, double(), n = iter.num, 
                  endian = "little")
                close(t11)
                t12 <- file("sigma.theta.txt", "rb")
                sig.t = readBin(t12, double(), n = iter.num, 
                  endian = "little")
                close(t12)
                t14 <- file("S_overall.txt", "rb")
                S.ov = readBin(t14, double(), n = iter.num, endian = "little")
                close(t14)
                t15 <- file("C_overall.txt", "rb")
                C.ov = readBin(t15, double(), n = iter.num, endian = "little")
                close(t15)
                total = iter.num
                q = burn_in
                a = a[(q + 1):total, ]
                T = T[(q + 1):total]
                L = L[(q + 1):total]
                b = b[(q + 1):total]
                p = p[(q + 1):total, ]
                sig.a = sig.a[(q + 1):total]
                S.1 = S.1[(q + 1):total, ]
                C.1 = C.1[(q + 1):total, ]
                S.1_new = S.1_new[(q + 1):total]
                C.1_new = C.1_new[(q + 1):total]
                C.ov = C.ov[(q + 1):total]
                S.ov = S.ov[(q + 1):total]
                t = t[(q + 1):total, ]
                sig.t = sig.t[(q + 1):total]
                taille = length((q + 1):total)
                thin.interval = Thin
                thin = seq(1, taille, by = thin.interval)
                a = as.matrix(a)
                a = a[thin, ]
                T = T[thin]
                L = L[thin]
                b = b[thin]
                p = as.matrix(p)
                p = p[thin, ]
                sig.a = sig.a[thin]
                S.1 = as.matrix(S.1)
                S.1 = S.1[thin, ]
                C.1 = as.matrix(C.1)
                C.1 = C.1[thin, ]
                S.1_new = S.1_new[thin]
                C.1_new = C.1_new[thin]
                C.ov = C.ov[thin]
                S.ov = S.ov[thin]
                t = as.matrix(t)
                t = t[thin, ]
                sig.t = sig.t[thin]
                alpha = rbind(alpha, as.matrix(a))
                THETA = rbind(THETA, as.matrix(T))
                LAMBDA = rbind(LAMBDA, as.matrix(L))
                beta = rbind(beta, as.matrix(b))
                PI = rbind(PI, as.matrix(p))
                sigma.alpha = rbind(sigma.alpha, as.matrix(sig.a))
                S1 = rbind(S1, as.matrix(S.1))
                C1 = rbind(C1, as.matrix(C.1))
                S1_new = rbind(S1_new, as.matrix(S.1_new))
                C1_new = rbind(C1_new, as.matrix(C.1_new))
                C_overall = rbind(C_overall, as.matrix(C.ov))
                S_overall = rbind(S_overall, as.matrix(S.ov))
                theta = rbind(theta, as.matrix(t))
                sigma.theta = rbind(sigma.theta, as.matrix(sig.t))
                if (condInd == TRUE & Gold_Std == FALSE & model == 
                  1) {
                  if (Gold_se == TRUE & Gold_sp == FALSE) {
                    C.2 = read.table(file.Spec2)
                    C.2 = C.2[(q + 1):total, ]
                    C.2 = as.matrix(C.2)
                    C.2 = C.2[thin, ]
                    C2 = rbind(C2, as.matrix(C.2))
                  }
                  else {
                    if (Gold_sp == TRUE & Gold_se == FALSE) {
                      S.2 = read.table(file.Sens2)
                      S.2 = S.2[(q + 1):total, ]
                      S.2 = as.matrix(S.2)
                      S.2 = S.2[thin, ]
                      S2 = rbind(S2, as.matrix(S.2))
                    }
                    else {
                      N2 = rs.length * iter.num
                      t16 <- file("Sens2.txt", "rb")
                      t17 <- file("Spec2.txt", "rb")
                      Sens2_bin = readBin(t16, double(), n = N1, 
                        endian = "little")
                      Spec2_bin = readBin(t17, double(), n = N1, 
                        endian = "little")
                      S.2 = matrix(0, ncol = rs.length, nrow = iter.num)
                      C.2 = matrix(0, ncol = rs.length, nrow = iter.num)
                      for (i in 1:rs.length) {
                        sequence = seq(i, iter.num * rs.length, 
                          rs.length)
                        S.2[, i] = Sens2_bin[sequence]
                        C.2[, i] = Spec2_bin[sequence]
                      }
                      close(t16)
                      close(t17)
                      S.2 = S.2[(q + 1):total, ]
                      C.2 = C.2[(q + 1):total, ]
                      S.2 = as.matrix(S.2)
                      C.2 = as.matrix(C.2)
                      S.2 = S.2[thin, ]
                      C.2 = C.2[thin, ]
                      S2 = rbind(S2, as.matrix(S.2))
                      C2 = rbind(C2, as.matrix(C.2))
                    }
                  }
                }
                else {
                  if (condInd == TRUE & Gold_Std == FALSE & model == 
                    2) {
                    a.1 = read.table(file.a1)
                    a.0 = read.table(file.a0)
                    S.2 = read.table(file.Sens2)
                    C.2 = read.table(file.Spec2)
                    a.1 = a.1[(q + 1):total, ]
                    a.0 = a.0[(q + 1):total, ]
                    S.2 = S.2[(q + 1):total, ]
                    C.2 = C.2[(q + 1):total, ]
                    a.1 = as.matrix(a.1)
                    a.0 = as.matrix(a.0)
                    a.1 = a.1[thin, ]
                    a.0 = a.0[thin, ]
                    S.2 = as.matrix(S.2)
                    C.2 = as.matrix(C.2)
                    S.2 = S.2[thin, ]
                    C.2 = C.2[thin, ]
                    a1 = rbind(a1, as.matrix(a.1))
                    a0 = rbind(a0, as.matrix(a.0))
                    S2 = rbind(S2, as.matrix(S.2))
                    C2 = rbind(C2, as.matrix(C.2))
                  }
                  else {
                    if (condInd == FALSE) {
                      d.1 = read.table(file.d1)
                      d.0 = read.table(file.d0)
                      a.1 = read.table(file.a1)
                      a.0 = read.table(file.a0)
                      b.1 = read.table(file.b1)
                      b.0 = read.table(file.b0)
                      S.2 = read.table(file.Sens2)
                      C.2 = read.table(file.Spec2)
                      d.1 = d.1[(q + 1):total, ]
                      d.0 = d.0[(q + 1):total, ]
                      a.1 = a.1[(q + 1):total, ]
                      a.0 = a.0[(q + 1):total, ]
                      b.1 = b.1[(q + 1):total, ]
                      b.0 = b.0[(q + 1):total, ]
                      S.2 = S.2[(q + 1):total, ]
                      C.2 = C.2[(q + 1):total, ]
                      d.1 = as.matrix(d.1)
                      d.0 = as.matrix(d.0)
                      a.1 = as.matrix(a.1)
                      a.0 = as.matrix(a.0)
                      b.1 = as.matrix(b.1)
                      b.0 = as.matrix(b.0)
                      d.1 = d.1[thin, ]
                      d.0 = d.0[thin, ]
                      a.1 = a.1[thin, ]
                      a.0 = a.0[thin, ]
                      b.1 = b.1[thin, ]
                      b.0 = b.0[thin, ]
                      S.2 = as.matrix(S.2)
                      C.2 = as.matrix(C.2)
                      S.2 = S.2[thin, ]
                      C.2 = C.2[thin, ]
                      a1 = rbind(a1, as.matrix(a.1))
                      a0 = rbind(a0, as.matrix(a.0))
                      b1 = rbind(b1, as.matrix(b.1))
                      b0 = rbind(b0, as.matrix(b.0))
                      d1 = rbind(d1, as.matrix(d.1))
                      d0 = rbind(d0, as.matrix(d.0))
                      S2 = rbind(S2, as.matrix(S.2))
                      C2 = rbind(C2, as.matrix(C.2))
                    }
                  }
                }
            }
        }
    }
    setwd(path)
    alpha = as.mcmc(alpha)
    THETA = as.mcmc(THETA)
    LAMBDA = as.mcmc(LAMBDA)
    beta = as.mcmc(beta)
    PI = as.mcmc(PI)
    sigma.alpha = as.mcmc(sigma.alpha)
    S1 = as.mcmc(S1)
    C1 = as.mcmc(C1)
    S1_new = as.mcmc(S1_new)
    C1_new = as.mcmc(C1_new)
    C_overall = as.mcmc(C_overall)
    S_overall = as.mcmc(S_overall)
    theta = as.mcmc(theta)
    sigma.theta = as.mcmc(sigma.theta)
    if (point_estimate == "mean") {
        mean_OR_med = 2
    }
    else {
        if (point_estimate == "median") {
            mean_OR_med = 3
        }
    }
    iter.size = length(beta)
    theta.est = apply(as.matrix(theta), 2, point_estimate)
    theta.HPD = HPDinterval(theta)
    theta.sd = sd(theta)
    sigma.theta.est = apply(as.matrix(sigma.theta), 2, point_estimate)
    sigma.theta.HPD = HPDinterval(sigma.theta)
    sigma.theta.sd = sd(sigma.theta)
    alpha.est = apply(as.matrix(alpha), 2, point_estimate)
    alpha.HPD = HPDinterval(alpha)
    alpha.sd = sd(alpha)
    THETA.est = apply(as.matrix(THETA), 2, point_estimate)
    THETA.HPD = HPDinterval(THETA)
    THETA.sd = sd(THETA)
    LAMBDA.est = apply(as.matrix(LAMBDA), 2, point_estimate)
    LAMBDA.HPD = HPDinterval(LAMBDA)
    LAMBDA.sd = sd(LAMBDA)
    beta.est = apply(as.matrix(beta), 2, point_estimate)
    beta.HPD = HPDinterval(beta)
    beta.sd = sd(beta)
    PI.est = apply(as.matrix(PI), 2, point_estimate)
    PI.HPD = HPDinterval(PI)
    PI.sd = sd(PI)
    sigma.alpha.est = apply(as.matrix(sigma.alpha), 2, point_estimate)
    sigma.alpha.HPD = HPDinterval(sigma.alpha)
    sigma.alpha.sd = sd(sigma.alpha)
    S1.est = apply(as.matrix(S1), 2, point_estimate)
    S1.HPD = HPDinterval(S1)
    S1.sd = sd(S1)
    C1.est = apply(as.matrix(C1), 2, point_estimate)
    C1.HPD = HPDinterval(C1)
    C1.sd = sd(C1)
    S1_new.est = apply(as.matrix(S1_new), 2, point_estimate)
    S1_new.HPD = HPDinterval(S1_new)
    S1_new.sd = sd(S1_new)
    C1_new.est = apply(as.matrix(C1_new), 2, point_estimate)
    C1_new.HPD = HPDinterval(C1_new)
    C1_new.sd = sd(C1_new)
    C_overall.est = apply(as.matrix(C_overall), 2, point_estimate)
    C_overall.HPD = HPDinterval(C_overall)
    C_overall.sd = sd(C_overall)
    S_overall.est = apply(as.matrix(S_overall), 2, point_estimate)
    S_overall.HPD = HPDinterval(S_overall)
    S_overall.sd = sd(S_overall)
    if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
        if (Gold_se == TRUE & Gold_sp == FALSE) {
            C2 = as.mcmc(C2)
            C2.est = apply(as.matrix(C2), 2, point_estimate)
            C2.HPD = HPDinterval(C2)
            C2.sd = sd(C2)
        }
        else {
            if (Gold_sp == TRUE & Gold_se == FALSE) {
                S2 = as.mcmc(S2)
                S2.est = apply(as.matrix(S2), 2, point_estimate)
                S2.HPD = HPDinterval(S2)
                S2.sd = sd(S2)
            }
            else {
                S2 = as.mcmc(S2)
                C2 = as.mcmc(C2)
                S2.est = apply(as.matrix(S2), 2, point_estimate)
                S2.HPD = HPDinterval(S2)
                S2.sd = sd(S2)
                C2.est = apply(as.matrix(C2), 2, point_estimate)
                C2.HPD = HPDinterval(C2)
                C2.sd = sd(C2)
            }
        }
    }
    else {
        if (condInd == TRUE & Gold_Std == FALSE & model == 2) {
            a1 = as.mcmc(a1)
            a0 = as.mcmc(a0)
            S2 = as.mcmc(S2)
            C2 = as.mcmc(C2)
            a1.est = apply(as.matrix(a1), 2, point_estimate)
            a1.HPD = HPDinterval(a1)
            a1.sd = sd(a1)
            a0.est = apply(as.matrix(a0), 2, point_estimate)
            a0.HPD = HPDinterval(a0)
            a0.sd = sd(a0)
            S2.est = apply(as.matrix(S2), 2, point_estimate)
            S2.HPD = HPDinterval(S2)
            S2.sd = sd(S2)
            C2.est = apply(as.matrix(C2), 2, point_estimate)
            C2.HPD = HPDinterval(C2)
            C2.sd = sd(C2)
        }
        else {
            if (condInd == FALSE) {
                a1 = as.mcmc(a1)
                a0 = as.mcmc(a0)
                b1 = as.mcmc(b1)
                b0 = as.mcmc(b0)
                d1 = as.mcmc(d1)
                d0 = as.mcmc(d0)
                S2 = as.mcmc(S2)
                C2 = as.mcmc(C2)
                a1.est = apply(as.matrix(a1), 2, point_estimate)
                a1.HPD = HPDinterval(a1)
                a1.sd = sd(a1)
                a0.est = apply(as.matrix(a0), 2, point_estimate)
                a0.HPD = HPDinterval(a0)
                a0.sd = sd(a0)
                b1.est = apply(as.matrix(b1), 2, point_estimate)
                b1.HPD = HPDinterval(b1)
                b1.sd = sd(b1)
                b0.est = apply(as.matrix(b0), 2, point_estimate)
                b0.HPD = HPDinterval(b0)
                b0.sd = sd(b0)
                d1.est = apply(as.matrix(d1), 2, point_estimate)
                d1.HPD = HPDinterval(d1)
                d1.sd = sd(d1)
                d0.est = apply(as.matrix(d0), 2, point_estimate)
                d0.HPD = HPDinterval(d0)
                d0.sd = sd(d0)
                S2.est = apply(as.matrix(S2), 2, point_estimate)
                S2.HPD = HPDinterval(S2)
                S2.sd = sd(S2)
                C2.est = apply(as.matrix(C2), 2, point_estimate)
                C2.HPD = HPDinterval(C2)
                C2.sd = sd(C2)
            }
        }
    }
    batch = 50
    ssize = iter.size/batch
    moy.t = moy.a = moy.T = moy.L = moy.b = moy.p = moy.sig.a = moy.sig.t = moy.s2 = moy.c2 = moy.s1 = moy.c1 = moy.s1_new = moy.c1_new = moy.s.ov = moy.c.ov = moy.a1 = moy.a0 = moy.b1 = moy.b0 = moy.d1 = moy.d0 = numeric()
    if (N == 1) {
        for (i in 1:batch) {
            moy.a = c(moy.a, mean(alpha[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)])/ssize)
            moy.p = c(moy.p, mean(PI[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)])/ssize)
            moy.s1 = c(moy.s1, mean(as.matrix(S1[round((1 + ssize * 
                (i - 1)), 0):round((ssize * i), 0)]))/ssize)
            moy.c1 = c(moy.c1, mean(as.matrix(C1[round((1 + ssize * 
                (i - 1)), 0):round((ssize * i), 0)]))/ssize)
            moy.L = c(moy.L, mean(LAMBDA[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)]))
            moy.T = c(moy.T, mean(THETA[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)]))
            moy.b = c(moy.b, mean(beta[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)]))
            moy.sig.a = c(moy.sig.a, mean(sigma.alpha[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.s.ov = c(moy.s.ov, mean(S_overall[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.c.ov = c(moy.c.ov, mean(C_overall[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.s1_new = c(moy.s1_new, mean(S1_new[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.c1_new = c(moy.c1_new, mean(C1_new[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.t = c(moy.t, mean(theta[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)])/ssize)
            moy.sig.t = c(moy.sig.t, mean(sigma.theta[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
        }
    }
    else {
        for (i in 1:batch) {
            moy.a = cbind(moy.a, colSums(alpha[round((1 + ssize * 
                (i - 1)), 0):round((ssize * i), 0), ])/ssize)
            moy.p = cbind(moy.p, colSums(PI[round((1 + ssize * 
                (i - 1)), 0):round((ssize * i), 0), ])/ssize)
            moy.s1 = cbind(moy.s1, colSums(as.matrix(S1[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0), ]))/ssize)
            moy.c1 = cbind(moy.c1, colSums(as.matrix(C1[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0), ]))/ssize)
            moy.L = c(moy.L, mean(LAMBDA[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)]))
            moy.T = c(moy.T, mean(THETA[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)]))
            moy.b = c(moy.b, mean(beta[round((1 + ssize * (i - 
                1)), 0):round((ssize * i), 0)]))
            moy.sig.a = c(moy.sig.a, mean(sigma.alpha[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.s.ov = c(moy.s.ov, mean(S_overall[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.c.ov = c(moy.c.ov, mean(C_overall[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.s1_new = c(moy.s1_new, mean(S1_new[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.c1_new = c(moy.c1_new, mean(C1_new[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
            moy.t = cbind(moy.t, colSums(theta[round((1 + ssize * 
                (i - 1)), 0):round((ssize * i), 0), ])/ssize)
            moy.sig.t = c(moy.sig.t, mean(sigma.theta[round((1 + 
                ssize * (i - 1)), 0):round((ssize * i), 0)]))
        }
    }
    if (N == 1) {
        alpha.MCerror = sqrt(sum((moy.a - mean(alpha)/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        PI.MCerror = sqrt(sum((moy.p - mean(PI)/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        S1.MCerror = sqrt(sum((moy.s1 - mean(as.matrix(S1))/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        C1.MCerror = sqrt(sum((moy.c1 - mean(as.matrix(C1))/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        theta.MCerror = sqrt(sum((moy.t - mean(theta)/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        sigma.theta.MCerror = sqrt(sum((moy.sig.t - mean(sigma.theta))^2)/(batch - 
            1))/sqrt(batch)
    }
    else {
        alpha.MCerror = sqrt(rowSums((moy.a - colSums(alpha)/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        PI.MCerror = sqrt(rowSums((moy.p - colSums(PI)/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        S1.MCerror = sqrt(rowSums((moy.s1 - colSums(as.matrix(S1))/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        C1.MCerror = sqrt(rowSums((moy.c1 - colSums(as.matrix(C1))/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        theta.MCerror = sqrt(rowSums((moy.t - colSums(theta)/iter.size)^2)/(batch - 
            1))/sqrt(batch)
        sigma.theta.MCerror = sqrt(sum((moy.sig.t - mean(sigma.theta))^2)/(batch - 
            1))/sqrt(batch)
    }
    THETA.MCerror = sqrt(sum((moy.T - mean(THETA))^2)/(batch - 
        1))/sqrt(batch)
    LAMBDA.MCerror = sqrt(sum((moy.L - mean(LAMBDA))^2)/(batch - 
        1))/sqrt(batch)
    beta.MCerror = sqrt(sum((moy.b - mean(beta))^2)/(batch - 
        1))/sqrt(batch)
    sigma.alpha.MCerror = sqrt(sum((moy.sig.a - mean(sigma.alpha))^2)/(batch - 
        1))/sqrt(batch)
    S_overall.MCerror = sqrt(sum((moy.s.ov - mean(S_overall))^2)/(batch - 
        1))/sqrt(batch)
    C_overall.MCerror = sqrt(sum((moy.c.ov - mean(C_overall))^2)/(batch - 
        1))/sqrt(batch)
    S1_new.MCerror = sqrt(sum((moy.s1_new - mean(S1_new))^2)/(batch - 
        1))/sqrt(batch)
    C1_new.MCerror = sqrt(sum((moy.c1_new - mean(C1_new))^2)/(batch - 
        1))/sqrt(batch)
    if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
        if (Gold_se == TRUE & Gold_sp == FALSE) {
            if (sub_rs[[1]] == 1) {
                for (i in 1:batch) {
                  moy.c2 = c(moy.c2, mean(C2[round((1 + ssize * 
                    (i - 1)), 0):round((ssize * i), 0)]))
                }
                C2.MCerror = sqrt(sum((moy.c2 - mean(C2))^2)/(batch - 
                  1))/sqrt(batch)
            }
            else {
                if (sub_rs[[1]] > 1) {
                  for (i in 1:batch) {
                    moy.c2 = cbind(moy.c2, colSums(as.matrix(C2[round((1 + 
                      ssize * (i - 1)), 0):round((ssize * i), 
                      0), ]))/ssize)
                  }
                  C2.MCerror = sqrt(rowSums((moy.c2 - colSums(as.matrix(C2))/iter.size)^2)/(batch - 
                    1))/sqrt(batch)
                }
            }
        }
        else {
            if (Gold_sp == TRUE & Gold_se == FALSE) {
                if (sub_rs[[1]] == 1) {
                  for (i in 1:batch) {
                    moy.s2 = c(moy.s2, mean(S2[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                  }
                  S2.MCerror = sqrt(sum((moy.s2 - mean(S2))^2)/(batch - 
                    1))/sqrt(batch)
                }
                else {
                  if (sub_rs[[1]] > 1) {
                    for (i in 1:batch) {
                      moy.s2 = cbind(moy.s2, colSums(as.matrix(S2[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                    }
                    S2.MCerror = sqrt(rowSums((moy.s2 - colSums(as.matrix(S2))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                  }
                }
            }
            else {
                if (sub_rs[[1]] == 1) {
                  for (i in 1:batch) {
                    moy.s2 = c(moy.s2, mean(S2[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.c2 = c(moy.c2, mean(C2[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                  }
                  S2.MCerror = sqrt(sum((moy.s2 - mean(S2))^2)/(batch - 
                    1))/sqrt(batch)
                  C2.MCerror = sqrt(sum((moy.c2 - mean(C2))^2)/(batch - 
                    1))/sqrt(batch)
                }
                else {
                  if (sub_rs[[1]] > 1) {
                    for (i in 1:batch) {
                      moy.s2 = cbind(moy.s2, colSums(as.matrix(S2[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.c2 = cbind(moy.c2, colSums(as.matrix(C2[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                    }
                    S2.MCerror = sqrt(rowSums((moy.s2 - colSums(as.matrix(S2))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    C2.MCerror = sqrt(rowSums((moy.c2 - colSums(as.matrix(C2))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                  }
                }
            }
        }
    }
    else {
        if (condInd == TRUE & Gold_Std == FALSE & model == 2) {
            if (sub_rs[[1]] == 1) {
                for (i in 1:batch) {
                  moy.a1 = c(moy.a1, mean(a1[round((1 + ssize * 
                    (i - 1)), 0):round((ssize * i), 0)]))
                  moy.a0 = c(moy.a0, mean(a0[round((1 + ssize * 
                    (i - 1)), 0):round((ssize * i), 0)]))
                  moy.s2 = c(moy.s2, mean(S2[round((1 + ssize * 
                    (i - 1)), 0):round((ssize * i), 0)]))
                  moy.c2 = c(moy.c2, mean(C2[round((1 + ssize * 
                    (i - 1)), 0):round((ssize * i), 0)]))
                }
                a1.MCerror = sqrt(sum((moy.a1 - mean(a1))^2)/(batch - 
                  1))/sqrt(batch)
                a0.MCerror = sqrt(sum((moy.a0 - mean(a0))^2)/(batch - 
                  1))/sqrt(batch)
                S2.MCerror = sqrt(sum((moy.s2 - mean(S2))^2)/(batch - 
                  1))/sqrt(batch)
                C2.MCerror = sqrt(sum((moy.c2 - mean(C2))^2)/(batch - 
                  1))/sqrt(batch)
            }
            else {
                if (sub_rs[[1]] > 1) {
                  for (i in 1:batch) {
                    moy.a1 = cbind(moy.a1, colSums(as.matrix(a1[round((1 + 
                      ssize * (i - 1)), 0):round((ssize * i), 
                      0), ]))/ssize)
                    moy.a0 = cbind(moy.a0, colSums(as.matrix(a0[round((1 + 
                      ssize * (i - 1)), 0):round((ssize * i), 
                      0), ]))/ssize)
                    moy.s2 = cbind(moy.s2, colSums(as.matrix(S2[round((1 + 
                      ssize * (i - 1)), 0):round((ssize * i), 
                      0), ]))/ssize)
                    moy.c2 = cbind(moy.c2, colSums(as.matrix(C2[round((1 + 
                      ssize * (i - 1)), 0):round((ssize * i), 
                      0), ]))/ssize)
                  }
                  a1.MCerror = sqrt(rowSums((moy.a1 - colSums(as.matrix(a1))/iter.size)^2)/(batch - 
                    1))/sqrt(batch)
                  a0.MCerror = sqrt(rowSums((moy.a0 - colSums(as.matrix(a0))/iter.size)^2)/(batch - 
                    1))/sqrt(batch)
                  S2.MCerror = sqrt(rowSums((moy.s2 - colSums(as.matrix(S2))/iter.size)^2)/(batch - 
                    1))/sqrt(batch)
                  C2.MCerror = sqrt(rowSums((moy.c2 - colSums(as.matrix(C2))/iter.size)^2)/(batch - 
                    1))/sqrt(batch)
                }
            }
        }
        else {
            if (condInd == FALSE) {
                if (sub_rs[[1]] == 1) {
                  for (i in 1:batch) {
                    moy.a1 = c(moy.a1, mean(a1[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.a0 = c(moy.a0, mean(a0[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.b1 = c(moy.b1, mean(b1[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.b0 = c(moy.b0, mean(b0[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.d1 = c(moy.d1, mean(d1[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.d0 = c(moy.d0, mean(d0[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.s2 = c(moy.s2, mean(S2[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                    moy.c2 = c(moy.c2, mean(C2[round((1 + ssize * 
                      (i - 1)), 0):round((ssize * i), 0)]))
                  }
                  a1.MCerror = sqrt(sum((moy.a1 - mean(a1))^2)/(batch - 
                    1))/sqrt(batch)
                  a0.MCerror = sqrt(sum((moy.a0 - mean(a0))^2)/(batch - 
                    1))/sqrt(batch)
                  b1.MCerror = sqrt(sum((moy.b1 - mean(b1))^2)/(batch - 
                    1))/sqrt(batch)
                  b0.MCerror = sqrt(sum((moy.b0 - mean(b0))^2)/(batch - 
                    1))/sqrt(batch)
                  d1.MCerror = sqrt(sum((moy.d1 - mean(d1))^2)/(batch - 
                    1))/sqrt(batch)
                  d0.MCerror = sqrt(sum((moy.d0 - mean(d0))^2)/(batch - 
                    1))/sqrt(batch)
                  S2.MCerror = sqrt(sum((moy.s2 - mean(S2))^2)/(batch - 
                    1))/sqrt(batch)
                  C2.MCerror = sqrt(sum((moy.c2 - mean(C2))^2)/(batch - 
                    1))/sqrt(batch)
                }
                else {
                  if (sub_rs[[1]] > 1) {
                    for (i in 1:batch) {
                      moy.a1 = cbind(moy.a1, colSums(as.matrix(a1[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.a0 = cbind(moy.a0, colSums(as.matrix(a0[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.b1 = cbind(moy.b1, colSums(as.matrix(b1[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.b0 = cbind(moy.b0, colSums(as.matrix(b0[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.d1 = cbind(moy.d1, colSums(as.matrix(d1[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.d0 = cbind(moy.d0, colSums(as.matrix(d0[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.s2 = cbind(moy.s2, colSums(as.matrix(S2[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                      moy.c2 = cbind(moy.c2, colSums(as.matrix(C2[round((1 + 
                        ssize * (i - 1)), 0):round((ssize * i), 
                        0), ]))/ssize)
                    }
                    a1.MCerror = sqrt(rowSums((moy.a1 - colSums(as.matrix(a1))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    a0.MCerror = sqrt(rowSums((moy.a0 - colSums(as.matrix(a0))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    b1.MCerror = sqrt(rowSums((moy.b1 - colSums(as.matrix(b1))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    b0.MCerror = sqrt(rowSums((moy.b0 - colSums(as.matrix(b0))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    d1.MCerror = sqrt(rowSums((moy.d1 - colSums(as.matrix(d1))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    d0.MCerror = sqrt(rowSums((moy.d0 - colSums(as.matrix(d0))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    S2.MCerror = sqrt(rowSums((moy.s2 - colSums(as.matrix(S2))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                    C2.MCerror = sqrt(rowSums((moy.c2 - colSums(as.matrix(C2))/iter.size)^2)/(batch - 
                      1))/sqrt(batch)
                  }
                }
            }
        }
    }
    data = list(data)
    if (real_life == FALSE) {
        pp = data[[1]][, 1]
        pn = data[[1]][, 2]
        np = data[[1]][, 3]
        nn = data[[1]][, 4]
        Sample.size = pp + pn + np + nn
        true.alpha = tv[[1]][, 1]
        true.theta = tv[[1]][, 2]
        true.S1 = tv[[1]][, 3]
        true.C1 = tv[[1]][, 4]
        true.PI = tv[[1]][, 5]
        true.THETA = tv[[2]][1]
        true.sigma.theta = tv[[2]][2]
        true.LAMBDA = tv[[2]][3]
        true.sigma.alpha = tv[[2]][4]
        true.beta = tv[[2]][5]
        if (Gold_Std == TRUE) {
            true.S_overall = tv[[2]][6]
            true.C_overall = tv[[2]][7]
        }
        else {
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                1) {
                true.S2 = tv[[3]][1, ]
                true.C2 = tv[[3]][2, ]
                true.S_overall = tv[[2]][6]
                true.C_overall = tv[[2]][7]
            }
            else {
                if (condInd == TRUE & Gold_Std == FALSE & model == 
                  2) {
                  true.a1 = tv[[3]][3, ]
                  true.a0 = tv[[3]][4, ]
                  true.S2 = tv[[3]][1, ]
                  true.C2 = tv[[3]][2, ]
                  true.S_overall = tv[[2]][8]
                  true.C_overall = tv[[2]][9]
                }
                else {
                  if (condInd == FALSE) {
                    true.S2 = tv[[3]][1, ]
                    true.C2 = tv[[3]][2, ]
                    true.a1 = tv[[3]][3, ]
                    true.a0 = tv[[3]][4, ]
                    true.b1 = tv[[3]][5, ]
                    true.b0 = tv[[3]][6, ]
                    true.d1 = tv[[2]][6]
                    true.d0 = tv[[2]][7]
                    true.S_overall = tv[[2]][8]
                    true.C_overall = tv[[2]][9]
                  }
                }
            }
        }
        test.file = paste("Summary for N =", round((iter.num * 
            nb_chains - (burn_in) * nb_chains)/Thin, 0), ".txt")
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("Number of chains =", nb_chains), file = test.file, 
            append = TRUE)
        write(paste("Number of iteration within a chain =", iter.num, 
            "    Burn in within each chain =", burn_in), file = test.file, 
            append = TRUE)
        write(paste("Thinning interval =", Thin), file = test.file, 
            append = TRUE)
        write(paste("Total number of iteration kept =", round((iter.num * 
            nb_chains - (burn_in) * nb_chains)/Thin, 0)), file = test.file, 
            append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("File location : ", path), file = test.file, 
            append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("Date :", Sys.time()), file = test.file, 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        if (Gold_Std == TRUE) {
            write("Perfect reference standard", file = test.file, 
                append = TRUE)
        }
        else {
            write("Imperfect reference standard", file = test.file, 
                append = TRUE)
        }
        write(paste(""), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tSAMPLE SIZE \t "), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         Total ++ +- -+ --"), file = test.file, 
            append = TRUE)
        for (i in 1:N) {
            write(paste("Study ", i, "", Sample.size[i], "", 
                pp[i], "", pn[i], "", np[i], "", nn[i]), file = test.file, 
                append = TRUE)
        }
        write(paste(""), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tPRIOR INFORMATION \t "), file = test.file, 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("Prior of prevalence (pi) is ", prior_dist_PI, 
            "(", round(alpha.PI, digits = 4), ",", round(beta.PI, 
                digits = 4), "), <=> pi in [", low.pi, ",", up.pi, 
            "]"), file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("Prior of beta is Uniform(", round(beta.a, 
            4), ",", round(beta.b, 4), ")"), file = test.file, 
            append = TRUE)
        write(paste("Prior of THETA is Uniform(", prior.THETA.lower, 
            ",", prior.THETA.upper, ")"), file = test.file, append = TRUE)
        write(paste("Prior of LAMBDA is Uniform(", prior.LAMBDA.lower, 
            ",", prior.LAMBDA.upper, ")"), file = test.file, 
            append = TRUE)
        if (prior_sig_a == 1) {
            write(paste("Prior of sigma_alpha is uniform(", l.disp.alpha, 
                ",", u.disp.alpha, ")"), file = test.file, append = TRUE)
        }
        else {
            if (prior_sig_a == 2) {
                write(paste("Prior of sigma_alpha^2 is uniform(", 
                  l.disp.alpha, ",", u.disp.alpha, ")"), file = test.file, 
                  append = TRUE)
            }
            else {
                if (prior_sig_a == 3) {
                  write(paste("Prior of precision of sigma_alpha is gamma(", 
                    l.disp.alpha, ",", u.disp.alpha, ")"), file = test.file, 
                    append = TRUE)
                }
            }
        }
        if (prior_sig_t == 1) {
            write(paste("Prior of sigma_theta is uniform(", l.disp.theta, 
                ",", u.disp.theta, ")"), file = test.file, append = TRUE)
        }
        else {
            if (prior_sig_t == 2) {
                write(paste("Prior of sigma_theta^2 is uniform(", 
                  l.disp.theta, ",", u.disp.theta, ")"), file = test.file, 
                  append = TRUE)
            }
            else {
                if (prior_sig_t == 3) {
                  write(paste("Prior of precision of sigma_theta is gamma(", 
                    l.disp.theta, ",", u.disp.theta, ")"), file = test.file, 
                    append = TRUE)
                }
            }
        }
        if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
            write(paste(""), file = test.file, append = TRUE)
            if (Gold_se == TRUE & Gold_sp == FALSE) {
                write(paste("Prior of S2 (Sensitivity of reference test) is "), 
                  file = test.file, append = TRUE)
                for (i in 1:rs.length) {
                  write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                    "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "assumed to be perfect."), file = test.file, 
                    append = TRUE)
                }
                write(paste(""), file = test.file, append = TRUE)
                write(paste("Prior of C2 (Specificity of reference test) is "), 
                  file = test.file, append = TRUE)
                for (i in 1:rs.length) {
                  write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                    "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", prior_dist_C2, "(", round(Spec2.alpha[i], 
                      digits = 4), ",", round(Spec2.beta[i], 
                      digits = 4), "), <=> C2 in [", low.sp[i], 
                    ",", up.sp[i], "]"), file = test.file, append = TRUE)
                }
            }
            else {
                if (Gold_sp == TRUE & Gold_se == FALSE) {
                  write(paste("Prior of S2 (Sensitivity of reference test) is "), 
                    file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_S2, "(", round(Sens2.alpha[i], 
                        digits = 4), ",", round(Sens2.beta[i], 
                        digits = 4), "), <=> S2 in [", low.se[i], 
                      ",", up.se[i], "]"), file = test.file, 
                      append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of C2 (Specificity of reference test) is "), 
                    file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "assumed to be perfect."), file = test.file, 
                      append = TRUE)
                  }
                }
                else {
                  write(paste("Prior of S2 (Sensitivity of reference test) is "), 
                    file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_S2, "(", round(Sens2.alpha[i], 
                        digits = 4), ",", round(Sens2.beta[i], 
                        digits = 4), "), <=> S2 in [", low.se[i], 
                      ",", up.se[i], "]"), file = test.file, 
                      append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of C2 (Specificity of reference test) is "), 
                    file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_C2, "(", round(Spec2.alpha[i], 
                        digits = 4), ",", round(Spec2.beta[i], 
                        digits = 4), "), <=> C2 in [", low.sp[i], 
                      ",", up.sp[i], "]"), file = test.file, 
                      append = TRUE)
                  }
                }
            }
        }
        else {
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                2) {
                write(paste(""), file = test.file, append = TRUE)
                write(paste("Prior of a1 is "), file = test.file, 
                  append = TRUE)
                for (i in 1:rs.length) {
                  write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                    "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "Normal (", round(mean.a1[i], digits = 4), 
                    ",", round(sd.a1[i], digits = 4), "), <=> S2 in []"), 
                    file = test.file, append = TRUE)
                }
                write(paste(""), file = test.file, append = TRUE)
                write(paste("Prior of a0 is "), file = test.file, 
                  append = TRUE)
                for (i in 1:rs.length) {
                  write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                    "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "Normal (", round(mean.a0[i], digits = 4), 
                    ",", round(sd.a0[i], digits = 4), "), <=> C2 in []"), 
                    file = test.file, append = TRUE)
                }
            }
            else {
                if (condInd == FALSE) {
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of a1 is "), file = test.file, 
                    append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Normal (", round(mean.a1[i], 
                        digits = 4), ",", round(sd.a1[i], digits = 4), 
                      "), <=> S2 in []"), file = test.file, append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of a0 is "), file = test.file, 
                    append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Normal (", round(mean.a0[i], 
                        digits = 4), ",", round(sd.a0[i], digits = 4), 
                      "), <=> C2 in []"), file = test.file, append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of b1 is "), file = test.file, 
                    append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Uniform (", round(low.b1[i], 
                        digits = 4), ",", round(up.b1[i], digits = 4), 
                      "), <=> S2 in []"), file = test.file, append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of b0 is "), file = test.file, 
                    append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Uniform (", round(low.b0[i], 
                        digits = 4), ",", round(up.b0[i], digits = 4), 
                      "), <=> C2 in []"), file = test.file, append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of d1 is Uniform(", round(low.d1, 
                    4), ",", round(up.d1, 4), ")"), file = test.file, 
                    append = TRUE)
                  write(paste("Prior of d0 is Uniform(", round(low.d0, 
                    4), ",", round(up.d0, 4), ")"), file = test.file, 
                    append = TRUE)
                }
            }
        }
        write(paste(), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tBETWEEN-STUDY parameters (Point estimate =", 
            point_estimate, ")\t "), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("THETA       ", round(true.THETA, digits = digit), 
            "", round(THETA.est, digits = digit), "", round(THETA.sd, 
                digits = digit), "", round(THETA.MCerror, digits = digit), 
            "", round(THETA.HPD[1], digits = digit), "", round(THETA.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        write(paste("LAMBDA      ", round(true.LAMBDA, digits = digit), 
            "", round(LAMBDA.est, digits = digit), "", round(LAMBDA.sd, 
                digits = digit), "", round(LAMBDA.MCerror, digits = digit), 
            "", round(LAMBDA.HPD[1], digits = digit), "", round(LAMBDA.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        write(paste("beta        ", round(true.beta, digits = digit), 
            "", round(beta.est, digits = digit), "", round(beta.sd, 
                digits = digit), "", round(beta.MCerror, digits = digit), 
            "", round(beta.HPD[1], digits = digit), "", round(beta.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        write(paste("sigma.alpha ", round(true.sigma.alpha, digits = digit), 
            "", round(sigma.alpha.est, digits = digit), "", round(sigma.alpha.sd, 
                digits = digit), "", round(sigma.alpha.MCerror, 
                digits = digit), "", round(sigma.alpha.HPD[1], 
                digits = digit), "", round(sigma.alpha.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        write(paste("sigma.theta ", round(true.sigma.theta, digits = digit), 
            "", round(sigma.theta.est, digits = digit), "", round(sigma.theta.sd, 
                digits = digit), "", round(sigma.theta.MCerror, 
                digits = digit), "", round(sigma.theta.HPD[1], 
                digits = digit), "", round(sigma.theta.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        write(paste("Sensitivity (overall)", round(true.S_overall, digits = digit), 
            "", round(S_overall.est, digits = digit), "", round(S_overall.sd, 
                digits = digit), "", round(S_overall.MCerror, 
                digits = digit), "", round(S_overall.HPD[1], 
                digits = digit), "", round(S_overall.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        write(paste("Specificity (overall)", round(true.C_overall, digits = digit), 
            "", round(C_overall.est, digits = digit), "", round(C_overall.sd, 
                digits = digit), "", round(C_overall.MCerror, 
                digits = digit), "", round(C_overall.HPD[1], 
                digits = digit), "", round(C_overall.HPD[2], 
                digits = digit)), file = test.file, append = TRUE)
        if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
            write(paste(""), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tReference standard (Point estimate =", 
                point_estimate, ")\t "), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            if (Gold_se == TRUE & Gold_sp == FALSE) {
                if (rs.length != 1) {
                  for (i in 1:rs.length) {
                    write(paste("S2 of Study(ies) ", sub_rs[[i + 
                      1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", round(true.S2[i], digits = digit), 
                      "(It was assumed to be perfect)"), file = test.file, 
                      append = TRUE)
                    write(paste("C2 of Study(ies) ", sub_rs[[i + 
                      1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", round(true.C2[i], digits = digit), 
                      "", round(C2.est[i], digits = digit), "", 
                      round(C2.sd[i], digits = digit), "", round(C2.MCerror[i], 
                        digits = digit), "", round(C2.HPD[i, 
                        1], digits = digit), "", round(C2.HPD[i, 
                        2], digits = digit)), file = test.file, 
                      append = TRUE)
                  }
                }
                else {
                  write(paste("S2       ", round(true.S2, digits = digit), 
                    "(It was assumed to be perfect)"), file = test.file, 
                    append = TRUE)
                  write(paste("C2       ", round(true.C2, digits = digit), 
                    "", round(C2.est, digits = digit), "", round(C2.sd, 
                      digits = digit), "", round(C2.MCerror, 
                      digits = digit), "", round(C2.HPD[1], digits = digit), 
                    "", round(C2.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                }
            }
            else {
                if (Gold_sp == TRUE & Gold_se == FALSE) {
                  if (rs.length != 1) {
                    for (i in 1:rs.length) {
                      write(paste("S2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.S2[i], digits = digit), 
                        "", round(S2.est[i], digits = digit), 
                        "", round(S2.sd[i], digits = digit), 
                        "", round(S2.MCerror[i], digits = digit), 
                        "", round(S2.HPD[i, 1], digits = digit), 
                        "", round(S2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("C2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.C2[i], digits = digit), 
                        "(It was assumed to be perfect)"), file = test.file, 
                        append = TRUE)
                    }
                  }
                  else {
                    write(paste("S2       ", round(true.S2, digits = digit), 
                      "", round(S2.est, digits = digit), "", 
                      round(S2.sd, digits = digit), "", round(S2.MCerror, 
                        digits = digit), "", round(S2.HPD[1], 
                        digits = digit), "", round(S2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("C2       ", round(true.C2, digits = digit), 
                      "(It was assumed to be perfect)"), file = test.file, 
                      append = TRUE)
                  }
                }
                else {
                  if (rs.length != 1) {
                    for (i in 1:rs.length) {
                      write(paste("S2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.S2[i], digits = digit), 
                        "", round(S2.est[i], digits = digit), 
                        "", round(S2.sd[i], digits = digit), 
                        "", round(S2.MCerror[i], digits = digit), 
                        "", round(S2.HPD[i, 1], digits = digit), 
                        "", round(S2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("C2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.C2[i], digits = digit), 
                        "", round(C2.est[i], digits = digit), 
                        "", round(C2.sd[i], digits = digit), 
                        "", round(C2.MCerror[i], digits = digit), 
                        "", round(C2.HPD[i, 1], digits = digit), 
                        "", round(C2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                  }
                  else {
                    write(paste("S2       ", round(true.S2, digits = digit), 
                      "", round(S2.est, digits = digit), "", 
                      round(S2.sd, digits = digit), "", round(S2.MCerror, 
                        digits = digit), "", round(S2.HPD[1], 
                        digits = digit), "", round(S2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("C2       ", round(true.C2, digits = digit), 
                      "", round(C2.est, digits = digit), "", 
                      round(C2.sd, digits = digit), "", round(C2.MCerror, 
                        digits = digit), "", round(C2.HPD[1], 
                        digits = digit), "", round(C2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                  }
                }
            }
        }
        else {
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                2) {
                write(paste(""), file = test.file, append = TRUE)
                write(paste("______________________________________________________"), 
                  file = test.file, append = TRUE)
                write(paste("\tReference standard (Point estimate =", 
                  point_estimate, ")\t "), file = test.file, 
                  append = TRUE)
                write(paste("______________________________________________________"), 
                  file = test.file, append = TRUE)
                write(paste(""), file = test.file, append = TRUE)
                write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
                  file = test.file, append = TRUE)
                write(paste(""), file = test.file, append = TRUE)
                if (rs.length != 1) {
                  for (i in 1:rs.length) {
                    write(paste("a1 of Study(ies) ", sub_rs[[i + 
                      1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", round(true.a1[i], digits = digit), 
                      "", round(a1.est[i], digits = digit), "", 
                      round(a1.sd[i], digits = digit), "", round(a1.MCerror[i], 
                        digits = digit), "", round(a1.HPD[i, 
                        1], digits = digit), "", round(a1.HPD[i, 
                        2], digits = digit)), file = test.file, 
                      append = TRUE)
                  }
                  for (i in 1:rs.length) {
                    write(paste("a0 of Study(ies) ", sub_rs[[i + 
                      1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", round(true.a0[i], digits = digit), 
                      "", round(a0.est[i], digits = digit), "", 
                      round(a0.sd[i], digits = digit), "", round(a0.MCerror[i], 
                        digits = digit), "", round(a0.HPD[i, 
                        1], digits = digit), "", round(a0.HPD[i, 
                        2], digits = digit)), file = test.file, 
                      append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("S2 of Study(ies) ", sub_rs[[i + 
                      1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", round(true.S2[i], digits = digit), 
                      "", round(S2.est[i], digits = digit), "", 
                      round(S2.sd[i], digits = digit), "", round(S2.MCerror[i], 
                        digits = digit), "", round(S2.HPD[i, 
                        1], digits = digit), "", round(S2.HPD[i, 
                        2], digits = digit)), file = test.file, 
                      append = TRUE)
                  }
                  for (i in 1:rs.length) {
                    write(paste("C2 of Study(ies) ", sub_rs[[i + 
                      1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                      1]])], "", round(true.C2[i], digits = digit), 
                      "", round(C2.est[i], digits = digit), "", 
                      round(C2.sd[i], digits = digit), "", round(C2.MCerror[i], 
                        digits = digit), "", round(C2.HPD[i, 
                        1], digits = digit), "", round(C2.HPD[i, 
                        2], digits = digit)), file = test.file, 
                      append = TRUE)
                  }
                }
                else {
                  write(paste("a1       ", round(true.a1, digits = digit), 
                    "", round(a1.est, digits = digit), "", round(a1.sd, 
                      digits = digit), "", round(a1.MCerror, 
                      digits = digit), "", round(a1.HPD[1], digits = digit), 
                    "", round(a1.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                  write(paste("a0       ", round(true.a0, digits = digit), 
                    "", round(a0.est, digits = digit), "", round(a0.sd, 
                      digits = digit), "", round(a0.MCerror, 
                      digits = digit), "", round(a0.HPD[1], digits = digit), 
                    "", round(a0.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("S2       ", round(true.S2, digits = digit), 
                    "", round(S2.est, digits = digit), "", round(S2.sd, 
                      digits = digit), "", round(S2.MCerror, 
                      digits = digit), "", round(S2.HPD[1], digits = digit), 
                    "", round(S2.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                  write(paste("C2       ", round(true.C2, digits = digit), 
                    "", round(C2.est, digits = digit), "", round(C2.sd, 
                      digits = digit), "", round(C2.MCerror, 
                      digits = digit), "", round(C2.HPD[1], digits = digit), 
                    "", round(C2.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                }
            }
            else {
                if (condInd == FALSE) {
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = test.file, append = TRUE)
                  write(paste("\tReference standard (Point estimate =", 
                    point_estimate, ")\t "), file = test.file, 
                    append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = test.file, append = TRUE)
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
                    file = test.file, append = TRUE)
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("d1       ", round(true.d1, digits = digit), 
                    "", round(d1.est, digits = digit), "", round(d1.sd, 
                      digits = digit), "", round(d1.MCerror, 
                      digits = digit), "", round(d1.HPD[1], digits = digit), 
                    "", round(d1.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                  write(paste("d0       ", round(true.d0, digits = digit), 
                    "", round(d0.est, digits = digit), "", round(d0.sd, 
                      digits = digit), "", round(d0.MCerror, 
                      digits = digit), "", round(d0.HPD[1], digits = digit), 
                    "", round(d0.HPD[2], digits = digit)), file = test.file, 
                    append = TRUE)
                  if (rs.length != 1) {
                    for (i in 1:rs.length) {
                      write(paste("a1 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.a1[i], digits = digit), 
                        "", round(a1.est[i], digits = digit), 
                        "", round(a1.sd[i], digits = digit), 
                        "", round(a1.MCerror[i], digits = digit), 
                        "", round(a1.HPD[i, 1], digits = digit), 
                        "", round(a1.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("a0 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.a0[i], digits = digit), 
                        "", round(a0.est[i], digits = digit), 
                        "", round(a0.sd[i], digits = digit), 
                        "", round(a0.MCerror[i], digits = digit), 
                        "", round(a0.HPD[i, 1], digits = digit), 
                        "", round(a0.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("S2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.S2[i], digits = digit), 
                        "", round(S2.est[i], digits = digit), 
                        "", round(S2.sd[i], digits = digit), 
                        "", round(S2.MCerror[i], digits = digit), 
                        "", round(S2.HPD[i, 1], digits = digit), 
                        "", round(S2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("C2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.C2[i], digits = digit), 
                        "", round(C2.est[i], digits = digit), 
                        "", round(C2.sd[i], digits = digit), 
                        "", round(C2.MCerror[i], digits = digit), 
                        "", round(C2.HPD[i, 1], digits = digit), 
                        "", round(C2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("b1 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.b1[i], digits = digit), 
                        "", round(b1.est[i], digits = digit), 
                        "", round(b1.sd[i], digits = digit), 
                        "", round(b1.MCerror[i], digits = digit), 
                        "", round(b1.HPD[i, 1], digits = digit), 
                        "", round(b1.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("b0 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(true.b0[i], digits = digit), 
                        "", round(b0.est[i], digits = digit), 
                        "", round(b0.sd[i], digits = digit), 
                        "", round(b0.MCerror[i], digits = digit), 
                        "", round(b0.HPD[i, 1], digits = digit), 
                        "", round(b0.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                  }
                  else {
                    write(paste("a1       ", round(true.a1, digits = digit), 
                      "", round(a1.est, digits = digit), "", 
                      round(a1.sd, digits = digit), "", round(a1.MCerror, 
                        digits = digit), "", round(a1.HPD[1], 
                        digits = digit), "", round(a1.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("a0       ", round(true.a0, digits = digit), 
                      "", round(a0.est, digits = digit), "", 
                      round(a0.sd, digits = digit), "", round(a0.MCerror, 
                        digits = digit), "", round(a0.HPD[1], 
                        digits = digit), "", round(a0.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("b1       ", round(true.b1, digits = digit), 
                      "", round(b1.est, digits = digit), "", 
                      round(b1.sd, digits = digit), "", round(b1.MCerror, 
                        digits = digit), "", round(b1.HPD[1], 
                        digits = digit), "", round(b1.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("b0       ", round(true.b0, digits = digit), 
                      "", round(b0.est, digits = digit), "", 
                      round(b0.sd, digits = digit), "", round(b0.MCerror, 
                        digits = digit), "", round(b0.HPD[1], 
                        digits = digit), "", round(b0.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("S2       ", round(true.S2, digits = digit), 
                      "", round(S2.est, digits = digit), "", 
                      round(S2.sd, digits = digit), "", round(S2.MCerror, 
                        digits = digit), "", round(S2.HPD[1], 
                        digits = digit), "", round(S2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("C2       ", round(true.C2, digits = digit), 
                      "", round(C2.est, digits = digit), "", 
                      round(C2.sd, digits = digit), "", round(C2.MCerror, 
                        digits = digit), "", round(C2.HPD[1], 
                        digits = digit), "", round(C2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                  }
                }
            }
        }
        write(paste(""), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tWITHIN-STUDY PARAMETERS \t "), file = test.file, 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\ttheta \t "), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        for (i in 1:N) {
            write(paste("Study ", i, "", round(true.theta[i], 
                digits = digit), "", round(theta.est[i], digits = digit), 
                "", round(theta.sd[i], digits = digit), "", round(theta.MCerror[i], 
                  digits = digit), "", round(theta.HPD[i, 1], 
                  digits = digit), "", round(theta.HPD[i, 2], 
                  digits = digit)), file = test.file, append = TRUE)
        }
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\talpha \t "), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        for (i in 1:N) {
            write(paste("Study ", i, "", round(true.alpha[i], 
                digits = digit), "", round(alpha.est[i], digits = digit), 
                "", round(alpha.sd[i], digits = digit), "", round(alpha.MCerror[i], 
                  digits = digit), "", round(alpha.HPD[i, 1], 
                  digits = digit), "", round(alpha.HPD[i, 2], 
                  digits = digit)), file = test.file, append = TRUE)
        }
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tPrevalence  \t "), file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        for (i in 1:N) {
            write(paste("Study ", i, "", round(true.PI[i], digits = digit), 
                "", round(PI.est[i], digits = digit), "", round(PI.sd[i], 
                  digits = digit), "", round(PI.MCerror[i], digits = digit), 
                "", round(PI.HPD[i, 1], digits = digit), "", 
                round(PI.HPD[i, 2], digits = digit)), file = test.file, 
                append = TRUE)
        }
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tSensitivity of test 1 (S1) \t "), file = test.file, 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        for (i in 1:N) {
            write(paste("Study ", i, "", round(true.S1[i], digits = digit), 
                "", round(S1.est[i], digits = digit), "", round(S1.sd[i], 
                  digits = digit), "", round(S1.MCerror[i], digits = digit), 
                "", round(S1.HPD[i, 1], digits = digit), "", 
                round(S1.HPD[i, 2], digits = digit)), file = test.file, 
                append = TRUE)
        }
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tSpecificity of test 1 (C1) \t "), file = test.file, 
            append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        for (i in 1:N) {
            write(paste("Study ", i, "", round(true.C1[i], digits = digit), 
                "", round(C1.est[i], digits = digit), "", round(C1.sd[i], 
                  digits = digit), "", round(C1.MCerror[i], digits = digit), 
                "", round(C1.HPD[i, 1], digits = digit), "", 
                round(C1.HPD[i, 2], digits = digit)), file = test.file, 
                append = TRUE)
        }
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tPosterior predictive value of Sensitivity of test under evaluation (S1) \t "), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("Sensitivity     ------ ", round(S1_new.est, 
            digits = digit), "", round(S1_new.sd, digits = digit), 
            "", round(S1_new.MCerror, digits = digit), "", round(S1_new.HPD[1], 
                digits = digit), "", round(S1_new.HPD[2], digits = digit)), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste("\tPosterior predictive value of Specificity of test under evaluation (C1) \t "), 
            file = test.file, append = TRUE)
        write(paste("______________________________________________________"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("         True_value Estimate Standard_dev MC_error C.I._lower C.I._upper"), 
            file = test.file, append = TRUE)
        write(paste(""), file = test.file, append = TRUE)
        write(paste("Specificity     ------ ", round(C1_new.est, 
            digits = digit), "", round(C1_new.sd, digits = digit), 
            "", round(C1_new.MCerror, digits = digit), "", round(C1_new.HPD[1], 
                digits = digit), "", round(C1_new.HPD[2], digits = digit)), 
            file = test.file, append = TRUE)
        Num_study = c()
        for (i in 1:N) {
            Num_study = c(Num_study, paste("Study", i))
        }


        if (condInd == TRUE & Gold_Std == FALSE & model == 1) {
            if (Gold_se == TRUE & Gold_sp == FALSE) {
                if (rs.length != 1) {
                  refstd_parameters = array(0, dim = c(rs.length, 
                    4, 1), dimnames = list(1:rs.length, c("True Value", 
                    paste(point_estimate, "estimate"), "HPD lower", 
                    "HPD upper"), "C2"))
                  refstd_parameters[, 1, 1] <- true.C2
                  refstd_parameters[, 2, 1] <- C2.est
                  refstd_parameters[, 3, 1] <- C2.HPD[, 1]
                  refstd_parameters[, 4, 1] <- C2.HPD[, 2]
                  long = length(alpha[, 1])
                  refstd_Parameters = array(0, c(long, rs.length, 
                    1))
                  refstd_Parameters[, , 1] <- C2
                  if (print_plot == TRUE) {
                    if (is.null(chain) == FALSE) {
                      file.png_RS = paste("RefStd trace plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS)#, paper = "a4", height = 20)
                      param = "Specificity"
                      par(mfcol = c(5, 2))
                      no_chains = length(chain)
                      iter_chain = round((iter.num * nb_chains - 
                        (burn_in) * nb_chains)/Thin, 0)/no_chains
                      longueur = 1:iter_chain
                      for (i in 1:rs.length) {
                        plot(x = longueur, y = refstd_Parameters[longueur, 
                          i, 1], type = "n", col = 1, ylab = paste(param, 
                          " of reference standard ", i), xlab = "iteration number", 
                          main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                        for (l in 1:length(chain)) {
                          lines(x = longueur, y = refstd_Parameters[, 
                            i, 1], col = l)
                        }
                      }
                    }
                    else {
                      file.png_RS = paste("RefStd trace plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS)#, paper = "a4", height = 20)
                      param = "Specificity"
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      for (i in 1:rs.length) {
                        plot(x = longueur, y = refstd_Parameters[, 
                          i, 1], type = "l", col = "grey", ylab = paste(param, 
                          " of reference standard ", i), xlab = "iteration number", 
                          main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                      }
                    }
                    dev.off()
                    file.png_RS2 = paste("RefStd density plots for N =", 
                      round((iter.num * nb_chains - (burn_in) * 
                        nb_chains)/Thin, 0), ".png")
                    png(file.png_RS2)#, paper = "a4", height = 20)
                    param = "Specificity"
                    par(mfcol = c(5, 2))
                    longueur = 1:long
                    for (i in 1:rs.length) {
                      plot(density(refstd_Parameters[, i, 1]), 
                        lwd = 4, type = "l", col = "grey", main = paste(param, 
                          " of reference standard ", i, " \n Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin))
                    }
                    dev.off()
                  }
                }
                else {
                  refstd_parameters = matrix(0, ncol = 4, nrow = 1)
                  rownames(refstd_parameters) = c("C2")
                  colnames(refstd_parameters) = c("True Value", 
                    paste(point_estimate, "estimate"), "HPD.low", 
                    "HPD.high")
                  refstd_parameters[1, 1] <- true.C2
                  refstd_parameters[1, 2] <- C2.est
                  refstd_parameters[1, 3] <- C2.HPD[1]
                  refstd_parameters[1, 4] <- C2.HPD[2]
                  long = length(THETA)
                  refstd_Parameters = matrix(0, nrow = long, 
                    ncol = 1)
                  colnames(refstd_Parameters) = c("C2")
                  refstd_Parameters[, 1] <- C2
                  if (print_plot == TRUE) {
                    if (is.null(chain) == FALSE) {
                      file.png_RS = paste("RefStd trace plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS)#, paper = "a4", height = 20)
                      param = "Specificity of reference standard"
                      par(mfcol = c(5, 2))
                      no_chains = length(chain)
                      iter_chain = round((iter.num * nb_chains - 
                        (burn_in) * nb_chains)/Thin, 0)/no_chains
                      longueur = 1:iter_chain
                      plot(x = longueur, y = refstd_Parameters[longueur, 
                        1], type = "n", col = 1, ylab = paste(param), 
                        xlab = "iteration number", main = paste("Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin))
                      for (l in 1:length(chain)) {
                        lines(x = longueur, y = refstd_Parameters[longueur + 
                          (iter_chain * (l - 1)), 1], col = l)
                      }
                    }
                    else {
                      file.png_RS = paste("RefStd trace plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS)#, paper = "a4", height = 20)
                      param = "Specificity of reference standard"
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      plot(x = longueur, y = refstd_Parameters[, 
                        1], type = "l", col = "grey", ylab = paste(param), 
                        xlab = "iteration number", main = paste("Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin))
                    }
                    dev.off()
                    file.png_RS2 = paste("RefStd density plots for N =", 
                      round((iter.num * nb_chains - (burn_in) * 
                        nb_chains)/Thin, 0), ".png")
                    png(file.png_RS2)#, paper = "a4", height = 20)
                    param = "Specificity"
                    par(mfcol = c(5, 2))
                    longueur = 1:long
                    plot(density(refstd_Parameters[, 1]), lwd = 4, 
                      type = "l", col = "grey", main = paste(param, 
                        " of reference standard \n Thinning interval = ", 
                        thin.interval, "\n Total samplesize kept = ", 
                        (iter.num * nb_chains - (burn_in) * nb_chains)/Thin))
                    dev.off()
                  }
                }
            }
            else {
                if (Gold_sp == TRUE & Gold_se == FALSE) {
                  if (rs.length != 1) {
                    refstd_parameters = array(0, dim = c(rs.length, 
                      4, 1), dimnames = list(1:rs.length, c("True Value", 
                      paste(point_estimate, "estimate"), "HPD lower", 
                      "HPD upper"), "S2"))
                    refstd_parameters[, 1, 1] <- true.S2
                    refstd_parameters[, 2, 1] <- S2.est
                    refstd_parameters[, 3, 1] <- S2.HPD[, 1]
                    refstd_parameters[, 4, 1] <- S2.HPD[, 2]
                    long = length(alpha[, 1])
                    refstd_Parameters = array(0, c(long, rs.length, 
                      1))
                    refstd_Parameters[, , 1] <- S2
                    if (print_plot == TRUE) {
                      if (is.null(chain) == FALSE) {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = "Sensitivity"
                        par(mfcol = c(5, 2))
                        no_chains = length(chain)
                        iter_chain = round((iter.num * nb_chains - 
                          (burn_in) * nb_chains)/Thin, 0)/no_chains
                        longueur = 1:iter_chain
                        for (i in 1:rs.length) {
                          plot(x = longueur, y = refstd_Parameters[longueur, 
                            i, 1], type = "n", col = 1, ylab = paste(param, 
                            " of reference standard ", i), xlab = "iteration number", 
                            main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                          for (l in 1:length(chain)) {
                            lines(x = longueur, y = refstd_Parameters[longueur + 
                              (iter_chain * (l - 1)), i, 1], 
                              col = l)
                          }
                        }
                      }
                      else {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = "Sensitivity"
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        for (i in 1:rs.length) {
                          plot(x = longueur, y = refstd_Parameters[, 
                            i, 1], type = "l", col = "grey", 
                            ylab = paste(param, " of reference standard ", 
                              i), xlab = "iteration number", 
                            main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = "Sensitivity"
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      for (i in 1:rs.length) {
                        plot(density(refstd_Parameters[, i, 1]), 
                          lwd = 4, type = "l", col = "grey", 
                          main = paste(param, " of reference standard ", 
                            i, " \n Thinning interval = ", thin.interval, 
                            "\n Total samplesize kept = ", (iter.num * 
                              nb_chains - (burn_in) * nb_chains)/Thin))
                      }
                      dev.off()
                    }
                  }
                  else {
                    refstd_parameters = matrix(0, ncol = 4, nrow = 1)
                    rownames(refstd_parameters) = c("S2")
                    colnames(refstd_parameters) = c("True Value", 
                      paste(point_estimate, "estimate"), "HPD.low", 
                      "HPD.high")
                    refstd_parameters[1, 1] <- true.S2
                    refstd_parameters[1, 2] <- S2.est
                    refstd_parameters[1, 3] <- S2.HPD[1]
                    refstd_parameters[1, 4] <- S2.HPD[2]
                    long = length(THETA)
                    refstd_Parameters = matrix(0, nrow = long, 
                      ncol = 1)
                    colnames(refstd_Parameters) = c("S2")
                    refstd_Parameters[, 1] <- S2
                    if (print_plot == TRUE) {
                      if (is.null(chain) == FALSE) {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = "Sensitivity of reference standard"
                        par(mfcol = c(5, 2))
                        no_chains = length(chain)
                        iter_chain = round((iter.num * nb_chains - 
                          (burn_in) * nb_chains)/Thin, 0)/no_chains
                        longueur = 1:iter_chain
                        plot(x = longueur, y = refstd_Parameters[longueur, 
                          1], type = "n", col = 1, ylab = paste(param), 
                          xlab = "iteration number", main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                        for (l in 1:length(chain)) {
                          lines(x = longueur, y = refstd_Parameters[longueur + 
                            (iter_chain * (l - 1)), 1], col = l)
                        }
                      }
                      else {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = "Sensitivity of reference standard"
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        plot(x = longueur, y = refstd_Parameters[, 
                          1], type = "l", col = "grey", ylab = paste(param), 
                          xlab = "iteration number", main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = "Sensitivity of reference standard"
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      plot(density(refstd_Parameters[, 1]), lwd = 4, 
                        type = "l", col = "grey", main = paste(param, 
                          " \n Thinning interval = ", thin.interval, 
                          "\n Total samplesize kept = ", (iter.num * 
                            nb_chains - (burn_in) * nb_chains)/Thin))
                      dev.off()
                    }
                  }
                }
                else {
                  if (rs.length != 1) {
                    refstd_parameters = array(0, dim = c(rs.length, 
                      4, 2), dimnames = list(1:rs.length, c("True Value", 
                      paste(point_estimate, "estimate"), "HPD lower", 
                      "HPD upper"), c("S2", "C2")))
                    refstd_parameters[, 1, 1] <- true.S2
                    refstd_parameters[, 2, 1] <- S2.est
                    refstd_parameters[, 3, 1] <- S2.HPD[, 1]
                    refstd_parameters[, 4, 1] <- S2.HPD[, 2]
                    refstd_parameters[, 1, 2] <- true.C2
                    refstd_parameters[, 2, 2] <- C2.est
                    refstd_parameters[, 3, 2] <- C2.HPD[, 1]
                    refstd_parameters[, 4, 2] <- C2.HPD[, 2]
                    long = length(alpha[, 1])
                    refstd_Parameters = array(0, c(long, rs.length, 
                      2))
                    refstd_Parameters[, , 1] <- S2
                    refstd_Parameters[, , 2] <- C2
                    if (print_plot == TRUE) {
                      if (is.null(chain) == FALSE) {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = c("Sensitivity", "Specificity")
                        par(mfcol = c(5, 2))
                        no_chains = length(chain)
                        iter_chain = round((iter.num * nb_chains - 
                          (burn_in) * nb_chains)/Thin, 0)/no_chains
                        longueur = 1:iter_chain
                        for (j in 1:2) {
                          for (i in 1:rs.length) {
                            plot(x = longueur, y = refstd_Parameters[longueur, 
                              i, j], type = "n", col = 1, ylab = paste(param[j], 
                              " of reference standard ", i), 
                              xlab = "iteration number", main = paste("Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                            for (l in 1:length(chain)) {
                              lines(x = longueur, y = refstd_Parameters[longueur + 
                                (iter_chain * (l - 1)), i, j], 
                                col = l)
                            }
                          }
                        }
                      }
                      else {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = c("Sensitivity", "Specificity")
                        par(mfcol = c(5, 2))
                        for (j in 1:2) {
                          longueur = 1:long
                          for (i in 1:rs.length) {
                            plot(x = longueur, y = refstd_Parameters[, 
                              i, j], type = "l", col = "grey", 
                              ylab = paste(param[j], " of reference standard ", 
                                i), xlab = "iteration number", 
                              main = paste("Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                          }
                        }
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = c("Sensitivity", "Specificity")
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      for (j in 1:2) {
                        for (i in 1:rs.length) {
                          plot(density(refstd_Parameters[, i, 
                            j]), lwd = 4, type = "l", col = "grey", 
                            main = paste(param[j], " of reference standard ", 
                              i, " \n Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                      }
                      dev.off()
                    }
                  }
                  else {
                    refstd_parameters = matrix(0, ncol = 4, nrow = 2)
                    rownames(refstd_parameters) = c("S2", "C2")
                    colnames(refstd_parameters) = c("True Value", 
                      paste(point_estimate, "estimate"), "HPD.low", 
                      "HPD.high")
                    refstd_parameters[1, 1] <- true.S2
                    refstd_parameters[1, 2] <- S2.est
                    refstd_parameters[1, 3] <- S2.HPD[1]
                    refstd_parameters[1, 4] <- S2.HPD[2]
                    refstd_parameters[2, 1] <- true.C2
                    refstd_parameters[2, 2] <- C2.est
                    refstd_parameters[2, 3] <- C2.HPD[1]
                    refstd_parameters[2, 4] <- C2.HPD[2]
                    long = length(THETA)
                    refstd_Parameters = matrix(0, nrow = long, 
                      ncol = 2)
                    colnames(refstd_Parameters) = c("S2", "C2")
                    refstd_Parameters[, 1] <- S2
                    refstd_Parameters[, 2] <- C2
                    if (print_plot == TRUE) {
                      if (is.null(chain) == FALSE) {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        no_chains = length(chain)
                        iter_chain = round((iter.num * nb_chains - 
                          (burn_in) * nb_chains)/Thin, 0)/no_chains
                        longueur = 1:iter_chain
                        param = c("Sensitivity of reference standard", 
                          "Specificity of reference standard")
                        par(mfcol = c(5, 2))
                        for (j in 1:2) {
                          plot(x = longueur, y = refstd_Parameters[longueur, 
                            j], type = "n", col = 1, ylab = paste(param[j]), 
                            xlab = "iteration number", main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                          for (l in 1:length(chain)) {
                            lines(x = longueur, y = refstd_Parameters[longueur + 
                              (iter_chain * (l - 1)), j], col = l)
                          }
                        }
                      }
                      else {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        Param = c("Sensitivity of reference standard", 
                          "Specificity of reference standard")
                        par(mfcol = c(5, 2))
                        for (j in 1:2) {
                          longueur = 1:long
                          plot(x = longueur, y = refstd_Parameters[, 
                            j], type = "l", col = "grey", ylab = paste(Param[j]), 
                            xlab = "iteration number", main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = c("Sensitivity of reference standard", 
                        "Specificity of reference standard")
                      par(mfcol = c(5, 2))
                      for (j in 1:2) {
                        longueur = 1:long
                        plot(density(refstd_Parameters[, j]), 
                          lwd = 4, type = "l", col = "grey", 
                          main = paste(param[j], " \n Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                      }
                      dev.off()
                    }
                  }
                }
            }
        }
        else {
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                2) {
                if (rs.length != 1) {
                  refstd_parameters = array(0, dim = c(rs.length, 
                    4, 4), dimnames = list(1:rs.length, c("True Value", 
                    paste(point_estimate, "estimate"), "HPD lower", 
                    "HPD upper"), c("S2", "C2", "a1", "a0")))
                  refstd_parameters[, 1, 1] <- true.S2
                  refstd_parameters[, 2, 1] <- S2.est
                  refstd_parameters[, 3, 1] <- S2.HPD[, 1]
                  refstd_parameters[, 4, 1] <- S2.HPD[, 2]
                  refstd_parameters[, 1, 2] <- true.C2
                  refstd_parameters[, 2, 2] <- C2.est
                  refstd_parameters[, 3, 2] <- C2.HPD[, 1]
                  refstd_parameters[, 4, 2] <- C2.HPD[, 2]
                  refstd_parameters[, 1, 3] <- true.a1
                  refstd_parameters[, 2, 3] <- a1.est
                  refstd_parameters[, 3, 3] <- a1.HPD[, 1]
                  refstd_parameters[, 4, 3] <- a1.HPD[, 2]
                  refstd_parameters[, 1, 4] <- true.a0
                  refstd_parameters[, 2, 4] <- a0.est
                  refstd_parameters[, 3, 4] <- a0.HPD[, 1]
                  refstd_parameters[, 4, 4] <- a0.HPD[, 2]
                  long = length(alpha[, 1])
                  refstd_Parameters = array(0, c(long, rs.length, 
                    4))
                  refstd_Parameters[, , 1] <- S2
                  refstd_Parameters[, , 2] <- C2
                  refstd_Parameters[, , 3] <- a1
                  refstd_Parameters[, , 4] <- a0
                  if (print_plot == TRUE) {
                    file.png_RS = paste("RefStd trace plots for N =", 
                      round((iter.num * nb_chains - (burn_in) * 
                        nb_chains)/Thin, 0), ".png")
                    png(file.png_RS)#, paper = "a4", height = 20)
                    param = c("Sensitivity", "Specificity", "a1", 
                      "a0")
                    par(mfcol = c(5, 2))
                    for (j in 1:4) {
                      longueur = 1:long
                      for (i in 1:rs.length) {
                        plot(x = longueur, y = refstd_Parameters[, 
                          i, j], type = "l", col = "grey", ylab = paste(param[j], 
                          " of reference standard ", i), xlab = "iteration number", 
                          main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                        abline(a = refstd_parameters[i, 2, j], 
                          b = 0, col = "black", lwd = 3)
                        abline(a = refstd_parameters[i, 1, j], 
                          b = 0, col = "red", lwd = 3)
                        abline(a = refstd_parameters[i, 3, j], 
                          b = 0, col = "green", lwd = 3)
                        abline(a = refstd_parameters[i, 4, j], 
                          b = 0, col = "green", lwd = 3)
                      }
                    }
                    dev.off()
                    file.png_RS2 = paste("RefStd density plots for N =", 
                      round((iter.num * nb_chains - (burn_in) * 
                        nb_chains)/Thin, 0), ".png")
                    png(file.png_RS2)#, paper = "a4", height = 20)
                    param = c("Sensitivity", "Specificity", "a1", 
                      "a0")
                    par(mfcol = c(5, 2))
                    longueur = 1:long
                    for (j in 1:4) {
                      for (i in 1:rs.length) {
                        plot(density(refstd_Parameters[, i, j]), 
                          lwd = 4, type = "l", col = "grey", 
                          main = paste(param[j], " of reference standard ", 
                            i, " \n Thinning interval = ", thin.interval, 
                            "\n Total samplesize kept = ", (iter.num * 
                              nb_chains - (burn_in) * nb_chains)/Thin))
                      }
                    }
                    dev.off()
                  }
                }
                else {
                  refstd_parameters = matrix(0, ncol = 4, nrow = 4)
                  rownames(refstd_parameters) = c("S2", "C2", 
                    "a1", "a0")
                  colnames(refstd_parameters) = c("True Value", 
                    paste(point_estimate, "estimate"), "HPD.low", 
                    "HPD.high")
                  refstd_parameters[1, 1] <- true.S2
                  refstd_parameters[1, 2] <- S2.est
                  refstd_parameters[1, 3] <- S2.HPD[1]
                  refstd_parameters[1, 4] <- S2.HPD[2]
                  refstd_parameters[2, 1] <- true.C2
                  refstd_parameters[2, 2] <- C2.est
                  refstd_parameters[2, 3] <- C2.HPD[1]
                  refstd_parameters[2, 4] <- C2.HPD[2]
                  refstd_parameters[3, 1] <- true.a1
                  refstd_parameters[3, 2] <- a1.est
                  refstd_parameters[3, 3] <- a1.HPD[1]
                  refstd_parameters[3, 4] <- a1.HPD[2]
                  refstd_parameters[4, 1] <- true.a0
                  refstd_parameters[4, 2] <- a0.est
                  refstd_parameters[4, 3] <- a0.HPD[1]
                  refstd_parameters[4, 4] <- a0.HPD[2]
                  long = length(THETA)
                  refstd_Parameters = matrix(0, nrow = long, 
                    ncol = 4)
                  colnames(refstd_Parameters) = c("S2", "C2", 
                    "a1", "a0")
                  refstd_Parameters[, 1] <- S2
                  refstd_Parameters[, 2] <- C2
                  refstd_Parameters[, 3] <- a1
                  refstd_Parameters[, 4] <- a0
                  if (print_plot == TRUE) {
                    file.png_RS = paste("RefStd trace plots for N =", 
                      round((iter.num * nb_chains - (burn_in) * 
                        nb_chains)/Thin, 0), ".png")
                    png(file.png_RS)#, paper = "a4", height = 20)
                    Param = c("Sensitivity of reference standard", 
                      "Specificity of reference standard", "a1", 
                      "a0")
                    par(mfcol = c(5, 2))
                    for (j in 1:4) {
                      longueur = 1:long
                      plot(x = longueur, y = refstd_Parameters[, 
                        j], type = "l", col = "grey", ylab = paste(Param[j]), 
                        xlab = "iteration number", main = paste("Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin))
                      abline(a = refstd_parameters[j, 2], b = 0, 
                        col = "black", lwd = 3)
                      abline(a = refstd_parameters[j, 1], b = 0, 
                        col = "red", lwd = 3)
                      abline(a = refstd_parameters[j, 3], b = 0, 
                        col = "green", lwd = 3)
                      abline(a = refstd_parameters[j, 4], b = 0, 
                        col = "green", lwd = 3)
                    }
                    dev.off()
                    file.png_RS2 = paste("RefStd density plots for N =", 
                      round((iter.num * nb_chains - (burn_in) * 
                        nb_chains)/Thin, 0), ".png")
                    png(file.png_RS2)#, paper = "a4", height = 20)
                    param = c("Sensitivity of reference standard", 
                      "Specificity of reference standard", "a1", 
                      "a0")
                    par(mfcol = c(5, 2))
                    for (j in 1:4) {
                      longueur = 1:long
                      plot(density(refstd_Parameters[, j]), lwd = 4, 
                        type = "l", col = "grey", main = paste(param[j], 
                          " \n Thinning interval = ", thin.interval, 
                          "\n Total samplesize kept = ", (iter.num * 
                            nb_chains - (burn_in) * nb_chains)/Thin))
                    }
                    dev.off()
                  }
                }
            }
        }
        parameters = array(0, dim = c(N, 4, 5), dimnames = list(Num_study, 
            c("True value", paste(point_estimate, "estimate"), 
                "HPD lower", "HPD upper"), c("theta", "alpha", 
                "pi", "S1", "C1")))
        parameters[, 1, 1] <- true.theta
        parameters[, 2, 1] <- theta.est
        parameters[, 3, 1] <- theta.HPD[, 1]
        parameters[, 4, 1] <- theta.HPD[, 2]
        parameters[, 1, 2] <- true.alpha
        parameters[, 2, 2] <- alpha.est
        parameters[, 3, 2] <- alpha.HPD[, 1]
        parameters[, 4, 2] <- alpha.HPD[, 2]
        parameters[, 1, 3] <- true.PI
        parameters[, 2, 3] <- PI.est
        parameters[, 3, 3] <- PI.HPD[, 1]
        parameters[, 4, 3] <- PI.HPD[, 2]
        parameters[, 1, 4] <- true.S1
        parameters[, 2, 4] <- S1.est
        parameters[, 3, 4] <- S1.HPD[, 1]
        parameters[, 4, 4] <- S1.HPD[, 2]
        parameters[, 1, 5] <- true.C1
        parameters[, 2, 5] <- C1.est
        parameters[, 3, 5] <- C1.HPD[, 1]
        parameters[, 4, 5] <- C1.HPD[, 2]
        long = length(alpha[, 1])
        Parameters = array(0, c(long, N, 5))
        Parameters[, , 1] <- theta
        Parameters[, , 2] <- alpha
        Parameters[, , 3] <- PI
        Parameters[, , 4] <- S1
        Parameters[, , 5] <- C1
        parameter = matrix(0, ncol = 4, nrow = 9)
        rownames(parameter) = c("THETA", "LAMBDA", "beta", "sigma.alpha", 
            "sigma.theta", "S Overall", "C Overall", "S1_new", 
            "C1_new")
        colnames(parameter) = c("True.value", paste(point_estimate, 
            "estimate"), "HPD.low", "HPD.high")
        parameter[1, 1] <- true.THETA
        parameter[1, 2] <- THETA.est
        parameter[1, 3] <- THETA.HPD[1]
        parameter[1, 4] <- THETA.HPD[2]
        parameter[2, 1] <- true.LAMBDA
        parameter[2, 2] <- LAMBDA.est
        parameter[2, 3] <- LAMBDA.HPD[1]
        parameter[2, 4] <- LAMBDA.HPD[2]
        parameter[3, 1] <- true.beta
        parameter[3, 2] <- beta.est
        parameter[3, 3] <- beta.HPD[1]
        parameter[3, 4] <- beta.HPD[2]
        parameter[4, 1] <- true.sigma.alpha
        parameter[4, 2] <- sigma.alpha.est
        parameter[4, 3] <- sigma.alpha.HPD[1]
        parameter[4, 4] <- sigma.alpha.HPD[2]
        parameter[5, 1] <- true.sigma.theta
        parameter[5, 2] <- sigma.theta.est
        parameter[5, 3] <- sigma.theta.HPD[1]
        parameter[5, 4] <- sigma.theta.HPD[2]
        parameter[6, 1] <- true.S_overall
        parameter[6, 2] <- S_overall.est
        parameter[6, 3] <- S_overall.HPD[1]
        parameter[6, 4] <- S_overall.HPD[2]
        parameter[7, 1] <- true.C_overall
        parameter[7, 2] <- C_overall.est
        parameter[7, 3] <- C_overall.HPD[1]
        parameter[7, 4] <- C_overall.HPD[2]
        parameter[8, 1] <- S1_new.est
        parameter[8, 2] <- S1_new.est
        parameter[8, 3] <- S1_new.HPD[1]
        parameter[8, 4] <- S1_new.HPD[2]
        parameter[9, 1] <- C1_new.est
        parameter[9, 2] <- C1_new.est
        parameter[9, 3] <- C1_new.HPD[1]
        parameter[9, 4] <- C1_new.HPD[2]
        long = length(THETA)
        Parameter = matrix(0, nrow = long, ncol = 9)
        colnames(Parameter) = c("THETA", "LAMBDA", "beta", "sigma.alpha", 
            "sigma.theta", "S overall", "C overall", "S1_new", 
            "C1_new")
        Parameter[, 1] <- THETA
        Parameter[, 2] <- LAMBDA
        Parameter[, 3] <- beta
        Parameter[, 4] <- sigma.alpha
        Parameter[, 5] <- sigma.theta
        Parameter[, 6] <- S_overall
        Parameter[, 7] <- C_overall
        Parameter[, 8] <- S1_new
        Parameter[, 9] <- C1_new
        if (print_plot == TRUE) {
            if (is.null(chain) == FALSE) {
                plot.name <- "Trace_plots_for_N="
                file.png5 = paste(plot.name, round((iter.num * 
                  nb_chains - (burn_in) * nb_chains)/Thin, 0), 
                  ".png", sep = "")
                png(file.png5, width=1440, height=1440, pointsize=18)#, paper = "a4", height = 20)
                param = c("theta", "alpha", "PI", "S1", "C1")
                Param = c("Capital Theta", "Capital Lambda", 
                  "beta", "~sigma[alpha]", "~sigma[theta]", "Sensitivity (overall)", 
                  "Specificity (overall)", "Sensitivity (new)", "Specificity (new)")
                no_chains = length(chain)
                iter_chain = round((iter.num * nb_chains - (burn_in) * 
                  nb_chains)/Thin, 0)/no_chains
                min_param = c(min(Parameters[, , 1]), min(Parameters[, 
                  , 2]), min(Parameters[, , 3]), min(Parameters[, 
                  , 4]), min(Parameters[, , 5]))
                max_param = c(max(Parameters[, , 1]), max(Parameters[, 
                  , 2]), max(Parameters[, , 3]), max(Parameters[, 
                  , 4]), max(Parameters[, , 5]))
                dlag = (max_param - min_param)/100
                range_param = numeric()
                for (j in 1:5) {
                  range_param = cbind(range_param, seq(min_param[j] + 
                    dlag[j]/2, max_param[j] - dlag[j]/2, by = dlag[j]))
                }
                par(mfcol = c(5, 2))
                longueur = 1:iter_chain
                for (j in 1:5) {
                  for (i in 1:N) {
                    plot(x = longueur, y = Parameters[longueur, 
                      i, j], type = "n", col = 1, ylab = paste(param[j], 
                      " of study ", i), xlab = "iteration number", 
                      main = paste("Thinning interval = ", thin.interval, 
                        "\n Total samplesize kept = ", (iter.num * 
                          nb_chains - (burn_in) * nb_chains)/Thin), 
                      ylim = range(range_param[, j]))
                    for (l in 1:length(chain)) {
                      lines(x = longueur, y = Parameters[longueur + 
                        (iter_chain * (l - 1)), i, j], col = l)
                    }
                  }
                }
                min_Param = c(min(Parameter[, 1]), min(Parameter[, 
                  2]), min(Parameter[, 3]), min(Parameter[, 4]), 
                  min(Parameter[, 5]), min(Parameter[, 6]), min(Parameter[, 
                    7]), min(Parameter[, 8]), min(Parameter[, 
                    9]))
                max_Param = c(max(Parameter[, 1]), max(Parameter[, 
                  2]), max(Parameter[, 3]), max(Parameter[, 4]), 
                  max(Parameter[, 5]), max(Parameter[, 6]), max(Parameter[, 
                    7]), max(Parameter[, 8]), max(Parameter[, 
                    9]))
                dlag = (max_Param - min_Param)/100
                range_Param = numeric()
                for (j in 1:9) {
                  range_Param = cbind(range_Param, seq(min_Param[j] + 
                    dlag[j]/2, max_Param[j] - dlag[j]/2, by = dlag[j]))
                }
                for (j in 1:9) {
                  plot(x = longueur, y = Parameter[longueur, 
                    j], type = "n", col = 1, ylab = paste(Param[j]), 
                    xlab = "iteration number", main = paste("Thinning interval = ", 
                      thin.interval, "\n Total samplesize kept = ", 
                      (iter.num * nb_chains - (burn_in) * nb_chains)/Thin), 
                    ylim = range(range_Param[, j]))
                  for (l in 1:length(chain)) {
                    lines(x = longueur, y = Parameter[longueur + 
                      (iter_chain * (l - 1)), j], col = l)
                  }
                }
                dev.off()
                image.list$trace_plots <- c(file.png5, "")
            }
            else {
                plot.name <- "Trace_plots_for_N="
                file.png2 = paste(plot.name, round((iter.num * 
                  nb_chains - (burn_in) * nb_chains)/Thin, 0), 
                  ".png", sep="")
                png(file.png2, width=1440, height=1440, pointsize=18)#, paper = "a4", height = 20)
                param = c("theta", "alpha", "PI", "S1", "C1")
                Param = c("Capital Theta", "Capital Lambda", 
                  "beta", "~sigma[alpha]", "~sigma[theta]", "Sensitivity (summary)", 
                  "Specificity (summary)", "Sensitivity (new)", "Specificity (new)")
                min_param = c(min(Parameters[, , 1]), min(Parameters[, 
                  , 2]), min(Parameters[, , 3]), min(Parameters[, 
                  , 4]), min(Parameters[, , 5]))
                max_param = c(max(Parameters[, , 1]), max(Parameters[, 
                  , 2]), max(Parameters[, , 3]), max(Parameters[, 
                  , 4]), max(Parameters[, , 5]))
                dlag = (max_param - min_param)/100
                range_param = numeric()
                for (j in 1:5) {
                  range_param = cbind(range_param, seq(min_param[j] + 
                    dlag[j]/2, max_param[j] - dlag[j]/2, by = dlag[j]))
                }
                par(mfcol = c(5, 2))
                longueur = 1:long
                for (j in 1:5) {
                  for (i in 1:N) {
                    plot(x = longueur, y = Parameters[, i, j], 
                      type = "l", col = "grey", ylab = paste(param[j], 
                        " of study ", i), xlab = "iteration number", 
                      main = paste("Thinning interval = ", thin.interval, 
                        "\n Total samplesize kept = ", (iter.num * 
                          nb_chains - (burn_in) * nb_chains)/Thin), 
                      ylim = range(range_param[, j]))
                    abline(a = parameters[i, 3, j], b = 0, col = "green", 
                      lwd = 3)
                    abline(a = parameters[i, 4, j], b = 0, col = "green", 
                      lwd = 3)
                  }
                }
                min_Param = c(min(Parameter[, 1]), min(Parameter[, 
                  2]), min(Parameter[, 3]), min(Parameter[, 4]), 
                  min(Parameter[, 5]), min(Parameter[, 6]), min(Parameter[, 
                    7]), min(Parameter[, 8]), min(Parameter[, 
                    9]))
                max_Param = c(max(Parameter[, 1]), max(Parameter[, 
                  2]), max(Parameter[, 3]), max(Parameter[, 4]), 
                  max(Parameter[, 5]), max(Parameter[, 6]), max(Parameter[, 
                    7]), max(Parameter[, 8]), max(Parameter[, 
                    9]))
                dlag = (max_Param - min_Param)/100
                range_Param = numeric()
                for (j in 1:9) {
                  range_Param = cbind(range_Param, seq(min_Param[j] + 
                    dlag[j]/2, max_Param[j] - dlag[j]/2, by = dlag[j]))
                }
                for (j in 1:9) {
                  plot(x = longueur, y = Parameter[, j], type = "l", 
                    col = "grey", ylab = paste(Param[j]), xlab = "iteration number", 
                    main = paste("Thinning interval = ", thin.interval, 
                      "\n Total samplesize kept = ", (iter.num * 
                        nb_chains - (burn_in) * nb_chains)/Thin), 
                    ylim = range(range_Param[, j]))
                  abline(a = parameter[j, 3], b = 0, col = "green", 
                    lwd = 3)
                  abline(a = parameter[j, 4], b = 0, col = "green", 
                    lwd = 3)
                }
                dev.off()
                image.list$trace_plots <- c(paste(plot.name, ".png", sep = ""), "")
            }
            dens.plot.name <- "Density_plots_for_N3="
            file.png3 = paste(dens.plot.name, round((iter.num * 
                nb_chains - (burn_in) * nb_chains)/Thin, 0), 
                ".png", sep="")
            png(file.png3, width=1440, height=1440)#, paper = "a4", height = 20)
            param = c("theta", "alpha", "PI", "S1", "C1")
            Param = c("Capital Theta", "Capital Lambda", "beta", 
                "~sigma[alpha]", "~sigma[theta]", "S Overall", 
                "C Overall", "S1_new", "C1_new")
            par(mfcol = c(5, 2))
            longueur = 1:long
            for (j in 1:5) {
                for (i in 1:N) {
                  plot(density(Parameters[, i, j]), lwd = 4, 
                    type = "l", col = "grey", main = paste(param[j], 
                      " of study ", i, " \n Thinning interval = ", 
                      thin.interval, "\n Total samplesize kept = ", 
                      (iter.num * nb_chains - (burn_in) * nb_chains)/Thin))
                }
            }
            for (j in 1:9) {
                plot(density(Parameter[, j]), lwd = 4, type = "l", 
                  col = "grey", main = paste(Param[j], " \n Thinning interval = ", 
                    thin.interval, "\n Total samplesize kept = ", 
                    (iter.num * nb_chains - (burn_in) * nb_chains)/Thin))
            }
            dev.off()
            #pdf("Summary ROC curve.pdf")
            # possibly the worse variable naming of all time.
            png("Summary_ROC_curve.png")
            #make.sroc.plot(Parameters, Parameter, data)
            default.x = range(1, 0)
            default.y = range(0, 1)
            plot(x = default.x, y = default.y, type = "n", xlim = rev(range(default.x)), 
                xlab = "", ylab = "")
            title(xlab = "Specificity", ylab = "Sensitivity", 
                cex.lab = 1.5, main = "Summary ROC curve")
            Sensi1 = apply(as.matrix(Parameters[, , 4]), 2, median)
            Speci1 = apply(as.matrix(Parameters[, , 5]), 2, median)
            Scale_factor = 10
            symbols(Speci1, Sensi1, circles = rowSums(as.matrix(data[[1]])), 
                inches = 0.1 * Scale_factor/7, add = TRUE)
            Ov_Se = 1 - pnorm((median(Parameter[, 1]) - median(Parameter[, 
                2])/2)/exp(median(Parameter[, 3])/2))
            Ov_Sp = pnorm((median(Parameter[, 1]) + median(Parameter[, 
                2])/2)/exp(-median(Parameter[, 3])/2))
            points(Ov_Sp, Ov_Se, pch = 19, cex = 2)
            thet = qnorm((1 - as.matrix(Parameters[, , 4])) + 
                1e-14) * exp(Parameter[, 3]/2) + Parameter[, 
                2]/2
            min_TH = quantile(thet, 0.025)
            max_TH = quantile(thet, 0.975)
            dTH = 5e-05
            TH_range = seq(min_TH + dTH/2, max_TH - dTH/2, dTH)
            S_sroc = 1 - pnorm((TH_range - median(Parameter[, 
                2])/2)/exp(median(Parameter[, 3])/2))
            C_sroc = pnorm((TH_range + median(Parameter[, 2])/2)/exp(-median(Parameter[, 
                3])/2))
            lines(C_sroc, S_sroc, lwd = 3, col = "black", lty = 1)
            dev.off()

 
        }
    }
    else {
        if (real_life == TRUE) {
            d = as.matrix(data[[1]])
            Sample.size = d[, 1] + d[, 2] + d[, 3] + d[, 4]
            pp = d[, 1]
            pn = d[, 2]
            np = d[, 3]
            nn = d[, 4]
            test.file = paste("Summary for N =", round((iter.num * 
                nb_chains - (burn_in) * nb_chains)/Thin, 0), 
                ".txt")
            write(paste("Number of chains =", nb_chains), file = test.file, 
                append = TRUE)
            write(paste("Number of iteration within a chain =", 
                iter.num, "    Burn in within each chain =", 
                burn_in), file = test.file, append = TRUE)
            write(paste("Thinning interval =", Thin), file = test.file, 
                append = TRUE)
            write(paste("Total number of iteration kept =", round((iter.num * 
                nb_chains - (burn_in) * nb_chains)/Thin, 0)), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("File location : ", path), file = test.file, 
                append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("Date :", Sys.time()), file = test.file, 
                append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            if (Gold_Std == TRUE) {
                write("Perfect reference standard", file = test.file, 
                  append = TRUE)
            }
            else {
                write("Imperfect reference standard", file = test.file, 
                  append = TRUE)
            }
            write(paste(""), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tSAMPLE SIZE \t "), file = test.file, 
                append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Total ++ +- -+ --"), file = test.file, 
                append = TRUE)
            for (i in 1:N) {
                write(paste("Study ", i, "", Sample.size[i], 
                  "", pp[i], "", pn[i], "", np[i], "", nn[i]), 
                  file = test.file, append = TRUE)
            }
            write(paste(""), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tPRIOR INFORMATION \t "), file = test.file, 
                append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("Prior of prevalence (pi) is ", prior_dist_PI, 
                "(", round(alpha.PI, digits = 4), ",", round(beta.PI, 
                  digits = 4), "), <=> pi in [", low.pi, ",", 
                up.pi, "]"), file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("Prior of beta is Uniform(", round(beta.a, 
                4), ",", round(beta.b, 4), ")"), file = test.file, 
                append = TRUE)
            write(paste("Prior of THETA is Uniform(", prior.THETA.lower, 
                ",", prior.THETA.upper, ")"), file = test.file, 
                append = TRUE)
            write(paste("Prior of LAMBDA is Uniform(", prior.LAMBDA.lower, 
                ",", prior.LAMBDA.upper, ")"), file = test.file, 
                append = TRUE)
            if (prior_sig_a == 1) {
                write(paste("Prior of sigma_alpha is uniform(", 
                  l.disp.alpha, ",", u.disp.alpha, ")"), file = test.file, 
                  append = TRUE)
            }
            else {
                if (prior_sig_a == 2) {
                  write(paste("Prior of sigma_alpha^2 is uniform(", 
                    l.disp.alpha, ",", u.disp.alpha, ")"), file = test.file, 
                    append = TRUE)
                }
                else {
                  if (prior_sig_a == 3) {
                    write(paste("Prior of precision of sigma_alpha is gamma(", 
                      l.disp.alpha, ",", u.disp.alpha, ")"), 
                      file = test.file, append = TRUE)
                  }
                }
            }
            if (prior_sig_t == 1) {
                write(paste("Prior of sigma_theta is uniform(", 
                  l.disp.theta, ",", u.disp.theta, ")"), file = test.file, 
                  append = TRUE)
            }
            else {
                if (prior_sig_t == 2) {
                  write(paste("Prior of sigma_theta^2 is uniform(", 
                    l.disp.theta, ",", u.disp.theta, ")"), file = test.file, 
                    append = TRUE)
                }
                else {
                  if (prior_sig_t == 3) {
                    write(paste("Prior of precision of sigma_theta is gamma(", 
                      l.disp.theta, ",", u.disp.theta, ")"), 
                      file = test.file, append = TRUE)
                  }
                }
            }
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                1) {
                write(paste(""), file = test.file, append = TRUE)
                if (Gold_se == TRUE & Gold_sp == FALSE) {
                  write(paste("Prior of S2 (Sensitivity of reference test) is "), 
                    file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "assumed to be perfect."), file = test.file, 
                      append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of C2 (Specificity of reference test) is "), 
                    file = test.file, append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_C2, "(", round(Spec2.alpha[i], 
                        digits = 4), ",", round(Spec2.beta[i], 
                        digits = 4), "), <=> C2 in [", low.sp[i], 
                      ",", up.sp[i], "]"), file = test.file, 
                      append = TRUE)
                  }
                }
                else {
                  if (Gold_sp == TRUE & Gold_se == FALSE) {
                    write(paste("Prior of S2 (Sensitivity of reference test) is "), 
                      file = test.file, append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_S2, "(", round(Sens2.alpha[i], 
                        digits = 4), ",", round(Sens2.beta[i], 
                        digits = 4), "), <=> S2 in [", low.se[i], 
                        ",", up.se[i], "]"), file = test.file, 
                        append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of C2 (Specificity of reference test) is "), 
                      file = test.file, append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "assumed to be perfect."), file = test.file, 
                        append = TRUE)
                    }
                  }
                  else {
                    write(paste("Prior of S2 (Sensitivity of reference test) is "), 
                      file = test.file, append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_S2, "(", round(Sens2.alpha[i], 
                        digits = 4), ",", round(Sens2.beta[i], 
                        digits = 4), "), <=> S2 in [", low.se[i], 
                        ",", up.se[i], "]"), file = test.file, 
                        append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of C2 (Specificity of reference test) is "), 
                      file = test.file, append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", prior_dist_C2, "(", round(Spec2.alpha[i], 
                        digits = 4), ",", round(Spec2.beta[i], 
                        digits = 4), "), <=> C2 in [", low.sp[i], 
                        ",", up.sp[i], "]"), file = test.file, 
                        append = TRUE)
                    }
                  }
                }
            }
            else {
                if (condInd == TRUE & Gold_Std == FALSE & model == 
                  2) {
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of a1 is "), file = test.file, 
                    append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Normal (", round(mean.a1[i], 
                        digits = 4), ",", round(sd.a1[i], digits = 4), 
                      "), <=> S2 in []"), file = test.file, append = TRUE)
                  }
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("Prior of a0 is "), file = test.file, 
                    append = TRUE)
                  for (i in 1:rs.length) {
                    write(paste("Study(ies) ", sub_rs[[i + 1]][1], 
                      "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Normal (", round(mean.a0[i], 
                        digits = 4), ",", round(sd.a0[i], digits = 4), 
                      "), <=> C2 in []"), file = test.file, append = TRUE)
                  }
                }
                else {
                  if (condInd == FALSE) {
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of a1 is "), file = test.file, 
                      append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Normal (", round(mean.a1[i], 
                        digits = 4), ",", round(sd.a1[i], digits = 4), 
                        "), <=> S2 in []"), file = test.file, 
                        append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of a0 is "), file = test.file, 
                      append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Normal (", round(mean.a0[i], 
                        digits = 4), ",", round(sd.a0[i], digits = 4), 
                        "), <=> C2 in []"), file = test.file, 
                        append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of b1 is "), file = test.file, 
                      append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Uniform (", round(low.b1[i], 
                        digits = 4), ",", round(up.b1[i], digits = 4), 
                        "), <=> S2 in []"), file = test.file, 
                        append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of b0 is "), file = test.file, 
                      append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "Uniform (", round(low.b0[i], 
                        digits = 4), ",", round(up.b0[i], digits = 4), 
                        "), <=> C2 in []"), file = test.file, 
                        append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("Prior of d1 is Uniform(", round(low.d1, 
                      4), ",", round(up.d1, 4), ")"), file = test.file, 
                      append = TRUE)
                    write(paste("Prior of d0 is Uniform(", round(low.d0, 
                      4), ",", round(up.d0, 4), ")"), file = test.file, 
                      append = TRUE)
                  }
                }
            }
            write(paste(), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tBETWEEN_STUDY parameters (Point estimate =", 
                point_estimate, ")\t "), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("THETA       ", round(THETA.est, digits = digit), 
                "", round(THETA.sd, digits = digit), "", round(THETA.MCerror, 
                  digits = digit), "", round(THETA.HPD[1], digits = digit), 
                "", round(THETA.HPD[2], digits = digit)), file = test.file, 
                append = TRUE)
            write(paste("LAMBDA      ", round(LAMBDA.est, digits = digit), 
                "", round(LAMBDA.sd, digits = digit), "", round(LAMBDA.MCerror, 
                  digits = digit), "", round(LAMBDA.HPD[1], digits = digit), 
                "", round(LAMBDA.HPD[2], digits = digit)), file = test.file, 
                append = TRUE)
            write(paste("beta        ", round(beta.est, digits = digit), 
                "", round(beta.sd, digits = digit), "", round(beta.MCerror, 
                  digits = digit), "", round(beta.HPD[1], digits = digit), 
                "", round(beta.HPD[2], digits = digit)), file = test.file, 
                append = TRUE)
            write(paste("sigma.alpha ", round(sigma.alpha.est, 
                digits = digit), "", round(sigma.alpha.sd, digits = digit), 
                "", round(sigma.alpha.MCerror, digits = digit), 
                "", round(sigma.alpha.HPD[1], digits = digit), 
                "", round(sigma.alpha.HPD[2], digits = digit)), 
                file = test.file, append = TRUE)
            write(paste("sigma.theta ", round(sigma.theta.est, 
                digits = digit), "", round(sigma.theta.sd, digits = digit), 
                "", round(sigma.theta.MCerror, digits = digit), 
                "", round(sigma.theta.HPD[1], digits = digit), 
                "", round(sigma.theta.HPD[2], digits = digit)), 
                file = test.file, append = TRUE)
            write(paste("S overall          ", round(S_overall.est, 
                digits = digit), "", round(S_overall.sd, digits = digit), 
                "", round(S_overall.MCerror, digits = digit), 
                "", round(S_overall.HPD[1], digits = digit), 
                "", round(S_overall.HPD[2], digits = digit)), 
                file = test.file, append = TRUE)
            write(paste("C overall          ", round(C_overall.est, 
                digits = digit), "", round(C_overall.sd, digits = digit), 
                "", round(C_overall.MCerror, digits = digit), 
                "", round(C_overall.HPD[1], digits = digit), 
                "", round(C_overall.HPD[2], digits = digit)), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                1) {
                write(paste(""), file = test.file, append = TRUE)
                write(paste("______________________________________________________"), 
                  file = test.file, append = TRUE)
                write(paste("\tReference standard (Point estimate =", 
                  point_estimate, ")\t "), file = test.file, 
                  append = TRUE)
                write(paste("______________________________________________________"), 
                  file = test.file, append = TRUE)
                write(paste(""), file = test.file, append = TRUE)
                write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                  file = test.file, append = TRUE)
                write(paste(""), file = test.file, append = TRUE)
                if (Gold_se == TRUE & Gold_sp == FALSE) {
                  if (rs.length != 1) {
                    for (i in 1:rs.length) {
                      write(paste("S2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "(It was assumed to be perfect)"), 
                        file = test.file, append = TRUE)
                      write(paste("C2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(C2.est[i], digits = digit), 
                        "", round(C2.sd[i], digits = digit), 
                        "", round(C2.MCerror[i], digits = digit), 
                        "", round(C2.HPD[i, 1], digits = digit), 
                        "", round(C2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                  }
                  else {
                    write(paste("S2           (It was assumed to be perfect)"), 
                      file = test.file, append = TRUE)
                    write(paste("C2       ", round(C2.est, digits = digit), 
                      "", round(C2.sd, digits = digit), "", round(C2.MCerror, 
                        digits = digit), "", round(C2.HPD[1], 
                        digits = digit), "", round(C2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                  }
                }
                else {
                  if (Gold_sp == TRUE & Gold_se == FALSE) {
                    if (rs.length != 1) {
                      for (i in 1:rs.length) {
                        write(paste("S2 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(S2.est[i], digits = digit), 
                          "", round(S2.sd[i], digits = digit), 
                          "", round(S2.MCerror[i], digits = digit), 
                          "", round(S2.HPD[i, 1], digits = digit), 
                          "", round(S2.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                        write(paste("C2 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "(It was assumed to be perfect)"), 
                          file = test.file, append = TRUE)
                      }
                    }
                    else {
                      write(paste("S2       ", round(S2.est, 
                        digits = digit), "", round(S2.sd, digits = digit), 
                        "", round(S2.MCerror, digits = digit), 
                        "", round(S2.HPD[1], digits = digit), 
                        "", round(S2.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("C2           (It was assumed to be perfect)"), 
                        file = test.file, append = TRUE)
                    }
                  }
                  else {
                    if (rs.length != 1) {
                      for (i in 1:rs.length) {
                        write(paste("S2 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(S2.est[i], digits = digit), 
                          "", round(S2.sd[i], digits = digit), 
                          "", round(S2.MCerror[i], digits = digit), 
                          "", round(S2.HPD[i, 1], digits = digit), 
                          "", round(S2.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                      for (i in 1:rs.length) {
                        write(paste("C2 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(C2.est[i], digits = digit), 
                          "", round(C2.sd[i], digits = digit), 
                          "", round(C2.MCerror[i], digits = digit), 
                          "", round(C2.HPD[i, 1], digits = digit), 
                          "", round(C2.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                    }
                    else {
                      write(paste("S2       ", round(S2.est, 
                        digits = digit), "", round(S2.sd, digits = digit), 
                        "", round(S2.MCerror, digits = digit), 
                        "", round(S2.HPD[1], digits = digit), 
                        "", round(S2.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("C2       ", round(C2.est, 
                        digits = digit), "", round(C2.sd, digits = digit), 
                        "", round(C2.MCerror, digits = digit), 
                        "", round(C2.HPD[1], digits = digit), 
                        "", round(C2.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                  }
                }
            }
            else {
                if (condInd == TRUE & Gold_Std == FALSE & model == 
                  2) {
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = test.file, append = TRUE)
                  write(paste("\tReference standard (Point estimate =", 
                    point_estimate, ")\t "), file = test.file, 
                    append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = test.file, append = TRUE)
                  write(paste(""), file = test.file, append = TRUE)
                  write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                    file = test.file, append = TRUE)
                  write(paste(""), file = test.file, append = TRUE)
                  if (rs.length != 1) {
                    for (i in 1:rs.length) {
                      write(paste("a1 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(a1.est[i], digits = digit), 
                        "", round(a1.sd[i], digits = digit), 
                        "", round(a1.MCerror[i], digits = digit), 
                        "", round(a1.HPD[i, 1], digits = digit), 
                        "", round(a1.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("a0 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(a0.est[i], digits = digit), 
                        "", round(a0.sd[i], digits = digit), 
                        "", round(a0.MCerror[i], digits = digit), 
                        "", round(a0.HPD[i, 1], digits = digit), 
                        "", round(a0.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    write(paste(""), file = test.file, append = TRUE)
                    for (i in 1:rs.length) {
                      write(paste("S2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(S2.est[i], digits = digit), 
                        "", round(S2.sd[i], digits = digit), 
                        "", round(S2.MCerror[i], digits = digit), 
                        "", round(S2.HPD[i, 1], digits = digit), 
                        "", round(S2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                    for (i in 1:rs.length) {
                      write(paste("C2 of Study(ies) ", sub_rs[[i + 
                        1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                        1]])], "", round(C2.est[i], digits = digit), 
                        "", round(C2.sd[i], digits = digit), 
                        "", round(C2.MCerror[i], digits = digit), 
                        "", round(C2.HPD[i, 1], digits = digit), 
                        "", round(C2.HPD[i, 2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                  }
                  else {
                    write(paste("a1       ", round(a1.est, digits = digit), 
                      "", round(a1.sd, digits = digit), "", round(a1.MCerror, 
                        digits = digit), "", round(a1.HPD[1], 
                        digits = digit), "", round(a1.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("a0       ", round(a0.est, digits = digit), 
                      "", round(a0.sd, digits = digit), "", round(a0.MCerror, 
                        digits = digit), "", round(a0.HPD[1], 
                        digits = digit), "", round(a0.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("S2       ", round(S2.est, digits = digit), 
                      "", round(S2.sd, digits = digit), "", round(S2.MCerror, 
                        digits = digit), "", round(S2.HPD[1], 
                        digits = digit), "", round(S2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("C2       ", round(C2.est, digits = digit), 
                      "", round(C2.sd, digits = digit), "", round(C2.MCerror, 
                        digits = digit), "", round(C2.HPD[1], 
                        digits = digit), "", round(C2.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                  }
                }
                else {
                  if (condInd == FALSE) {
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("______________________________________________________"), 
                      file = test.file, append = TRUE)
                    write(paste("\tReference standard (Point estimate =", 
                      point_estimate, ")\t "), file = test.file, 
                      append = TRUE)
                    write(paste("______________________________________________________"), 
                      file = test.file, append = TRUE)
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                      file = test.file, append = TRUE)
                    write(paste(""), file = test.file, append = TRUE)
                    write(paste("d1       ", round(d1.est, digits = digit), 
                      "", round(d1.sd, digits = digit), "", round(d1.MCerror, 
                        digits = digit), "", round(d1.HPD[1], 
                        digits = digit), "", round(d1.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    write(paste("d0       ", round(d0.est, digits = digit), 
                      "", round(d0.sd, digits = digit), "", round(d0.MCerror, 
                        digits = digit), "", round(d0.HPD[1], 
                        digits = digit), "", round(d0.HPD[2], 
                        digits = digit)), file = test.file, append = TRUE)
                    if (rs.length != 1) {
                      for (i in 1:rs.length) {
                        write(paste("a1 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(a1.est[i], digits = digit), 
                          "", round(a1.sd[i], digits = digit), 
                          "", round(a1.MCerror[i], digits = digit), 
                          "", round(a1.HPD[i, 1], digits = digit), 
                          "", round(a1.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                      for (i in 1:rs.length) {
                        write(paste("a0 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(a0.est[i], digits = digit), 
                          "", round(a0.sd[i], digits = digit), 
                          "", round(a0.MCerror[i], digits = digit), 
                          "", round(a0.HPD[i, 1], digits = digit), 
                          "", round(a0.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                      write(paste(""), file = test.file, append = TRUE)
                      for (i in 1:rs.length) {
                        write(paste("S2 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(S2.est[i], digits = digit), 
                          "", round(S2.sd[i], digits = digit), 
                          "", round(S2.MCerror[i], digits = digit), 
                          "", round(S2.HPD[i, 1], digits = digit), 
                          "", round(S2.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                      for (i in 1:rs.length) {
                        write(paste("C2 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(C2.est[i], digits = digit), 
                          "", round(C2.sd[i], digits = digit), 
                          "", round(C2.MCerror[i], digits = digit), 
                          "", round(C2.HPD[i, 1], digits = digit), 
                          "", round(C2.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                      for (i in 1:rs.length) {
                        write(paste("b1 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(b1.est[i], digits = digit), 
                          "", round(b1.sd[i], digits = digit), 
                          "", round(b1.MCerror[i], digits = digit), 
                          "", round(b1.HPD[i, 1], digits = digit), 
                          "", round(b1.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                      for (i in 1:rs.length) {
                        write(paste("b0 of Study(ies) ", sub_rs[[i + 
                          1]][1], "to", sub_rs[[i + 1]][length(sub_rs[[i + 
                          1]])], "", round(b0.est[i], digits = digit), 
                          "", round(b0.sd[i], digits = digit), 
                          "", round(b0.MCerror[i], digits = digit), 
                          "", round(b0.HPD[i, 1], digits = digit), 
                          "", round(b0.HPD[i, 2], digits = digit)), 
                          file = test.file, append = TRUE)
                      }
                    }
                    else {
                      write(paste("a1       ", round(a1.est, 
                        digits = digit), "", round(a1.sd, digits = digit), 
                        "", round(a1.MCerror, digits = digit), 
                        "", round(a1.HPD[1], digits = digit), 
                        "", round(a1.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("a0       ", round(a0.est, 
                        digits = digit), "", round(a0.sd, digits = digit), 
                        "", round(a0.MCerror, digits = digit), 
                        "", round(a0.HPD[1], digits = digit), 
                        "", round(a0.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("b1       ", round(b1.est, 
                        digits = digit), "", round(b1.sd, digits = digit), 
                        "", round(b1.MCerror, digits = digit), 
                        "", round(b1.HPD[1], digits = digit), 
                        "", round(b1.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("b0       ", round(b0.est, 
                        digits = digit), "", round(b0.sd, digits = digit), 
                        "", round(b0.MCerror, digits = digit), 
                        "", round(b0.HPD[1], digits = digit), 
                        "", round(b0.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("S2       ", round(S2.est, 
                        digits = digit), "", round(S2.sd, digits = digit), 
                        "", round(S2.MCerror, digits = digit), 
                        "", round(S2.HPD[1], digits = digit), 
                        "", round(S2.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                      write(paste("C2       ", round(C2.est, 
                        digits = digit), "", round(C2.sd, digits = digit), 
                        "", round(C2.MCerror, digits = digit), 
                        "", round(C2.HPD[1], digits = digit), 
                        "", round(C2.HPD[2], digits = digit)), 
                        file = test.file, append = TRUE)
                    }
                  }
                }
            }
            write(paste(""), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tWITHIN-STUDY PARAMETERS \t "), file = test.file, 
                append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\ttheta \t "), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            for (i in 1:N) {
                write(paste("Study ", i, "", round(theta.est[i], 
                  digits = digit), "", round(theta.sd[i], digits = digit), 
                  "", round(theta.MCerror[i], digits = digit), 
                  "", round(theta.HPD[i, 1], digits = digit), 
                  "", round(theta.HPD[i, 2], digits = digit)), 
                  file = test.file, append = TRUE)
            }
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\talpha \t "), file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            for (i in 1:N) {
                write(paste("Study ", i, "", round(alpha.est[i], 
                  digits = digit), "", round(alpha.sd[i], digits = digit), 
                  "", round(alpha.MCerror[i], digits = digit), 
                  "", round(alpha.HPD[i, 1], digits = digit), 
                  "", round(alpha.HPD[i, 2], digits = digit)), 
                  file = test.file, append = TRUE)
            }
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tPrevalence \t "), file = test.file, 
                append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            for (i in 1:N) {
                write(paste("Study ", i, "", round(PI.est[i], 
                  digits = digit), "", round(PI.sd[i], digits = digit), 
                  "", round(PI.MCerror[i], digits = digit), "", 
                  round(PI.HPD[i, 1], digits = digit), "", round(PI.HPD[i, 
                    2], digits = digit)), file = test.file, append = TRUE)
            }
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tSensitivity of test 1 (S1) \t "), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            for (i in 1:N) {
                write(paste("Study ", i, "", round(S1.est[i], 
                  digits = digit), "", round(S1.sd[i], digits = digit), 
                  "", round(S1.MCerror[i], digits = digit), "", 
                  round(S1.HPD[i, 1], digits = digit), "", round(S1.HPD[i, 
                    2], digits = digit)), file = test.file, append = TRUE)
            }
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tSpecificity of test 1 (C1) \t "), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            for (i in 1:N) {
                write(paste("Study ", i, "", round(C1.est[i], 
                  digits = digit), "", round(C1.sd[i], digits = digit), 
                  "", round(C1.MCerror[i], digits = digit), "", 
                  round(C1.HPD[i, 1], digits = digit), "", round(C1.HPD[i, 
                    2], digits = digit)), file = test.file, append = TRUE)
            }
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tPosterior predictive value of Sensitivity of test under evaluation (S1) \t "), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("Sensitivity ", round(S1_new.est, digits = digit), 
                "", round(S1_new.sd, digits = digit), "", round(S1_new.MCerror, 
                  digits = digit), "", round(S1_new.HPD[1], digits = digit), 
                "", round(S1_new.HPD[2], digits = digit)), file = test.file, 
                append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste("\tPosterior predictive value of Specificity of test under evaluation (C1) \t "), 
                file = test.file, append = TRUE)
            write(paste("______________________________________________________"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("         Estimate Standard_Dev MC_error C.I._lower C.I._upper"), 
                file = test.file, append = TRUE)
            write(paste(""), file = test.file, append = TRUE)
            write(paste("Specificity", round(C1_new.est, digits = digit), 
                "", round(C1_new.sd, digits = digit), "", round(C1_new.MCerror, 
                  digits = digit), "", round(C1_new.HPD[1], digits = digit), 
                "", round(C1_new.HPD[2], digits = digit)), file = test.file, 
                append = TRUE)
            Num_study = c()
            for (i in 1:N) {
                Num_study = c(Num_study, paste("Study", i))
            }
            if (condInd == TRUE & Gold_Std == FALSE & model == 
                1) {
                if (Gold_se == TRUE & Gold_sp == FALSE) {
                  if (rs.length != 1) {
                    refstd_parameters = array(0, dim = c(rs.length, 
                      3, 1), dimnames = list(1:rs.length, c(paste(point_estimate, 
                      "estimate"), "HPD lower", "HPD upper"), 
                      "C2"))
                    refstd_parameters[, 1, 1] <- C2.est
                    refstd_parameters[, 2, 1] <- C2.HPD[, 1]
                    refstd_parameters[, 3, 1] <- C2.HPD[, 2]
                    long = length(alpha[, 1])
                    refstd_Parameters = array(0, c(long, rs.length, 
                      1))
                    refstd_Parameters[, , 1] <- C2
                    if (print_plot == TRUE) {
                      if (is.null(chain) == FALSE) {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = "Specificity"
                        par(mfcol = c(5, 2))
                        no_chains = length(chain)
                        iter_chain = round((iter.num * nb_chains - 
                          (burn_in) * nb_chains)/Thin, 0)/no_chains
                        longueur = 1:iter_chain
                        for (i in 1:rs.length) {
                          plot(x = longueur, y = refstd_Parameters[longueur, 
                            i, 1], type = "n", col = 1, ylab = paste(param, 
                            " of reference standard ", i), xlab = "iteration number", 
                            main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                          for (l in 1:length(chain)) {
                            lines(x = longueur, y = refstd_Parameters[longueur + 
                              (iter_chain * (l - 1)), i, 1], 
                              col = l)
                          }
                        }
                      }
                      else {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = "Specificity"
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        for (i in 1:rs.length) {
                          plot(x = longueur, y = refstd_Parameters[, 
                            i, 1], type = "l", col = "grey", 
                            ylab = paste(param, " of reference standard ", 
                              i), xlab = "iteration number", 
                            main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = "Specificity"
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      for (i in 1:rs.length) {
                        plot(density(refstd_Parameters[, i, 1]), 
                          lwd = 4, type = "l", col = "grey", 
                          main = paste(param, " of reference standard ", 
                            i, " \n Thinning interval = ", thin.interval, 
                            "\n Total samplesize kept = ", (iter.num * 
                              nb_chains - (burn_in) * nb_chains)/Thin))
                      }
                      dev.off()
                    }
                  }
                  else {
                    refstd_parameters = matrix(0, ncol = 3, nrow = 1)
                    rownames(refstd_parameters) = c("C2")
                    colnames(refstd_parameters) = c(paste(point_estimate, 
                      "estimate"), "HPD.low", "HPD.high")
                    refstd_parameters[1, 1] <- C2.est
                    refstd_parameters[1, 2] <- C2.HPD[1]
                    refstd_parameters[1, 3] <- C2.HPD[2]
                    long = length(THETA)
                    refstd_Parameters = matrix(0, nrow = long, 
                      ncol = 1)
                    colnames(refstd_Parameters) = c("C2")
                    refstd_Parameters[, 1] <- C2
                    if (print_plot == TRUE) {
                      if (is.null(chain) == FALSE) {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = c("Specificity of reference standard")
                        par(mfcol = c(5, 2))
                        no_chains = length(chain)
                        iter_chain = round((iter.num * nb_chains - 
                          (burn_in) * nb_chains)/Thin, 0)/no_chains
                        longueur = 1:iter_chain
                        plot(x = longueur, y = refstd_Parameters[longueur, 
                          1], type = "n", col = 1, ylab = paste(param), 
                          xlab = "iteration number", main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                        for (l in 1:length(chain)) {
                          lines(x = longueur, y = refstd_Parameters[longueur + 
                            (iter_chain * (l - 1)), 1], col = l)
                        }
                      }
                      else {
                        file.png_RS = paste("RefStd trace plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS)#, paper = "a4", height = 20)
                        param = c("Specificity of reference standard")
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        plot(x = longueur, y = refstd_Parameters[, 
                          1], type = "l", col = "grey", ylab = paste(param), 
                          xlab = "iteration number", main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = c("Specificity")
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      plot(density(refstd_Parameters[, 1]), lwd = 4, 
                        type = "l", col = "grey", main = paste(param, 
                          " of reference standard \n Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin))
                      dev.off()
                    }
                  }
                }
                else {
                  if (Gold_sp == TRUE & Gold_se == FALSE) {
                    if (rs.length != 1) {
                      refstd_parameters = array(0, dim = c(rs.length, 
                        3, 1), dimnames = list(1:rs.length, c(paste(point_estimate, 
                        "estimate"), "HPD lower", "HPD upper"), 
                        "S2"))
                      refstd_parameters[, 1, 1] <- S2.est
                      refstd_parameters[, 2, 1] <- S2.HPD[, 1]
                      refstd_parameters[, 3, 1] <- S2.HPD[, 2]
                      long = length(alpha[, 1])
                      refstd_Parameters = array(0, c(long, rs.length, 
                        1))
                      refstd_Parameters[, , 1] <- S2
                      if (print_plot == TRUE) {
                        if (is.null(chain) == FALSE) {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity")
                          par(mfcol = c(5, 2))
                          no_chains = length(chain)
                          iter_chain = round((iter.num * nb_chains - 
                            (burn_in) * nb_chains)/Thin, 0)/no_chains
                          longueur = 1:iter_chain
                          for (i in 1:rs.length) {
                            plot(x = longueur, y = refstd_Parameters[longueur, 
                              i, 1], type = "n", col = 1, ylab = paste(param, 
                              " of reference standard ", i), 
                              xlab = "iteration number", main = paste("Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                            for (l in 1:length(chain)) {
                              lines(x = longueur, y = refstd_Parameters[longueur + 
                                (iter_chain * (l - 1)), i, 1], 
                                col = l)
                            }
                          }
                        }
                        else {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity")
                          par(mfcol = c(5, 2))
                          longueur = 1:long
                          for (i in 1:rs.length) {
                            plot(x = longueur, y = refstd_Parameters[, 
                              i, 1], type = "l", col = "grey", 
                              ylab = paste(param, " of reference standard ", 
                                i), xlab = "iteration number", 
                              main = paste("Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                          }
                        }
                        dev.off()
                        file.png_RS2 = paste("RefStd density plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS2)#, paper = "a4", height = 20)
                        param = c("Sensitivity")
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        for (i in 1:rs.length) {
                          plot(density(refstd_Parameters[, i, 
                            1]), lwd = 4, type = "l", col = "grey", 
                            main = paste(param, " of reference standard ", 
                              i, " \n Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                        dev.off()
                      }
                    }
                    else {
                      refstd_parameters = matrix(0, ncol = 3, 
                        nrow = 1)
                      rownames(refstd_parameters) = c("S2")
                      colnames(refstd_parameters) = c(paste(point_estimate, 
                        "estimate"), "HPD.low", "HPD.high")
                      refstd_parameters[1, 1] <- S2.est
                      refstd_parameters[1, 2] <- S2.HPD[1]
                      refstd_parameters[1, 3] <- S2.HPD[2]
                      long = length(THETA)
                      refstd_Parameters = matrix(0, nrow = long, 
                        ncol = 1)
                      colnames(refstd_Parameters) = c("S2")
                      refstd_Parameters[, 1] <- S2
                      if (print_plot == TRUE) {
                        if (is.null(chain) == FALSE) {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity of reference standard")
                          par(mfcol = c(5, 2))
                          no_chains = length(chain)
                          iter_chain = round((iter.num * nb_chains - 
                            (burn_in) * nb_chains)/Thin, 0)/no_chains
                          longueur = 1:iter_chain
                          plot(x = longueur, y = refstd_Parameters[longueur, 
                            1], type = "n", col = 1, ylab = paste(param), 
                            xlab = "iteration number", main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                          for (l in 1:length(chain)) {
                            lines(x = longueur, y = refstd_Parameters[longueur + 
                              (iter_chain * (l - 1)), 1], col = l)
                          }
                        }
                        else {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity of reference standard")
                          par(mfcol = c(5, 2))
                          longueur = 1:long
                          plot(x = longueur, y = refstd_Parameters[, 
                            1], type = "l", col = "grey", ylab = paste(param), 
                            xlab = "iteration number", main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                        dev.off()
                        file.png_RS2 = paste("RefStd density plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS2)#, paper = "a4", height = 20)
                        param = c("Sensitivity of reference standard")
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        plot(density(refstd_Parameters[, 1]), 
                          lwd = 4, type = "l", col = "grey", 
                          main = paste(param, " \n Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                        dev.off()
                      }
                    }
                  }
                  else {
                    if (rs.length != 1) {
                      refstd_parameters = array(0, dim = c(rs.length, 
                        3, 2), dimnames = list(1:rs.length, c(paste(point_estimate, 
                        "estimate"), "HPD lower", "HPD upper"), 
                        c("S2", "C2")))
                      refstd_parameters[, 1, 1] <- S2.est
                      refstd_parameters[, 2, 1] <- S2.HPD[, 1]
                      refstd_parameters[, 3, 1] <- S2.HPD[, 2]
                      refstd_parameters[, 1, 2] <- C2.est
                      refstd_parameters[, 2, 2] <- C2.HPD[, 1]
                      refstd_parameters[, 3, 2] <- C2.HPD[, 2]
                      long = length(alpha[, 1])
                      refstd_Parameters = array(0, c(long, rs.length, 
                        2))
                      refstd_Parameters[, , 1] <- S2
                      refstd_Parameters[, , 2] <- C2
                      if (print_plot == TRUE) {
                        if (is.null(chain) == FALSE) {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity", "Specificity")
                          par(mfcol = c(5, 2))
                          no_chains = length(chain)
                          iter_chain = round((iter.num * nb_chains - 
                            (burn_in) * nb_chains)/Thin, 0)/no_chains
                          longueur = 1:iter_chain
                          for (j in 1:2) {
                            for (i in 1:rs.length) {
                              plot(x = longueur, y = refstd_Parameters[longueur, 
                                i, j], type = "n", col = 1, ylab = paste(param[j], 
                                " of reference standard ", i), 
                                xlab = "iteration number", main = paste("Thinning interval = ", 
                                  thin.interval, "\n Total samplesize kept = ", 
                                  (iter.num * nb_chains - (burn_in) * 
                                    nb_chains)/Thin))
                              for (l in 1:length(chain)) {
                                lines(x = longueur, y = refstd_Parameters[longueur + 
                                  (iter_chain * (l - 1)), i, 
                                  j], col = l)
                              }
                            }
                          }
                        }
                        else {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity", "Specificity")
                          par(mfcol = c(5, 2))
                          longueur = 1:long
                          for (j in 1:2) {
                            for (i in 1:rs.length) {
                              plot(x = longueur, y = refstd_Parameters[, 
                                i, j], type = "l", col = "grey", 
                                ylab = paste(param[j], " of reference standard ", 
                                  i), xlab = "iteration number", 
                                main = paste("Thinning interval = ", 
                                  thin.interval, "\n Total samplesize kept = ", 
                                  (iter.num * nb_chains - (burn_in) * 
                                    nb_chains)/Thin))
                            }
                          }
                        }
                        dev.off()
                        file.png_RS2 = paste("RefStd density plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS2)#, paper = "a4", height = 20)
                        param = c("Sensitivity", "Specificity")
                        par(mfcol = c(5, 2))
                        longueur = 1:long
                        for (j in 1:2) {
                          for (i in 1:rs.length) {
                            plot(density(refstd_Parameters[, 
                              i, j]), lwd = 4, type = "l", col = "grey", 
                              main = paste(param[j], " of reference standard ", 
                                i, " \n Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                          }
                        }
                        dev.off()
                      }
                    }
                    else {
                      refstd_parameters = matrix(0, ncol = 3, 
                        nrow = 2)
                      rownames(refstd_parameters) = c("S2", "C2")
                      colnames(refstd_parameters) = c(paste(point_estimate, 
                        "estimate"), "HPD.low", "HPD.high")
                      refstd_parameters[1, 1] <- S2.est
                      refstd_parameters[1, 2] <- S2.HPD[1]
                      refstd_parameters[1, 3] <- S2.HPD[2]
                      refstd_parameters[2, 1] <- C2.est
                      refstd_parameters[2, 2] <- C2.HPD[1]
                      refstd_parameters[2, 3] <- C2.HPD[2]
                      long = length(THETA)
                      refstd_Parameters = matrix(0, nrow = long, 
                        ncol = 2)
                      colnames(refstd_Parameters) = c("S2", "C2")
                      refstd_Parameters[, 1] <- S2
                      refstd_Parameters[, 2] <- C2
                      if (print_plot == TRUE) {
                        if (is.null(chain) == FALSE) {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity of reference standard", 
                            "Specificity of reference standard")
                          par(mfcol = c(5, 2))
                          no_chains = length(chain)
                          iter_chain = round((iter.num * nb_chains - 
                            (burn_in) * nb_chains)/Thin, 0)/no_chains
                          longueur = 1:iter_chain
                          for (j in 1:2) {
                            plot(x = longueur, y = refstd_Parameters[longueur, 
                              j], type = "n", col = 1, ylab = paste(param[j]), 
                              xlab = "iteration number", main = paste("Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                            for (l in 1:length(chain)) {
                              lines(x = longueur, y = refstd_Parameters[longueur + 
                                (iter_chain * (l - 1)), j], col = l)
                            }
                          }
                        }
                        else {
                          file.png_RS = paste("RefStd trace plots for N =", 
                            round((iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin, 0), ".png")
                          png(file.png_RS)#, paper = "a4", height = 20)
                          param = c("Sensitivity of reference standard", 
                            "Specificity of reference standard")
                          par(mfcol = c(5, 2))
                          longueur = 1:long
                          for (j in 1:2) {
                            plot(x = longueur, y = refstd_Parameters[, 
                              j], type = "l", col = "grey", ylab = paste(param[j]), 
                              xlab = "iteration number", main = paste("Thinning interval = ", 
                                thin.interval, "\n Total samplesize kept = ", 
                                (iter.num * nb_chains - (burn_in) * 
                                  nb_chains)/Thin))
                          }
                        }
                        dev.off()
                        file.png_RS2 = paste("RefStd density plots for N =", 
                          round((iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin, 0), ".png")
                        png(file.png_RS2)#, paper = "a4", height = 20)
                        param = c("Sensitivity of reference standard", 
                          "Specificity of reference standard")
                        par(mfcol = c(5, 2))
                        for (j in 1:2) {
                          longueur = 1:long
                          plot(density(refstd_Parameters[, j]), 
                            lwd = 4, type = "l", col = "grey", 
                            main = paste(param[j], " \n Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                        dev.off()
                      }
                    }
                  }
                }
            }
            else {
                if (condInd == TRUE & Gold_Std == FALSE & model == 
                  2) {
                  if (rs.length != 1) {
                    refstd_parameters = array(0, dim = c(rs.length, 
                      3, 4), dimnames = list(1:rs.length, c(paste(point_estimate, 
                      "estimate"), "HPD lower", "HPD upper"), 
                      c("S2", "C2", "a1", "a0")))
                    refstd_parameters[, 1, 1] <- S2.est
                    refstd_parameters[, 2, 1] <- S2.HPD[, 1]
                    refstd_parameters[, 3, 1] <- S2.HPD[, 2]
                    refstd_parameters[, 1, 2] <- C2.est
                    refstd_parameters[, 2, 2] <- C2.HPD[, 1]
                    refstd_parameters[, 3, 2] <- C2.HPD[, 2]
                    refstd_parameters[, 1, 3] <- a1.est
                    refstd_parameters[, 2, 3] <- a1.HPD[, 1]
                    refstd_parameters[, 3, 3] <- a1.HPD[, 2]
                    refstd_parameters[, 1, 4] <- a0.est
                    refstd_parameters[, 2, 4] <- a0.HPD[, 1]
                    refstd_parameters[, 3, 4] <- a0.HPD[, 2]
                    long = length(alpha[, 1])
                    refstd_Parameters = array(0, dim = c(long, 
                      rs.length, 4))
                    refstd_Parameters[, , 1] <- S2
                    refstd_Parameters[, , 2] <- C2
                    refstd_Parameters[, , 3] <- a1
                    refstd_Parameters[, , 4] <- a0
                    if (print_plot == TRUE) {
                      file.png_RS = paste("RefStd trace plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS)#, paper = "a4", height = 20)
                      param = c("Sensitivity", "Specificity", 
                        "a1", "a0")
                      par(mfcol = c(5, 2))
                      for (j in 1:4) {
                        longueur = 1:long
                        for (i in 1:rs.length) {
                          plot(x = longueur, y = refstd_Parameters[, 
                            i, j], type = "l", col = "grey", 
                            ylab = paste(param[j], " of reference standard ", 
                              i), xlab = "iteration number", 
                            main = paste("Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                          abline(a = refstd_parameters[i, 2, 
                            j], b = 0, col = "green", lwd = 3)
                          abline(a = refstd_parameters[i, 3, 
                            j], b = 0, col = "green", lwd = 3)
                        }
                      }
                      dev.off()
                      file.png_RS2 = paste("RefStd density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = c("Sensitivity", "Specificity", 
                        "a1", "a0")
                      par(mfcol = c(5, 2))
                      longueur = 1:long
                      for (j in 1:4) {
                        for (i in 1:rs.length) {
                          plot(density(refstd_Parameters[, i, 
                            j]), lwd = 4, type = "l", col = "grey", 
                            main = paste(param[j], " of reference standard ", 
                              i, " \n Thinning interval = ", 
                              thin.interval, "\n Total samplesize kept = ", 
                              (iter.num * nb_chains - (burn_in) * 
                                nb_chains)/Thin))
                        }
                      }
                      dev.off()
                    }
                  }
                  else {
                    refstd_parameters = matrix(0, ncol = 3, nrow = 4)
                    rownames(refstd_parameters) = c("S2", "C2", 
                      "a1", "a0")
                    colnames(refstd_parameters) = c(paste(point_estimate, 
                      "estimate"), "HPD.low", "HPD.high")
                    refstd_parameters[1, 1] <- S2.est
                    refstd_parameters[1, 2] <- S2.HPD[1]
                    refstd_parameters[1, 3] <- S2.HPD[2]
                    refstd_parameters[2, 1] <- C2.est
                    refstd_parameters[2, 2] <- C2.HPD[1]
                    refstd_parameters[2, 3] <- C2.HPD[2]
                    refstd_parameters[3, 1] <- a1.est
                    refstd_parameters[3, 2] <- a1.HPD[1]
                    refstd_parameters[3, 3] <- a1.HPD[2]
                    refstd_parameters[4, 1] <- a0.est
                    refstd_parameters[4, 2] <- a0.HPD[1]
                    refstd_parameters[4, 3] <- a0.HPD[2]
                    long = length(THETA)
                    refstd_Parameters = matrix(0, nrow = long, 
                      ncol = 4)
                    colnames(refstd_Parameters) = c("S2", "C2", 
                      "a1", "a0")
                    refstd_Parameters[, 1] <- S2
                    refstd_Parameters[, 2] <- C2
                    refstd_Parameters[, 3] <- a1
                    refstd_Parameters[, 4] <- a0
                    if (print_plot == TRUE) {
                      file.png_RS = paste("RefStd trace plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS, pointsize=18)#, paper = "a4", height = 20)
                      Param = c("Sensitivity of reference standard", 
                        "Specificity of reference standard", 
                        "a1", "a0")
                      par(mfcol = c(5, 2))
                      for (j in 1:4) {
                        longueur = 1:long
                        plot(x = longueur, y = refstd_Parameters[, 
                          j], type = "l", col = "grey", ylab = paste(Param[j]), 
                          xlab = "iteration number", main = paste("Thinning interval = ", 
                            thin.interval, "\n Total samplesize kept = ", 
                            (iter.num * nb_chains - (burn_in) * 
                              nb_chains)/Thin))
                        abline(a = refstd_parameters[j, 2], b = 0, 
                          col = "green", lwd = 3)
                        abline(a = refstd_parameters[j, 3], b = 0, 
                          col = "green", lwd = 3)
                      }
                      dev.off()
                      file.png_RS2 = paste("Density plots for N =", 
                        round((iter.num * nb_chains - (burn_in) * 
                          nb_chains)/Thin, 0), ".png")
                      png(file.png_RS2)#, paper = "a4", height = 20)
                      param = c("Sensitivity", "Specificity", 
                        "a1", "a0")
                      par(mfcol = c(5, 2))
                      for (j in 1:4) {
                        longueur = 1:long
                        plot(density(refstd_Parameters[, j]), 
                          lwd = 4, type = "l", col = "grey", 
                          main = paste(param[j], " of reference standard ", 
                            j, " \n Thinning interval = ", thin.interval, 
                            "\n Total samplesize kept = ", (iter.num * 
                              nb_chains - (burn_in) * nb_chains)/Thin))
                      }
                      dev.off()
                    }
                  }
                }
            }
            parameters = array(0, dim = c(N, 3, 5), dimnames = list(Num_study, 
                c(paste(point_estimate, "estimate"), "HPD lower", 
                  "HPD upper"), c("theta", "alpha", "pi", "S1", 
                  "C1")))
            parameters[, 1, 1] <- theta.est
            parameters[, 2, 1] <- theta.HPD[, 1]
            parameters[, 3, 1] <- theta.HPD[, 2]
            parameters[, 1, 2] <- alpha.est
            parameters[, 2, 2] <- alpha.HPD[, 1]
            parameters[, 3, 2] <- alpha.HPD[, 2]
            parameters[, 1, 3] <- PI.est
            parameters[, 2, 3] <- PI.HPD[, 1]
            parameters[, 3, 3] <- PI.HPD[, 2]
            parameters[, 1, 4] <- S1.est
            parameters[, 2, 4] <- S1.HPD[, 1]
            parameters[, 3, 4] <- S1.HPD[, 2]
            parameters[, 1, 5] <- C1.est
            parameters[, 2, 5] <- C1.HPD[, 1]
            parameters[, 3, 5] <- C1.HPD[, 2]
            long = length(alpha[, 1])
            Parameters = array(0, c(long, N, 5))
            Parameters[, , 1] <- theta
            Parameters[, , 2] <- alpha
            Parameters[, , 3] <- PI
            Parameters[, , 4] <- S1
            Parameters[, , 5] <- C1
            parameter = matrix(0, ncol = 3, nrow = 9)
            rownames(parameter) = c("THETA", "LAMBDA", "beta", 
                "sigma.alpha", "sigma.theta", "Sensitivity (summary)", "Specificity (summary)", 
                "Sensitivity (new)", "Specificity (new)")
            colnames(parameter) = c(paste(point_estimate, "estimate"), 
                "HPD.low", "HPD.high")
            parameter[1, 1] <- THETA.est
            parameter[1, 2] <- THETA.HPD[1]
            parameter[1, 3] <- THETA.HPD[2]
            parameter[2, 1] <- LAMBDA.est
            parameter[2, 2] <- LAMBDA.HPD[1]
            parameter[2, 3] <- LAMBDA.HPD[2]
            parameter[3, 1] <- beta.est
            parameter[3, 2] <- beta.HPD[1]
            parameter[3, 3] <- beta.HPD[2]
            parameter[4, 1] <- sigma.alpha.est
            parameter[4, 2] <- sigma.alpha.HPD[1]
            parameter[4, 3] <- sigma.alpha.HPD[2]
            parameter[5, 1] <- sigma.theta.est
            parameter[5, 2] <- sigma.theta.HPD[1]
            parameter[5, 3] <- sigma.theta.HPD[2]
            parameter[6, 1] <- S_overall.est
            parameter[6, 2] <- S_overall.HPD[1]
            parameter[6, 3] <- S_overall.HPD[2]
            parameter[7, 1] <- C_overall.est
            parameter[7, 2] <- C_overall.HPD[1]
            parameter[7, 3] <- C_overall.HPD[2]
            parameter[8, 1] <- S1_new.est
            parameter[8, 2] <- S1_new.HPD[1]
            parameter[8, 3] <- S1_new.HPD[2]
            parameter[9, 1] <- C1_new.est
            parameter[9, 2] <- C1_new.HPD[1]
            parameter[8, 3] <- C1_new.HPD[2]
            long = length(THETA)
            Parameter = matrix(0, nrow = long, ncol = 9)
            colnames(Parameter) = c("THETA", "LAMBDA", "beta", 
                "sigma.alpha", "sigma.theta", "Sensitivity (summary)", "Specificity (summary)", 
                "Sensitivity (new)", "Specificity (new)")
            Parameter[, 1] <- THETA
            Parameter[, 2] <- LAMBDA
            Parameter[, 3] <- beta
            Parameter[, 4] <- sigma.alpha
            Parameter[, 5] <- sigma.theta
            Parameter[, 6] <- S_overall
            Parameter[, 7] <- C_overall
            Parameter[, 8] <- S1_new
            Parameter[, 9] <- C1_new
            if (print_plot == TRUE) {
                if (is.null(chain) == FALSE) {
                  file.png5 = paste("Trace plots for N =", round((iter.num * 
                    nb_chains - (burn_in) * nb_chains)/Thin, 
                    0), ".png")
                  png(file.png5, width=1440, height=1440, pointsize=18)#, paper = "a4", height = 20)
                  param = c("theta", "alpha", "PI", "S1", "C1")
                  Param = c("Capital Theta", "Capital Lambda", 
                    "beta", "~sigma[alpha]", "~sigma[theta]", 
                    "Sensitivity (summary)", "Specificty (summary)", "Sensitivity (new)", "Specificity (new)")
                  no_chains = length(chain)
                  iter_chain = round((iter.num * nb_chains - 
                    (burn_in) * nb_chains)/Thin, 0)/no_chains
                  min_param = c(min(Parameters[, , 1]), min(Parameters[, 
                    , 2]), min(Parameters[, , 3]), min(Parameters[, 
                    , 4]), min(Parameters[, , 5]))
                  max_param = c(max(Parameters[, , 1]), max(Parameters[, 
                    , 2]), max(Parameters[, , 3]), max(Parameters[, 
                    , 4]), max(Parameters[, , 5]))
                  dlag = (max_param - min_param)/100
                  range_param = numeric()
                  for (j in 1:5) {
                    range_param = cbind(range_param, seq(min_param[j] + 
                      dlag[j]/2, max_param[j] - dlag[j]/2, by = dlag[j]))
                  }
                  par(mfcol = c(5, 2))
                  longueur = 1:iter_chain
                  for (j in 1:5) {
                    for (i in 1:N) {
                      plot(x = longueur, y = Parameters[longueur, 
                        i, j], type = "n", col = 1, ylab = paste(param[j], 
                        " of study ", i), xlab = "iteration number", 
                        main = paste("Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin), ylim = range(range_param[, 
                          j]))
                      for (l in 1:length(chain)) {
                        lines(x = longueur, y = Parameters[longueur + 
                          (iter_chain * (l - 1)), i, j], col = l)
                      }
                    }
                  }
                  min_Param = c(min(Parameter[, 1]), min(Parameter[, 
                    2]), min(Parameter[, 3]), min(Parameter[, 
                    4]), min(Parameter[, 5]), min(Parameter[, 
                    6]), min(Parameter[, 7]), min(Parameter[, 
                    8]), min(Parameter[, 9]))
                  max_Param = c(max(Parameter[, 1]), max(Parameter[, 
                    2]), max(Parameter[, 3]), max(Parameter[, 
                    4]), max(Parameter[, 5]), max(Parameter[, 
                    6]), max(Parameter[, 7]), max(Parameter[, 
                    8]), max(Parameter[, 9]))
                  dlag = (max_Param - min_Param)/100
                  range_Param = numeric()
                  for (j in 1:9) {
                    range_Param = cbind(range_Param, seq(min_Param[j] + 
                      dlag[j]/2, max_Param[j] - dlag[j]/2, by = dlag[j]))
                  }
                  for (j in 1:9) {
                    plot(x = longueur, y = Parameter[longueur, 
                      j], type = "n", col = 1, ylab = paste(Param[j]), 
                      xlab = "iteration number", main = paste("Thinning interval = ", 
                        thin.interval, "\n Total samplesize kept = ", 
                        (iter.num * nb_chains - (burn_in) * nb_chains)/Thin), 
                      ylim = range(range_Param[, j]))
                    for (l in 1:length(chain)) {
                      lines(x = longueur, y = Parameter[longueur + 
                        (iter_chain * (l - 1)), j], col = l)
                    }
                  }
                  dev.off()
                }
                else {
                  file.png2 = paste("Trace plots for N =", round((iter.num * 
                    nb_chains - (burn_in) * nb_chains)/Thin, 
                    0), ".png")
                  png(file.png2, pointsize=18, width=1440, height=1440)#, paper = "a4", height = 20)
                  param = c("theta", "alpha", "PI", "S1", "C1")
                  Param = c("Capital Theta", "Capital Lambda", 
                    "beta", "~sigma[alpha]", "~sigma[theta]", 
                    "S Overall", "C Overall", "S1_new", "C1_new")
                  min_param = c(min(Parameters[, , 1]), min(Parameters[, 
                    , 2]), min(Parameters[, , 3]), min(Parameters[, 
                    , 4]), min(Parameters[, , 5]), min(Parameters[, 
                    , 6]), min(Parameters[, , 7]))
                  max_param = c(max(Parameters[, , 1]), max(Parameters[, 
                    , 2]), max(Parameters[, , 3]), max(Parameters[, 
                    , 4]), max(Parameters[, , 5]), max(Parameters[, 
                    , 6]), max(Parameters[, , 7]))
                  dlag = (max_param - min_param)/100
                  range_param = numeric()
                  for (j in 1:5) {
                    range_param = cbind(range_param, seq(min_param[j] + 
                      dlag[j]/2, max_param[j] - dlag[j]/2, by = dlag[j]))
                  }
                  par(mfcol = c(5, 2))
                  longueur = 1:long
                  for (j in 1:5) {
                    for (i in 1:N) {
                      plot(x = longueur, y = Parameters[, i, 
                        j], type = "l", col = "grey", ylab = paste(param[j], 
                        " of study ", i), xlab = "iteration number", 
                        main = paste("Thinning interval = ", 
                          thin.interval, "\n Total samplesize kept = ", 
                          (iter.num * nb_chains - (burn_in) * 
                            nb_chains)/Thin), ylim = range(range_param[, 
                          j]))
                      abline(a = parameters[i, 2, j], b = 0, 
                        col = "green", lwd = 3)
                      abline(a = parameters[i, 3, j], b = 0, 
                        col = "green", lwd = 3)
                    }
                  }
                  min_Param = c(min(Parameter[, 1]), min(Parameter[, 
                    2]), min(Parameter[, 3]), min(Parameter[, 
                    4]), min(Parameter[, 5]), min(Parameter[, 
                    6]), min(Parameter[, 7]), min(Parameter[, 
                    8]), min(Parameter[, 9]))
                  max_Param = c(max(Parameter[, 1]), max(Parameter[, 
                    2]), max(Parameter[, 3]), max(Parameter[, 
                    4]), max(Parameter[, 5]), max(Parameter[, 
                    6]), max(Parameter[, 7]), max(Parameter[, 
                    8]), max(Parameter[, 9]))
                  dlag = (max_Param - min_Param)/100
                  range_Param = numeric()
                  for (j in 1:9) {
                    range_Param = cbind(range_Param, seq(min_Param[j] + 
                      dlag[j]/2, max_Param[j] - dlag[j]/2, by = dlag[j]))
                  }
                  for (j in 1:9) {
                    plot(x = longueur, y = Parameter[, j], type = "l", 
                      col = "grey", ylab = paste(Param[j]), xlab = "iteration number", 
                      main = paste("Thinning interval = ", thin.interval, 
                        "\n Total samplesize kept = ", (iter.num * 
                          nb_chains - (burn_in) * nb_chains)/Thin), 
                      ylim = range(range_Param[, j]))
                    abline(a = parameter[j, 2], b = 0, col = "green", 
                      lwd = 3)
                    abline(a = parameter[j, 3], b = 0, col = "green", 
                      lwd = 3)
                  }
                  dev.off()
                }


                density.plot.name <- "Density_plots_for_N_"

                # build density pdfs
                file.pdf3 = paste(density.plot.name, round((iter.num * 
                  nb_chains - (burn_in) * nb_chains)/Thin, 0), 
                  ".pdf", sep="")
                pdf(file.pdf3, paper = "a4", height = 20)
                make.density.plot(Parameters, Parameter, long, iter.num, 
                            nb_chains, burn_in, no_chains, Thin, thin.interval, N)
                dev.off()

                #file.png3 = paste(density.plot.name, round((iter.num * 
                #  nb_chains - (burn_in) * nb_chains)/Thin, 0), 
                #  ".png", sep="")
                #png(file.png3, width=1440, heigh=1440, pointsize=20)#, paper = "a4", height = 20)
                
                # for the kernel plots, we only generate PNGs for overalls
                density.pngs <- make.density.plot(Parameters, Parameter, long, iter.num, 
                                    nb_chains, burn_in, no_chains, Thin, thin.interval, N,
                                    make.pngs=TRUE, base.file.name=density.plot.name)

                image.list <- append(image.list, density.pngs)


                
                #image.list$density_plots <- c(file.png3, file.pdf3)

                # possibly the worse variable naming of all time.
                png("Summary_ROC_curve.png", width=1440, heigh=1440, pointsize=20)
                make.sroc.plot(Parameters, Parameter, data)
                dev.off()
                

                pdf("Summary_ROC_curve.pdf")
                make.sroc.plot(Parameters, Parameter, data)
                dev.off()
                image.list[["Summary ROC"]] <- "Summary_ROC_curve.png"#c("Summary_ROC_curve.png", "Summary_ROC_curve.pdf")

            }
        }
    }
    if (Gold_Std == FALSE) {
        Results = list(parameter, parameters, refstd_parameters, 
            paste("See '", getwd(), "' for complete results", 
                sep = ""))
        names(Results) = c("Between-study parameters", "Within-study parameters", 
            "Reference standard", "")
    }
    else {
        Results = list(parameter, parameters, paste("See '", 
            getwd(), "' for complete results", sep = ""))
        names(Results) = c("Between-study parameters", "Within-study parameters", 
            "")
    }

    Results$image.list <- image.list
    return(Results)
    cat(paste("See \"", getwd(), "\" for complete results", sep = ""))
}


make.sens.spec.density.plots <- function(refstd_Parameters, long, iter.num, 
                            nb_chains, burn_in, no_chains, Thin, thin.interval){
  param = c("Sensitivity", "Specificity", 
    "a1", "a0")
  par(mfcol = c(5, 2))
  for (j in 1:4) {
    longueur = 1:long
    plot(density(refstd_Parameters[, j]), 
      lwd = 4, type = "l", col = "grey", 
      main = paste(param[j], " of reference standard ", 
        j, " \n Thinning interval = ", thin.interval, 
        "\n Total samplesize kept = ", (iter.num * 
          nb_chains - (burn_in) * nb_chains)/Thin))
  }
}

make.density.plot <- function(Parameters, Parameter, long, 
            iter.num, nb_chains, burn_in, no_chains, Thin, thin.interval, N,
            make.pngs=FALSE, base.file.name=NULL){
    param = c("theta", "alpha", "PI", "Sensitivity", "Specificity")
    Param = c("Capital Theta", "Capital Lambda", 
      "beta", "~sigma[alpha]", "~sigma[theta]", "Sensitivity (summary)", 
      "Specificity (summary)", "Sensitivity (new)", "Specificity (new)")
    par(mfcol = c(5, 2))
    longueur = 1:long

    if (!make.pngs){
        for (j in 1:5) {
            for (i in 1:N) {
                # we're not making the PNGs, so just write to
                # the open device (assumed to be a PDF)
                plot(density(Parameters[, i, j]), lwd = 4, 
                  type = "l", col = "grey", main = paste(param[j], 
                    " of study ", i, " \n Thinning interval = ", 
                    thin.interval, "\n Total samplesize kept = ", 
                    (iter.num * nb_chains - (burn_in) * nb_chains)/Thin))
          }
        }
    }

    # we'll return these
    image.list <- c()            
    image.names <- c()

    # these, I believe, are the overall stats.
    for (j in 1:9) {
      if (make.pngs){
        file.png = paste(base.file.name, round((iter.num * 
             nb_chains - (burn_in) * nb_chains)/Thin, 0), "_", Param[j],
             ".png", sep="")
        # open a device to write to.
        png(file.png, width=480, heigh=480)#, pointsize=20)
        
        image.list <- c(image.list, file.png)
        image.names <- c(image.names, Param[j])
      }
      
      # no generate the density plot.
      plot(density(Parameter[, j]), lwd = 4, type = "l", 
        col = "grey", main = paste(Param[j], " \n Thinning interval = ", 
          thin.interval, "\n Total samplesize kept = ", 
          (iter.num * nb_chains - (burn_in) * nb_chains)/Thin))
      
      if (make.pngs){
        # kill the current PNG
        dev.off()
      }
    }

    if (make.pngs){
        image.list <- as.list(image.list)
        names(image.list) <- image.names
        image.list
    }

}


make.sroc.plot <- function(Parameters, Parameter, data){
    default.x = range(1, 0)
    default.y = range(0, 1)
    plot(x = default.x, y = default.y, type = "n", xlim = rev(range(default.x)), 
        xlab = "", ylab = "")
    title(xlab = "Specificity", ylab = "Sensitivity", 
        cex.lab = 1.5, main = "Summary ROC curve")
    Sensi1 = apply(as.matrix(Parameters[, , 4]), 2, median)
    Speci1 = apply(as.matrix(Parameters[, , 5]), 2, median)
    Scale_factor = 10
    symbols(Speci1, Sensi1, circles = rowSums(as.matrix(data[[1]])), 
        inches = 0.1 * Scale_factor/7, add = TRUE)
    Ov_Se = 1 - pnorm((median(Parameter[, 1]) - median(Parameter[, 
        2])/2)/exp(median(Parameter[, 3])/2))
    Ov_Sp = pnorm((median(Parameter[, 1]) + median(Parameter[, 
        2])/2)/exp(-median(Parameter[, 3])/2))
    points(Ov_Sp, Ov_Se, pch = 19, cex = 2)
    thet = qnorm((1 - as.matrix(Parameters[, , 4])) + 
        1e-14) * exp(Parameter[, 3]/2) + Parameter[, 
        2]/2
    min_TH = quantile(thet, 0.025)
    max_TH = quantile(thet, 0.975)
    dTH = 5e-05
    TH_range = seq(min_TH + dTH/2, max_TH - dTH/2, dTH)
    S_sroc = 1 - pnorm((TH_range - median(Parameter[, 
        2])/2)/exp(median(Parameter[, 3])/2))
    C_sroc = pnorm((TH_range + median(Parameter[, 2])/2)/exp(-median(Parameter[, 
        3])/2))
    lines(C_sroc, S_sroc, lwd = 3, col = "black", lty = 1)
}

