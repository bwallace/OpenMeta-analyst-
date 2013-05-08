HSROC <-
function (data, iter.num, init = NULL, sub_rs = NULL, first.run = TRUE, 
    path = getwd(), refresh = 100, prior.SEref = NULL, prior.SPref = NULL, 
    prior_PI = c(0, 1), prior_LAMBDA = c(-3, 3), prior_THETA = c(-1.5, 
        1.5), prior_sd_alpha = list(0, 2, "sd"), prior_sd_theta = list(0, 
        2, "sd"), prior_beta = c(-0.75, 0.75)) 
{
    if (missing(data)) 
        stop("You must provide a valid 'data' argument", call. = FALSE)
    N = length(data[, 1])
    Mem.check = N * iter.num * 8
    if (Mem.check > 1.6e+08) {
        print("Warning")
        print("You might come into trouble regarding memory allocation if you are using 32-bit version")
        print("Please select one of the options below")
        switch(menu(c("Abord and choose fewer iterations", "Ignore this warning")), 
            return("Please select fewer iterations"), NULL)
    }
    if (missing(iter.num) | iter.num <= 0) {
        cat("The number of iteration is either missing or less than 1. \n", 
            call. = FALSE)
        stop("Please respecify and call HSROC() again.\n")
    }
    if (is.null(sub_rs) == TRUE) {
        sub_rs = list(1, 1:N)
    }
    if (sub_rs[[1]] != (length(sub_rs) - 1)) {
        cat(paste("The value of the first element of 'sub_rs' (sub_rs[[1]] = ", 
            sub_rs[[1]], " ) does not match the number of remaining elements (length(sub_rs[[2:", 
            length(sub_rs), "]])) = ", length(2:length(sub_rs)), 
            "\n", sep = ""))
        stop("Please respecify and call HSROC() again.\n")
    }
    if (is.logical(first.run) == FALSE) {
        cat("The 'first.run' argument must be a logical object. \n")
        stop("Please respecify and call HSROC() again.\n")
    }
    if (is.null(prior.SEref) == FALSE | is.null(prior.SPref) == 
        FALSE) {
        if ((length(prior.SEref)/2 + length(prior.SEref)/2)/2 != 
            sub_rs[[1]]) {
            cat("The number of reference standards in 'prior.SEref' and(or) 'prior.SPref' is not matching the one defined in the 'sub_rs[[1]]' argument. \n")
            stop("Please respecify and call HSROC() again.\n")
        }
    }
    if (is.null(prior.SEref) == TRUE & is.null(prior.SPref) == 
        TRUE) {
        Gold_Std = TRUE
    }
    else {
        Gold_Std = FALSE
    }
    if (is.null(prior.SEref) == TRUE) {
        write(1, file = "S2.txt", ncolumns = 1)
    }
    else {
        write(2, file = "S2.txt", ncolumns = 1)
    }
    if (is.null(prior.SPref) == TRUE) {
        write(1, file = "C2.txt", ncolumns = 1)
    }
    else {
        write(2, file = "C2.txt", ncolumns = 1)
    }
    if (is.null(init) == FALSE) {
        random = FALSE
        if (sum(dim(init[[1]])) != N + 5) {
            cat(paste("Initial values for the within-study parameters were misspecified. Make sure the ordering described in the help file is preserved. \n"))
            stop("Please respecify and call HSROC() again.\n")
        }
        if (length(init[[2]]) != 5) {
            cat(paste("Initial values for the between-study parameters were misspecified. Make sure the ordering described in the help file is preserved. \n"))
            stop("Please respecify and call HSROC() again.\n")
        }
        if (Gold_Std == FALSE) {
            if (sum(dim(init[[3]])) != sub_rs[[1]] + 2) {
                cat(paste("Initial values for the test under evaluation were misspecified. Make sure the ordering described in the help file is preserved. \n"))
                stop("Please respecify and call HSROC() again.\n")
            }
        }
    }
    else {
        random = TRUE
    }
    low.pi = prior_PI[1]
    up.pi = prior_PI[2]
    if (all(low.pi < up.pi) == FALSE) {
        cat("The 'prior_PI' argument is a vector with 2 components specifying a range.  Thus, the first component of the vector must be less than the second component. Type '? HSROC' for more help. \n")
        stop("Please respecify and call HSROC() again.\n")
    }
    prior.LAMBDA.lower = prior_LAMBDA[1]
    prior.LAMBDA.upper = prior_LAMBDA[2]
    if (all(prior.LAMBDA.lower < prior.LAMBDA.upper) == FALSE) {
        cat("The 'prior_LAMBDA' argument is a vector with 2 components specifying a range.  Thus, the first component of the vector must be less than the second component. Type '? HSROC' for more help. \n")
        stop("Please respecify and call HSROC() again.\n")
    }
    prior.THETA.lower = prior_THETA[1]
    prior.THETA.upper = prior_THETA[2]
    if (all(prior.THETA.lower < prior.THETA.upper) == FALSE) {
        cat("The 'prior_THETA' argument is a vector with 2 components specifying a range.  Thus, the first component of the vector must be less than the second component. Type '? HSROC' for more help. \n")
        stop("Please respecify and call HSROC() again.\n")
    }
    l.disp.alpha = prior_sd_alpha[[1]]
    u.disp.alpha = prior_sd_alpha[[2]]
    if (all(l.disp.alpha < u.disp.alpha) == FALSE & prior_sd_alpha[[3]] != 
        "p") {
        cat("The 'prior_sd_alpha' argument is a list with the first 2 components specifying a range.  Thus, the first component of the list must be less than the second component. Type '? HSROC' for more help. \n")
        stop("Please respecify and call HSROC() again.\n")
    }
    l.disp.theta = prior_sd_theta[[1]]
    u.disp.theta = prior_sd_theta[[2]]
    if (all(l.disp.theta < u.disp.theta) == FALSE & prior_sd_theta[[3]] != 
        "p") {
        cat("The 'prior_sd_theta' argument is a list with the first 2 components specifying a range.  Thus, the first component of the list must be less than the second component. Type '? HSROC' for more help. \n")
        stop("Please respecify and call HSROC() again.\n")
    }
    if (is.null(prior_beta)) {
        beta.a = -log((prior.LAMBDA.upper/3) + 1)
        beta.b = log((prior.LAMBDA.upper/3) + 1)
    }
    else {
        beta.a = prior_beta[1]
        beta.b = prior_beta[2]
        if (all(beta.a < beta.b) == FALSE) {
            cat("The 'prior_beta' argument is a vector with 2 components specifying a range.  Thus, the first component of the vector must be less than the second component. Type '? HSROC' for more help. \n")
            stop("Please respecify and call HSROC() again.\n")
        }
    }
    write(1, file = "model.txt", ncolumns = 1)
    write(iter.num, file = "iter.txt")
    data = list(data)
    file.pi = "PI.txt"
    file.C2 = "Spec2.txt"
    file.S2 = "Sens2.txt"
    file.alpha = "alpha.txt"
    file.theta = "theta.txt"
    file.sig.theta = "sigma.theta.txt"
    file.sig.alpha = "sigma.alpha.txt"
    file.THETA = "capital.THETA.txt"
    file.LAMBDA = "LAMBDA.txt"
    file.beta = "beta.txt"
    file.C1 = "Spec1.txt"
    file.S1 = "Sens1.txt"
    file.C_overall = "C_overall.txt"
    file.S_overall = "S_overall.txt"
    file.choix = "choix.txt"
    file.ll = "log.likelihood.txt"
    file.Yj = "Y_j.txt"
    file.TV = "Start_values.txt"
    file.TV2 = "Start_values2.txt"
    file.TV3 = "Start_REFSTD.txt"
    file.Restart = "Restore.txt"
    file.Restart2 = "Restore2.txt"
    file.Restart_REFSTD = "Restore3.txt"
    file.Restart_index = "Restore_index.txt"
    file.A.alpha = "A.alpha.txt"
    file.B.alpha = "B.alpha.txt"
    file.mean.rij.one = "mean.rij.one.txt"
    file.mean.rij.zero = "mean.rij.zero.txt"
    setwd(path)
    condInd = TRUE
    prior_dist_PI = "beta"
    range_rij = c(-prior.LAMBDA.upper/2 - 3 * exp(-beta.a/2), 
        prior.LAMBDA.upper/2 + 3 * exp(beta.b/2))
    low.rj = range_rij[1]
    up.rj = range_rij[2]
    write(c(low.rj, up.rj), file = "range of latent variable.txt", 
        ncolumns = 2)
    alpha.PI = beta.parameter(low = low.pi, up = up.pi)[1, ]
    beta.PI = beta.parameter(low = low.pi, up = up.pi)[2, ]
    L.disp.alpha = L.disp.theta = numeric()
    if (l.disp.alpha == 0) {
        L.disp.alpha = 1e-10
    }
    else {
        if (l.disp.alpha > 0) {
            L.disp.alpha = l.disp.alpha
        }
    }
    if (prior_sd_alpha[[3]] == "sd") {
        prior_sig_alpha = 1
        low.disp.alpha = u.disp.alpha^(-2)
        up.disp.alpha = L.disp.alpha^(-2)
        write(1, file = "Prior on sigma_alpha.txt", ncolumns = 1)
    }
    else {
        if (prior_sd_alpha[[3]] == "v") {
            prior_sig_alpha = 2
            low.disp.alpha = u.disp.alpha^(-1)
            up.disp.alpha = L.disp.alpha^(-1)
            write(2, file = "Prior on sigma_alpha.txt", ncolumns = 1)
        }
        else {
            if (prior_sd_alpha[[3]] == "p") {
                prior_sig_alpha = 3
                low.disp.alpha = L.disp.alpha
                up.disp.alpha = u.disp.alpha
                write(3, file = "Prior on sigma_alpha.txt", ncolumns = 1)
            }
        }
    }
    if (l.disp.theta == 0) {
        L.disp.theta = 1e-10
    }
    else {
        if (l.disp.theta > 0) {
            L.disp.theta = l.disp.theta
        }
    }
    if (prior_sd_theta[[3]] == "sd") {
        prior_sig_theta = 1
        low.disp.theta = u.disp.theta^(-2)
        up.disp.theta = L.disp.theta^(-2)
        write(1, file = "Prior on sigma_theta.txt", ncolumns = 1)
    }
    else {
        if (prior_sd_theta[[3]] == "v") {
            prior_sig_theta = 2
            low.disp.theta = u.disp.theta^(-1)
            up.disp.theta = L.disp.theta^(-1)
            write(2, file = "Prior on sigma_theta.txt", ncolumns = 1)
        }
        else {
            if (prior_sd_theta[[3]] == "p") {
                prior_sig_theta = 3
                low.disp.theta = L.disp.theta
                up.disp.theta = u.disp.theta
                write(3, file = "Prior on sigma_theta.txt", ncolumns = 1)
            }
        }
    }
    long.se = length(prior.SEref)
    low.se = prior.SEref[1:sub_rs[[1]]]
    up.se = prior.SEref[(sub_rs[[1]] + 1):long.se]
    long.sp = length(prior.SPref)
    low.sp = prior.SPref[1:sub_rs[[1]]]
    up.sp = prior.SPref[(sub_rs[[1]] + 1):long.sp]
    if (Gold_Std == FALSE) {
        if (is.null(prior.SEref) == TRUE) {
            Sens2.alpha = Sens2.beta = NULL
            Gold_se = TRUE
        }
        else {
            if (is.null(prior.SEref) == FALSE) {
                Sens2.alpha = beta.parameter(low = low.se, up = up.se)[1, 
                  ]
                Sens2.beta = beta.parameter(low = low.se, up = up.se)[2, 
                  ]
                Gold_se = FALSE
            }
        }
    }
    else {
        if (Gold_Std == TRUE) {
            Sens2.alpha = Sens2.beta = NULL
            Gold_se = NULL
        }
    }
    if (Gold_Std == FALSE) {
        if (is.null(prior.SPref) == TRUE) {
            Spec2.alpha = Spec2.beta = NULL
            Gold_sp = TRUE
        }
        else {
            if (is.null(prior.SPref) == FALSE) {
                Spec2.alpha = beta.parameter(low = low.sp, up = up.sp)[1, 
                  ]
                Spec2.beta = beta.parameter(low = low.sp, up = up.sp)[2, 
                  ]
                Gold_sp = FALSE
            }
        }
    }
    else {
        if (Gold_Std == TRUE) {
            Spec2.alpha = Spec2.beta = NULL
            Gold_sp = NULL
        }
    }
    if (first.run == TRUE) {
        RESTART_i = NA
        RESTART = NA
        RESTART_REFSTD = NA
        file.create(file.Restart)
        file.create(file.Restart2)
        file.create(file.Restart_REFSTD)
    }
    else {
        DATA.restart = read.table(file.Restart)
        DATA.restart2 = read.table(file.Restart2)
        DATA.restart_refstd = read.table(file.Restart_REFSTD)
        RESTART_i = t(DATA.restart)
        RESTART = DATA.restart2
        RESTART_REFSTD = DATA.restart_refstd
    }
    PRIOR.Parameters = c(beta.a, beta.b, prior.THETA.lower, prior.THETA.upper, 
        prior.LAMBDA.lower, prior.LAMBDA.upper, low.disp.alpha, 
        up.disp.alpha, low.disp.theta, up.disp.theta, alpha.PI, 
        beta.PI, Sens2.alpha, Sens2.beta, Spec2.alpha, Spec2.beta)
    test.results = data[[1]]
    Start.values = Which_data(RANDOM = random, data = data, init = init, 
        GS = Gold_Std)[[1]]
    Start.values2 = Which_data(RANDOM = random, data = data, 
        init = init, GS = Gold_Std)[[2]]
    Start.REFSTD = Which_data(RANDOM = random, data = data, init = init, 
        GS = Gold_Std)[[3]]
    INITS = Initialization(first.run = first.run, random = random, 
        param = PRIOR.Parameters, cond.Ind = condInd, rs = sub_rs, 
        GS_se = Gold_se, GS_sp = Gold_sp, Data1 = Start.values, 
        Data2 = RESTART_i, Data3 = RESTART, Data4 = Start.values2, 
        Data5 = Start.REFSTD, Data6 = RESTART_REFSTD, path = path, 
        studies = N, sco = FALSE, psa = prior_sd_alpha[[3]], 
        pst = prior_sd_theta[[3]])
    if (INITS[[1]][3] == 0 | INITS[[1]][1] == 0) {
        cat(paste("Unsuitable initial values were provided. "))
        stop("Please respecify and call HSROC() again.\n  If you're using 'init=NULL' you need just to run the 'HSROC' function again.\n")
    }
    init.sigma.alpha = INITS[[1]][3]
    prec.alpha = INITS[[1]][4]
    init.THETA = INITS[[1]][5]
    init.LAMBDA = INITS[[1]][6]
    init.beta = INITS[[1]][7]
    init.alpha = as.vector(INITS[[2]][, 1])
    init.S1 = as.vector(INITS[[2]][, 3])
    init.C1 = as.vector(INITS[[2]][, 4])
    init.PI = as.vector(INITS[[2]][, 5])
    init.sigma.theta = INITS[[1]][1]
    prec.theta = INITS[[1]][2]
    init.theta = as.vector(INITS[[2]][, 2])
    if (Gold_Std == FALSE) {
        if (Gold_se == TRUE) {
            init.C2 = as.vector(INITS[[3]][2, ])
        }
        else {
            if (Gold_sp == TRUE) {
                init.S2 = as.vector(INITS[[3]][1, ])
            }
            else {
                init.S2 = as.vector(INITS[[3]][1, ])
                init.C2 = as.vector(INITS[[3]][2, ])
            }
        }
    }
    D = DATA.organizer(d = test.results, m = N)
    n = D[[1]]
    All.Studies = D[[2]]
    t1 = numeric()
    t2 = numeric()
    T = t(mapply(Test, All.Studies))
    t1 = T[, 1]
    t2 = T[, 2]
    studygroup = rep((1:N), n)
    n_rs = numeric()
    n_REFSTD = REFSTD_3(rs = sub_rs, n.sample = D[[1]])
    studygroup_REFSTD = REFSTD_4(rs = sub_rs, n.sample = D[[1]], 
        n_rs = n_REFSTD)
    Total = sum(n)
    n.refstd = sub_rs[[1]]
    PRIOR.BETWEEN = rbind(c(beta.a, beta.b), c(prior.THETA.lower, 
        prior.THETA.upper), c(prior.LAMBDA.lower, prior.LAMBDA.upper), 
        c(l.disp.alpha, u.disp.alpha), c(l.disp.theta, u.disp.theta), 
        c(low.pi, up.pi), c(low.rj, up.rj))
    colnames(PRIOR.BETWEEN) = c("Lower bound", "Upper bound")
    rownames(PRIOR.BETWEEN) = c("beta", "THETA", "LAMBDA", "sigma_alpha", 
        "sigma_theta", "prevalence", "Range_rij")
    write.table(PRIOR.BETWEEN, file = "Prior.information.txt")
    if (Gold_Std == FALSE) {
        if (Gold_se == TRUE) {
            C2.p = c()
            for (i in 1:length(n_REFSTD)) {
                C2.p = c(C2.p, paste("C2", i, sep = ""))
            }
            PRIOR.C2 = cbind(low.sp, up.sp)
            rownames(PRIOR.C2) = C2.p
            colnames(PRIOR.C2) = c("lower", "upper")
            write.table(PRIOR.C2, file = "Prior.information.txt", 
                append = TRUE, col.names = FALSE)
        }
        else {
            if (Gold_sp == TRUE) {
                S2.p = c()
                for (i in 1:length(n_REFSTD)) {
                  S2.p = c(S2.p, paste("S2", i, sep = ""))
                }
                PRIOR.S2 = cbind(low.se, up.se)
                rownames(PRIOR.S2) = S2.p
                colnames(PRIOR.S2) = c("lower", "upper")
                write.table(PRIOR.S2, file = "Prior.information.txt", 
                  append = TRUE, col.names = FALSE)
            }
            else {
                S2.p = C2.p = c()
                for (i in 1:length(n_REFSTD)) {
                  S2.p = c(S2.p, paste("S2", i, sep = ""))
                  C2.p = c(C2.p, paste("C2", i, sep = ""))
                }
                PRIOR.S2 = cbind(low.se, up.se)
                rownames(PRIOR.S2) = S2.p
                colnames(PRIOR.S2) = c("lower", "upper")
                PRIOR.C2 = cbind(low.sp, up.sp)
                rownames(PRIOR.C2) = C2.p
                colnames(PRIOR.C2) = c("lower", "upper")
                write.table(PRIOR.S2, file = "Prior.information.txt", 
                  append = TRUE, col.names = FALSE)
                write.table(PRIOR.C2, file = "Prior.information.txt", 
                  append = TRUE, col.names = FALSE)
            }
        }
    }
    vec.PI = as.numeric(init.PI)
    vec.S1 = as.numeric(init.S1)
    vec.C1 = as.numeric(init.C1)
    vec.alpha = as.numeric(init.alpha)
    vec.sigma.alpha = as.numeric(init.sigma.alpha)
    vec.THETA = as.numeric(init.THETA)
    vec.LAMBDA = as.numeric(init.LAMBDA)
    vec.beta = as.numeric(init.beta)
    vec.MH = as.numeric(exp(vec.beta))
    vec.sigma.theta = as.numeric(init.sigma.theta)
    vec.theta = as.numeric(init.theta)
    if (Gold_Std == TRUE) {
        vec.S2 = vec.C2 = init.S2 = init.C2 = 1
        Sens2.alpha = Sens2.beta = Spec2.alpha = Spec2.beta = 1
    }
    else {
        if (Gold_se == TRUE) {
            vec.C2 = as.numeric(init.C2)
            vec.S2 = 1
        }
        else {
            if (Gold_sp == TRUE) {
                vec.C2 = 1
                vec.S2 = as.numeric(init.S2)
            }
            else {
                vec.C2 = as.numeric(init.C2)
                vec.S2 = as.numeric(init.S2)
            }
        }
    }
    gibbs = gibbs_sampler_Cpp(iter.num, Gold_Std, Gold_se, Gold_sp, 
        Total, t1, t2, init.PI, init.S1, init.S2, init.C1, init.C2, 
        n, N, alpha.PI, beta.PI, n.refstd, n_REFSTD, Sens2.alpha, 
        Sens2.beta, Spec2.alpha, Spec2.beta, init.alpha, init.theta, 
        init.beta, low.rj, up.rj, init.THETA, init.sigma.theta, 
        init.sigma.alpha, init.LAMBDA, prior.LAMBDA.lower, prior.LAMBDA.upper, 
        beta.a, beta.b, prior.THETA.lower, prior.THETA.upper, 
        low.disp.alpha, up.disp.alpha, low.disp.theta, up.disp.theta, 
        prior_sig_alpha, prior_sig_theta, refresh)
    Restore(gibbs, Gold_Std)
    if (Gold_Std == TRUE) {
        file.remove(file.C2)
        file.remove(file.S2)
    }
    cat(paste("The files created during the Gibbs sampler process are in  \"", 
        getwd(), "\" ", sep = ""))
}
