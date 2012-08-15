simdata <-
function (N, n, n.random = FALSE, sub_rs = NULL, prev, se_ref = NULL, 
    sp_ref = NULL, T, range.T = c(-Inf, Inf), L, range.L = c(-Inf, 
        Inf), sd_t, sd_a, b, path = getwd()) 
{
    if (N < 1) {
        cat("Number of studies must be at least 1 or greater. \n")
        stop("Please respecify and call simdata() again.\n")
    }
    if (missing(N)) {
        cat("You must provide the number of studies 'N'. \n")
        stop("Please respecify and call simdata() again.\n")
    }
    if (missing(n)) {
        cat("You must provide the number of individuals 'n' within each study. \n")
        stop("Please respecify and call simdata() again.\n")
    }
    if (is.logical(n.random) == FALSE) {
        cat("The 'n.random' argument must be a logical object. \n")
        stop("Please respecify and call simdata() again.\n")
    }
    if (length(n) != N & length(n) != 1 & n.random == FALSE) {
        cat("You must provide the number of individuals 'n' for each study. \n")
        stop("Please respecify and call simdata() again.\n")
    }
    if (is.null(sub_rs) == TRUE) {
        n_rs = 1
        sub_rs = list(1, 1:N)
    }
    else {
        n_rs = sub_rs[[1]]
    }
    if (sub_rs[[1]] != (length(sub_rs) - 1)) {
        cat(paste("The value of the first element of 'sub_rs' (sub_rs[[1]] = ", 
            sub_rs[[1]], " ) does \n\t\tnot match the number of remaining elements (length(sub_rs[[2:", 
            length(sub_rs), "]])) = ", length(2:length(sub_rs)), 
            "\n", sep = ""))
        stop("Please respecify and call simdata() again.\n")
    }
    if (is.null(se_ref) == FALSE | is.null(sp_ref) == FALSE) {
        if ((length(se_ref) != sub_rs[[1]])) {
            cat("The number of reference standards in 'se_ref' and(or) \n\t\t\t\t'sp_ref' is not matching the one defined in the \n\t\t\t\t'sub_rs[[1]]' argument. \n")
            stop("Please respecify and call simdata() again.\n")
        }
    }
    if (is.null(sd_t) == TRUE) {
        SCO = TRUE
    }
    else {
        SCO = FALSE
    }
    if (length(prev) != 1 & length(prev) != N) 
        stop(paste("You must provide M =", N, "elements for 'prev' argument"), 
            call. = FALSE)
    if (is.null(se_ref) != "TRUE" & length(se_ref) != sub_rs[[1]]) 
        stop("Number of reference standards not matching the one defined for 'sub_rs' argument", 
            call. = FALSE)
    if (is.null(se_ref) != "TRUE" & length(sp_ref) != sub_rs[[1]]) 
        stop("Number of reference standards not matching the one defined for 'sub_rs' argument", 
            call. = FALSE)
    if (missing(T)) 
        stop("You must provide a value for parameter THETA ('T') ")
    if (missing(L)) 
        stop("You must provide a value for parameter LAMBDA  ('L')")
    if (missing(sd_a)) 
        stop("You must provide a value for parameter sd_alpha ('sd_a')")
    if (missing(sd_t)) 
        stop("You must provide a value for parameter sd_theta ('sd_t')")
    if (missing(b)) 
        stop("You must provide a value for parameter beta ('b')")
    s2 = se_ref
    c2 = sp_ref
    pi = prev
    LOW_a = range.L[1]
    UP_a = range.L[2]
    LOW_t = range.T[1]
    UP_t = range.T[2]
    alpha = mapply(truncnorm2, rep(LOW_a, N), rep(UP_a, N), MoreArgs = list(L, 
        sd_a, 1))
    if (SCO == FALSE) {
        theta = mapply(truncnorm2, rep(LOW_t, N), rep(UP_t, N), 
            MoreArgs = list(T, sd_t, 1))
    }
    else {
        theta = rep(T, N)
    }
    if (is.null(s2) == "TRUE" & is.null(c2) == "TRUE") {
        Gold_Std = TRUE
        model = NULL
    }
    else {
        if (is.null(s2) == FALSE & is.null(c2) == FALSE) {
            Gold_Std = FALSE
            model = 1
        }
    }
    if (Gold_Std == TRUE) {
        se2 = sp2 = 1
        tp <- pnorm((theta - (alpha/2)) * exp(-b/2), mean = 0, 
            sd = 1, lower.tail = FALSE)
        fp <- pnorm((theta + (alpha/2)) * exp(b/2), mean = 0, 
            sd = 1, lower.tail = FALSE)
        TP_overall <- pnorm((T - (L/2)) * exp(-b/2), mean = 0, 
            sd = 1, lower.tail = FALSE)
        FP_overall <- pnorm((T + (L/2)) * exp(b/2), mean = 0, 
            sd = 1, lower.tail = FALSE)
    }
    else {
        if (Gold_Std == FALSE & model == 1) {
            se2 = sp2 = ni_rs = numeric()
            for (i in 1:n_rs) {
                ni_rs = c(ni_rs, length(sub_rs[[i + 1]]))
                se2 = c(se2, rep(s2[i], ni_rs[i]))
                sp2 = c(sp2, rep(c2[i], ni_rs[i]))
            }
            tp <- pnorm((theta - (alpha/2)) * exp(-b/2), mean = 0, 
                sd = 1, lower.tail = FALSE)
            fp <- pnorm((theta + (alpha/2)) * exp(b/2), mean = 0, 
                sd = 1, lower.tail = FALSE)
            TP_overall <- pnorm((T - (L/2)) * exp(-b/2), mean = 0, 
                sd = 1, lower.tail = FALSE)
            FP_overall <- pnorm((T + (L/2)) * exp(b/2), mean = 0, 
                sd = 1, lower.tail = FALSE)
        }
    }
    if (n.random == "FALSE" & length(n) == 1) {
        n = rep(n, N)
    }
    else {
        if (n.random == "TRUE" & length(n) >= 1) {
            n = sample(n, N, replace = TRUE)
        }
    }
    p1 = pi * tp * se2 + (1 - pi) * fp * (1 - sp2)
    p2 = pi * tp * (1 - se2) + (1 - pi) * fp * sp2
    p3 = pi * (1 - tp) * se2 + (1 - pi) * (1 - fp) * (1 - sp2)
    p4 = pi * (1 - tp) * (1 - se2) + (1 - pi) * (1 - fp) * sp2
    prob = cbind(p1, p2, p3, p4)
    data.sim = matrix(0, ncol = 4, nrow = N)
    colnames(data.sim) = c("++", "+-", "-+", "--")
    for (i in 1:N) {
        data.sim[i, ] = rmultinom(n = 1, size = n[i], prob = prob[i, 
            ])
    }
    file.TV = "True_values.txt"
    file.TV2 = "True_values2.txt"
    file.TV3 = "True_REFSTD.txt"
    file.TV.index = "True_values_index.txt"
    if (SCO == FALSE) {
        sim1 = cbind(alpha, theta, tp, (1 - fp), pi, data.sim[, 
            1], data.sim[, 2], data.sim[, 3], data.sim[, 4])
        write.table(sim1, file = file.TV, col.names = FALSE, 
            row.names = FALSE)
    }
    else {
        if (SCO == TRUE) {
            sim1 = cbind(alpha, tp, (1 - fp), pi, data.sim[, 
                1:4])
            write.table(sim1, file = file.TV, col.names = FALSE, 
                row.names = FALSE)
        }
    }
    if (SCO == FALSE) {
        if (Gold_Std == TRUE) {
            sim2 = c(T, sd_t, L, sd_a, b, TP_overall, (1 - FP_overall))
            write(sim2, file = file.TV2, ncolumns = 7)
            names(sim2) = c("THETA", "sigma theta", "LAMBDA", 
                "sigma alpha", "beta", "Overal ++", "Overall --")
            sim_rs = "PERFECT"
        }
        else {
            if (Gold_Std == FALSE & model == 1) {
                sim2 = c(T, sd_t, L, sd_a, b, TP_overall, (1 - 
                  FP_overall))
                write(sim2, file = file.TV2, ncolumns = 7)
                names(sim2) = c("THETA", "sigma theta", "LAMBDA", 
                  "sigma alpha", "beta", "Overal ++", "Overall --")
                sim_rs = rbind(s2, c2)
                write.table(sim_rs, file = file.TV3, col.names = FALSE, 
                  row.names = FALSE)
                sim_rs_label = numeric()
                for (i in 1:n_rs) {
                  sim_rs_label = c(sim_rs_label, i)
                }
                colnames(sim_rs) = sim_rs_label
            }
        }
    }
    else {
        if (SCO == TRUE) {
            if (Gold_Std == TRUE) {
                sim2 = c(T, L, sd_a, b, TP_overall, (1 - FP_overall))
                write(sim2, file = file.TV2, ncolumns = 6)
                names(sim2) = c("THETA", "LAMBDA", "sigma alpha", 
                  "beta", "Overal ++", "Overall --")
                sim_rs = "PERFECT"
            }
            else {
                if (Gold_Std == FALSE & model == 1) {
                  sim2 = c(T, L, sd_a, b, TP_overall, (1 - FP_overall))
                  write(sim2, file = file.TV2, ncolumns = 6)
                  names(sim2) = c("THETA", "LAMBDA", "sigma alpha", 
                    "beta", "Overal ++", "Overall --")
                  sim_rs = rbind(s2, c2)
                  write.table(sim_rs, file = file.TV3, col.names = FALSE, 
                    row.names = FALSE)
                  sim_rs_label = numeric()
                  for (i in 1:n_rs) {
                    sim_rs_label = c(sim_rs_label, i)
                  }
                  colnames(sim_rs) = sim_rs_label
                }
            }
        }
    }
    if (SCO == FALSE) {
        write(paste("______________________________________________________"), 
            file = file.TV.index, append = TRUE)
        write(paste("\t   True_values.txt "), file = file.TV.index, 
            append = TRUE)
        write(paste("Column 1 : alpha parameters for all M = ", 
            N, " study(ies)\t    "), file = file.TV.index, append = TRUE)
        write(paste("Column 2 : theta parameters for all M = ", 
            N, " study(ies)\t    "), file = file.TV.index, append = TRUE)
        write(paste("Column 3 : sensitivity of test under evaluation (S1) for all M = ", 
            N, " study(ies)\t    "), file = file.TV.index, append = TRUE)
        write(paste("Column 4 : specificity of test under evaluation (C1) for all M = ", 
            N, " study(ies)\t    "), file = file.TV.index, append = TRUE)
        write(paste("Column 5 : prevalence for all M = ", N, 
            " study(ies)\t    "), file = file.TV.index, append = TRUE)
        write(paste("Column 6 : Observed cell with both test under evaluation and reference standard positive\t    "), 
            file = file.TV.index, append = TRUE)
        write(paste("Column 7 : Observed cell with test under evaluation positive and reference standard negative\t    "), 
            file = file.TV.index, append = TRUE)
        write(paste("Column 8 : Observed cell with test under evaluation negative and reference standard positive\t    "), 
            file = file.TV.index, append = TRUE)
        write(paste("Column 9 : Observed cell with both test under evaluation and reference standard negative\t    "), 
            file = file.TV.index, append = TRUE)
        write(paste("______________________________________________________"), 
            file = file.TV.index, append = TRUE)
        write(paste("\t   True_values2.txt "), file = file.TV.index, 
            append = TRUE)
        write(paste("Column 1 : THETA parameter\t    "), file = file.TV.index, 
            append = TRUE)
        write(paste("Column 2 : sigma theta parameter\t    "), 
            file = file.TV.index, append = TRUE)
        write(paste("Column 3 : LAMBDA parameter\t    "), file = file.TV.index, 
            append = TRUE)
        write(paste("Column 4 : sigma alpha parameter\t    "), 
            file = file.TV.index, append = TRUE)
        write(paste("Column 5 : beta parameter\t    "), file = file.TV.index, 
            append = TRUE)
        if (Gold_Std == TRUE) {
            write(paste("Column 6 : Overall sensitivity of test under evaluation (S1 overall) \t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 7 : Overall specificity of test under evaluation (C1 overall) \t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("______________________________________________________"), 
                file = file.TV.index, append = TRUE)
        }
        else {
            if (Gold_Std == FALSE & model == 1) {
                write(paste("Column 6 : Overall sensitivity of test under evaluation (S1 overall) \t    "), 
                  file = file.TV.index, append = TRUE)
                write(paste("Column 7 : Overall specificity of test under evaluation (C1 overall) \t    "), 
                  file = file.TV.index, append = TRUE)
                write(paste("______________________________________________________"), 
                  file = file.TV.index, append = TRUE)
                write(paste("\t   True_REFSTD.txt "), file = file.TV.index, 
                  append = TRUE)
                write(paste("Row 1 : sensitivity of reference standard (S2) \t    "), 
                  file = file.TV.index, append = TRUE)
                write(paste("Row 2 : specificity of reference standard (C2) \t    "), 
                  file = file.TV.index, append = TRUE)
            }
        }
    }
    else {
        if (SCO == TRUE) {
            write(paste("______________________________________________________"), 
                file = file.TV.index, append = TRUE)
            write(paste("\t   True_values.txt "), file = file.TV.index, 
                append = TRUE)
            write(paste("Column 1 : alpha parameters for all M = ", 
                N, " study(ies)\t    "), file = file.TV.index, 
                append = TRUE)
            write(paste("Column 2 : sensitivity of test under evaluation (S1) for all M = ", 
                N, " study(ies)\t    "), file = file.TV.index, 
                append = TRUE)
            write(paste("Column 3 : specificity of test under evaluation (C1) for all M = ", 
                N, " study(ies)\t    "), file = file.TV.index, 
                append = TRUE)
            write(paste("Column 4 : prevalence for all M = ", 
                N, " study(ies)\t    "), file = file.TV.index, 
                append = TRUE)
            write(paste("Column 5 : Observed cell with both test under evaluation and reference standard positive\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 6 : Observed cell with test under evaluation positive and reference standard negative\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 7 : Observed cell with test under evaluation negative and reference standard positive\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 8 : Observed cell with both test under evaluation and reference standard negative\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("______________________________________________________"), 
                file = file.TV.index, append = TRUE)
            write(paste("SAME CUT-OFF USED OVER ALL STUDIES"), 
                file = file.TV.index, append = TRUE)
            write(paste("______________________________________________________"), 
                file = file.TV.index, append = TRUE)
            write(paste("\t   True_values2.txt "), file = file.TV.index, 
                append = TRUE)
            write(paste("Column 1 : THETA parameter\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 2 : LAMBDA parameter\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 3 : sigma alpha parameter\t    "), 
                file = file.TV.index, append = TRUE)
            write(paste("Column 4 : beta parameter\t    "), file = file.TV.index, 
                append = TRUE)
            if (Gold_Std == TRUE) {
                write(paste("Column 5 : Overall sensitivity of test under evaluation (S1 overall) \t    "), 
                  file = file.TV.index, append = TRUE)
                write(paste("Column 6 : Overall specificity of test under evaluation (C1 overall) \t    "), 
                  file = file.TV.index, append = TRUE)
                write(paste("______________________________________________________"), 
                  file = file.TV.index, append = TRUE)
            }
            else {
                if (Gold_Std == FALSE & model == 1) {
                  write(paste("Column 5 : Overall sensitivity of test under evaluation (S1 overall) \t    "), 
                    file = file.TV.index, append = TRUE)
                  write(paste("Column 6 : Overall specificity of test under evaluation (C1 overall) \t    "), 
                    file = file.TV.index, append = TRUE)
                  write(paste("______________________________________________________"), 
                    file = file.TV.index, append = TRUE)
                  write(paste("\t   True_REFSTD.txt "), file = file.TV.index, 
                    append = TRUE)
                  write(paste("Row 1 : sensitivity of reference standard (S2) \t    "), 
                    file = file.TV.index, append = TRUE)
                  write(paste("Row 2 : specificity of reference standard (C2) \t    "), 
                    file = file.TV.index, append = TRUE)
                }
            }
        }
    }
    if (N == 1) {
        ssim1 = rbind(sim1[, 1:5])
        colnames(ssim1) = c("alpha", "theta", "++", "--", "prev")
    }
    else {
        ssim1 = sim1[, 1:5]
        colnames(ssim1) = c("alpha", "theta", "++", "--", "prev")
    }
    sim.results = list(data.sim, ssim1, sim2, sim_rs)
    names(sim.results) = c("Data", "Whithin study parameters", 
        "Between study parameters", "Reference standard")
    return(sim.results)
}
