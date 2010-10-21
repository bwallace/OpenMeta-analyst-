.invcalc <-
function (X, W, k) 
{
    wX <- sqrt(W) %*% X
    res.qrs <- qr.solve(wX, diag(k))
    tcrossprod(res.qrs)
}
.onLoad <-
function (lib, pkg) 
{
    loadmsg <- "Loading 'metafor' package (version 0.5-7). For an overview \nand introduction to the package please type: help(metafor)."
    packageStartupMessage(loadmsg, domain = NULL, appendLF = TRUE)
}
.QE.func <-
function (tau2val, Y, vi, X, k, objective, verbose = FALSE) 
{
    wi <- 1/(vi + tau2val)
    W <- diag(wi)
    stXWX <- .invcalc(X = X, W = W, k = k)
    P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
    RSS <- crossprod(Y, P) %*% Y
    if (verbose) 
        print(c(RSS - objective))
    RSS - objective
}
.tr <-
function (X) 
sum(diag(X))
addpoly <-
function (x, ...) 
UseMethod("addpoly")
addpoly.default <-
function (x, vi, sei, row = -1, level = 95, digits = 2, annotate = TRUE, 
    mlab = NULL, transf = FALSE, atransf = FALSE, targs = NULL, 
    col = "black", efac = 1, cex = NULL, ...) 
{
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    yi <- x
    if (missing(vi)) 
        vi <- sei^2
    yivi.na <- is.na(cbind(yi, vi))
    if (any(yivi.na)) {
        not.na <- apply(yivi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit") {
            yi <- yi[not.na]
            vi <- vi[not.na]
            mlab <- mlab[not.na]
        }
        if (na.act == "na.fail") 
            stop("Missing values in results.")
    }
    k <- length(yi)
    alpha <- (100 - level)/100
    ci.lb <- yi - qnorm(1 - alpha/2) * sqrt(vi)
    ci.ub <- yi + qnorm(1 - alpha/2) * sqrt(vi)
    yi.ut <- yi
    ci.lb.ut <- ci.lb
    ci.ub.ut <- ci.ub
    if (is.function(transf)) {
        if (is.null(targs)) {
            yi <- sapply(yi, transf)
            ci.lb <- sapply(ci.lb, transf)
            ci.ub <- sapply(ci.ub, transf)
        }
        else {
            yi <- sapply(yi, transf, targs)
            ci.lb <- sapply(ci.lb, transf, targs)
            ci.ub <- sapply(ci.ub, transf, targs)
        }
    }
    par.usr <- par("usr")
    height <- par.usr[4] - par.usr[3]
    cex.adj <- min(1, 20/height)
    xlim <- par.usr[1:2]
    if (is.null(cex)) 
        cex <- par("cex") * cex.adj
    if (length(row) == 1L) 
        row <- row:(row - k + 1)
    for (i in 1:k) {
        polygon(x = c(ci.lb[i], yi[i], ci.ub[i], yi[i]), y = c(row[i], 
            row[i] + (height/100) * cex * efac, row[i], row[i] - 
                (height/100) * cex * efac), col = col, ...)
        if (annotate) {
            if (is.function(atransf)) {
                if (is.null(targs)) {
                  text(x = xlim[2], row[i], labels = paste(formatC(sapply(yi.ut[i], 
                    atransf), digits = digits, format = "f", 
                    flag = " "), "[", formatC(sapply(ci.lb.ut[i], 
                    atransf), digits = digits, format = "f", 
                    flag = " "), ",", formatC(sapply(ci.ub.ut[i], 
                    atransf), digits = digits, format = "f", 
                    flag = " "), "]"), pos = 2, cex = cex, ...)
                }
                else {
                  text(x = xlim[2], row[i], labels = paste(formatC(sapply(yi.ut[i], 
                    atransf, targs), digits = digits, format = "f", 
                    flag = " "), "[", formatC(sapply(ci.lb.ut[i], 
                    atransf, targs), digits = digits, format = "f", 
                    flag = " "), ",", formatC(sapply(ci.ub.ut[i], 
                    atransf, targs), digits = digits, format = "f", 
                    flag = " "), "]"), pos = 2, cex = cex, ...)
                }
            }
            else {
                text(x = xlim[2], row[i], labels = paste(formatC(yi[i], 
                  digits = digits, format = "f", flag = " "), 
                  "[", formatC(ci.lb[i], digits = digits, format = "f", 
                    flag = " "), ",", formatC(ci.ub[i], digits = digits, 
                    format = "f", flag = " "), "]"), pos = 2, 
                  cex = cex)
            }
        }
        if (!is.null(mlab)) {
            text(xlim[1], row[i], mlab[i], pos = 4, cex = cex, 
                ...)
        }
    }
}
addpoly.rma <-
function (x, row = -2, level = x$level, digits = 2, annotate = TRUE, 
    mlab = NULL, transf = FALSE, atransf = FALSE, targs = NULL, 
    col = "black", efac = 1, cex = NULL, ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    if (!x$int.only) 
        stop("The model should not contain moderators.")
    if (is.null(mlab)) 
        mlab <- ifelse((x$method == "FE"), "FE Model", "RE Model")
    addpoly(x$b, vi = x$vb, row = row, level = level, digits = digits, 
        annotate = annotate, mlab = mlab, transf = transf, atransf = atransf, 
        col = col, targs = targs, efac = efac, cex = cex, ...)
}
anova.rma.uni <-
function (object, object2, digits = object$digits, ...) 
{
    if (!is.element("rma.uni", class(object))) 
        stop("Argument 'object' must be an object of class \"rma.uni\".")
    if (!is.element("rma.uni", class(object2))) 
        stop("Argument 'object2' must be an object of class \"rma.uni\".")
    m.f <- object
    m.r <- object2
    if (!(identical(m.f$yi, m.r$yi) && identical(m.f$vi, m.r$vi))) 
        stop("Observed outcomes and/or sampling variances not equal in the full and reduced model.")
    if (m.f$method == "FE") {
        p.f <- m.f$p
    }
    else {
        p.f <- m.f$p + 1
    }
    if (m.r$method == "FE") {
        p.r <- m.r$p
    }
    else {
        p.r <- m.r$p + 1
    }
    if (p.f == p.r) 
        stop("Models have the same number of parameters. LRT not meaningful.")
    if (p.f < p.r) {
        m.f <- object2
        m.r <- object
        p.s <- p.f
        p.f <- p.r
        p.r <- p.s
    }
    if (m.f$method == "FE" && m.r$method != "FE") 
        stop("Full model uses a fixed- and reduced model uses random/mixed-effects model.")
    p.diff <- p.f - p.r
    if (m.f$method == "REML") {
        LRT <- abs(m.r$fit.stats$REML[2] - m.f$fit.stats$REML[2])
        fit.stats.f <- m.f$fit.stats$REML
        fit.stats.r <- m.r$fit.stats$REML
        if (!identical(m.f$X, m.r$X)) 
            warning("Models with different fixed effects. REML comparisons are not meaningful.")
    }
    else {
        LRT <- abs(m.r$fit.stats$ML[2] - m.f$fit.stats$ML[2])
        fit.stats.f <- m.f$fit.stats$ML
        fit.stats.r <- m.r$fit.stats$ML
    }
    pval <- pchisq(LRT, df = p.diff, lower.tail = FALSE)
    if (m.f$method == "FE" || identical(m.r$tau2, 0)) {
        VAF <- NA
    }
    else {
        VAF <- round(100 * max(0, (m.r$tau2 - m.f$tau2)/m.r$tau2), 
            2)
    }
    res <- list(fit.stats.f, fit.stats.r, p.f, p.r, LRT, pval, 
        m.f$QE, m.r$QE, m.f$tau2, m.r$tau2, VAF, m.f$method, 
        digits)
    names(res) <- c("fit.stats.f", "fit.stats.r", "p.f", "p.r", 
        "LRT", "pval", "QE.f", "QE.r", "tau2.f", "tau2.r", "VAF", 
        "method", "digits")
    class(res) <- c("anova.rma.uni")
    return(res)
}
blup <-
function (x, ...) 
UseMethod("blup")
blup.rma.uni <-
function (x, level = x$level, digits = x$digits, transf = FALSE, 
    targs = NULL, ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    alpha <- (100 - level)/100
    if (!x$knha) {
        crit <- qnorm(1 - alpha/2)
    }
    else {
        crit <- qt(1 - alpha/2, df = x$k - x$p)
    }
    pred <- rep(NA, x$k.f)
    vpred <- rep(NA, x$k.f)
    li <- x$tau2/(x$tau2 + x$vi.f)
    for (i in (1:x$k.f)[x$not.na]) {
        Xi <- matrix(x$X.f[i, ], nrow = 1)
        pred[i] <- li[i] * x$yi.f[i] + (1 - li[i]) * Xi %*% x$b
        vpred[i] <- li[i] * x$vi.f[i] + (1 - li[i])^2 * Xi %*% 
            tcrossprod(x$vb, Xi)
    }
    se <- sqrt(vpred)
    pi.lb <- pred - crit * se
    pi.ub <- pred + crit * se
    if (is.function(transf)) {
        if (is.null(targs)) {
            pred <- sapply(pred, transf)
            se <- rep(NA, x$k.f)
            pi.lb <- sapply(pi.lb, transf)
            pi.ub <- sapply(pi.ub, transf)
        }
        else {
            pred <- sapply(pred, transf, targs)
            se <- rep(NA, x$k.f)
            pi.lb <- sapply(pi.lb, transf, targs)
            pi.ub <- sapply(pi.ub, transf, targs)
        }
    }
    if (na.act == "na.omit") {
        out <- list(pred = pred[x$not.na], se = se[x$not.na], 
            pi.lb = pi.lb[x$not.na], pi.ub = pi.ub[x$not.na])
        out$slab <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(pred = pred, se = se, pi.lb = pi.lb, pi.ub = pi.ub)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
cint <-
function (object, ...) 
UseMethod("cint")
cint.rma.uni <-
function (object, fixed = FALSE, random = TRUE, level = object$level, 
    digits = object$digits, control = list(), ...) 
{
    if (!is.element("rma.uni", class(object))) 
        stop("Argument 'object' must be an object of class \"rma.uni\".")
    x <- object
    if (random) {
        alpha <- (100 - level)/100
        crit.u <- qchisq(1 - alpha/2, x$k - x$p)
        crit.l <- qchisq(alpha/2, x$k - x$p)
        con <- list(tol = .Machine$double.eps^0.25, maxiter = 1000, 
            tau2.min = x$control$tau2.min, tau2.max = 50, verbose = FALSE)
        con[pmatch(names(control), names(con))] <- control
        status.lb <- 1
        status.ub <- 1
        conv <- 1
        if (.QE.func(con$tau2.min, Y = cbind(x$yi), vi = x$vi, 
            X = x$X, k = x$k, objective = 0, verbose = FALSE) < 
            crit.l) {
            tau2.lb <- NA
            tau2.ub <- NA
        }
        else {
            if (.QE.func(con$tau2.min, Y = cbind(x$yi), vi = x$vi, 
                X = x$X, k = x$k, objective = 0, verbose = FALSE) > 
                crit.u) {
                tau2.lb <- try(uniroot(.QE.func, interval = c(con$tau2.min, 
                  con$tau2.max), tol = con$tol, maxiter = con$maxiter, 
                  Y = cbind(x$yi), vi = x$vi, X = x$X, k = x$k, 
                  objective = crit.u, verbose = con$verbose)$root, 
                  silent = TRUE)
                if (!is.numeric(tau2.lb)) {
                  tau2.lb <- NA
                  status.lb <- 0
                  conv <- 0
                }
            }
            else {
                tau2.lb <- con$tau2.min
            }
            tau2.ub <- try(uniroot(.QE.func, interval = c(tau2.lb, 
                con$tau2.max), tol = con$tol, maxiter = con$maxiter, 
                Y = cbind(x$yi), vi = x$vi, X = x$X, k = x$k, 
                objective = crit.l, verbose = con$verbose)$root, 
                silent = TRUE)
            if (is.numeric(tau2.ub) == FALSE) {
                tau2.ub <- NA
                status.ub <- 0
                conv <- 0
            }
        }
        if (status.lb == 0L) {
            warning("Error in iterative search for the lower bound.")
        }
        if (status.ub == 0L) {
            warning("Error in iterative search for the upper bound.")
        }
        if (conv == 0L) {
            stop("Try increasing tau2.max (via the 'control' argument).")
        }
        if (x$int.only) {
            wi <- 1/x$vi
            s2 <- (x$k - 1) * sum(wi)/(sum(wi)^2 - sum(wi^2))
            I2.lb <- tau2.lb/(tau2.lb + s2) * 100
            I2.ub <- tau2.ub/(tau2.ub + s2) * 100
            H2.lb <- tau2.lb/s2 + 1
            H2.ub <- tau2.ub/s2 + 1
        }
        else {
            I2.lb <- NA
            I2.ub <- NA
            H2.lb <- NA
            H2.ub <- NA
        }
        if (is.na(tau2.lb) && is.na(tau2.lb)) {
            cat("The upper and lower bound both fall below ", 
                con$tau2.min, ".\nThe CI is therefore equal to the null set.\n\n", 
                sep = "")
        }
        tau2 <- round(c(x$tau2, tau2.lb, tau2.ub), digits)
        tau <- round(sqrt(c(ifelse(x$tau2 >= 0, x$tau2, NA), 
            ifelse(tau2.lb >= 0, tau2.lb, NA), ifelse(tau2.ub >= 
                0, tau2.ub, NA))), digits)
        I2 <- round(c(x$I2, I2.lb, I2.ub), digits)
        H2 <- round(c(x$H2, H2.lb, H2.ub), digits)
        if (x$int.only) {
            res.random <- rbind(tau2, tau, I2, H2)
            dimnames(res.random)[[1]] <- c("tau^2", "tau", "I^2(%)", 
                "H^2")
        }
        else {
            res.random <- rbind(tau2, tau)
            dimnames(res.random)[[1]] <- c("tau^2", "tau")
        }
        if (x$method == "FE") 
            res.random[, 1] <- NA
        dimnames(res.random)[[2]] <- c("estimate", "ci.lb", "ci.ub")
    }
    if (fixed) {
        alpha <- (100 - level)/100
        if (x$knha) {
            crit <- qt(1 - alpha/2, df = x$k - x$p)
        }
        else {
            crit <- qnorm(1 - alpha/2)
        }
        ci.lb <- c(x$b - crit * x$se)
        ci.ub <- c(x$b + crit * x$se)
        res.fixed <- round(cbind(x$b, ci.lb, ci.ub), digits)
        dimnames(res.fixed)[[2]] <- c("estimate", "ci.lb", "ci.ub")
    }
    if (fixed && random) {
        res <- list(fixed = data.frame(res.fixed), random = data.frame(res.random))
        return(res)
    }
    if (fixed) 
        return(data.frame(res.fixed))
    if (random) 
        return(data.frame(res.random))
}
coef.rma <-
function (object, ...) 
{
    if (!is.element("rma", class(object))) 
        stop("Argument 'object' must be an object of class \"rma\".")
    x <- object
    res.table <- cbind(x$b, x$se, x$zval, x$pval, x$ci.lb, x$ci.ub)
    dimnames(res.table)[[2]] <- c("estimate", "se", "zval", "pval", 
        "ci.lb", "ci.ub")
    if (is.element("rma.uni", class(x)) && x$knha) {
        dimnames(res.table)[[2]][3] <- c("tval")
    }
    res.table <- data.frame(res.table)
    return(res.table)
}
cumul <-
function (x, ...) 
UseMethod("cumul")
cumul.rma.mh <-
function (x, order = NULL, digits = x$digits, transf = FALSE, 
    ...) 
{
    if (!is.element("rma.mh", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.mh\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (is.null(order)) 
        order <- 1:x$k.f
    ai.f <- x$ai.f[order]
    bi.f <- x$bi.f[order]
    ci.f <- x$ci.f[order]
    di.f <- x$di.f[order]
    yi.f <- x$yi.f[order]
    vi.f <- x$vi.f[order]
    not.na <- x$not.na[order]
    slab <- x$slab[order]
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    b <- rep(NA, x$k.f)
    se <- rep(NA, x$k.f)
    zval <- rep(NA, x$k.f)
    pval <- rep(NA, x$k.f)
    ci.lb <- rep(NA, x$k.f)
    ci.ub <- rep(NA, x$k.f)
    QE <- rep(NA, x$k.f)
    QEp <- rep(NA, x$k.f)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma.mh(ai = ai.f[1:i], bi = bi.f[1:i], ci = ci.f[1:i], 
            di = di.f[1:i], measure = x$measure, add = x$add, 
            to = x$to, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        b[i] <- res$b
        se[i] <- res$se
        zval[i] <- res$zval
        pval[i] <- res$pval
        ci.lb[i] <- res$ci.lb
        ci.ub[i] <- res$ci.ub
        QE[i] <- res$QE
        QEp[i] <- res$QEp
    }
    alpha <- (100 - x$level)/100
    crit <- qnorm(1 - alpha/2)
    b[1] <- yi.f[1]
    se[1] <- sqrt(vi.f[1])
    zval[1] <- yi.f[1]/se[1]
    pval[1] <- 2 * pnorm(abs(zval[1]), lower.tail = FALSE)
    ci.lb[1] <- yi.f[1] - crit * se[1]
    ci.ub[1] <- yi.f[1] + crit * se[1]
    QE[1] <- 0
    QEp[1] <- 1
    if (transf) {
        if (x$measure == "OR" || x$measure == "RR") {
            b <- exp(b)
            se <- rep(NA, x$k.f)
            ci.lb <- exp(ci.lb)
            ci.ub <- exp(ci.ub)
        }
    }
    if (na.act == "na.omit") {
        out <- list(estimate = b[not.na], se = se[not.na], zval = zval[not.na], 
            pval = pval[not.na], ci.lb = ci.lb[not.na], ci.ub = ci.ub[not.na], 
            Q = QE[not.na], Qp = QEp[not.na])
        out$slab <- slab[not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(estimate = b, se = se, zval = zval, pval = pval, 
            ci.lb = ci.lb, ci.ub = ci.ub, Q = QE, Qp = QEp)
        out$slab <- slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    out$slab.null <- x$slab.null
    out$level <- x$level
    class(out) <- c("list.rma", "cumul.rma")
    return(out)
}
cumul.rma.peto <-
function (x, order = NULL, digits = x$digits, transf = FALSE, 
    ...) 
{
    if (!is.element("rma.peto", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.peto\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (is.null(order)) 
        order <- 1:x$k.f
    ai.f <- x$ai.f[order]
    bi.f <- x$bi.f[order]
    ci.f <- x$ci.f[order]
    di.f <- x$di.f[order]
    yi.f <- x$yi.f[order]
    vi.f <- x$vi.f[order]
    not.na <- x$not.na[order]
    slab <- x$slab[order]
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    b <- rep(NA, x$k.f)
    se <- rep(NA, x$k.f)
    zval <- rep(NA, x$k.f)
    pval <- rep(NA, x$k.f)
    ci.lb <- rep(NA, x$k.f)
    ci.ub <- rep(NA, x$k.f)
    QE <- rep(NA, x$k.f)
    QEp <- rep(NA, x$k.f)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma.peto(ai = ai.f[1:i], bi = bi.f[1:i], ci = ci.f[1:i], 
            di = di.f[1:i], add = x$add, to = x$to, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        b[i] <- res$b
        se[i] <- res$se
        zval[i] <- res$zval
        pval[i] <- res$pval
        ci.lb[i] <- res$ci.lb
        ci.ub[i] <- res$ci.ub
        QE[i] <- res$QE
        QEp[i] <- res$QEp
    }
    alpha <- (100 - x$level)/100
    crit <- qnorm(1 - alpha/2)
    b[1] <- yi.f[1]
    se[1] <- sqrt(vi.f[1])
    zval[1] <- yi.f[1]/se[1]
    pval[1] <- 2 * pnorm(abs(zval[1]), lower.tail = FALSE)
    ci.lb[1] <- yi.f[1] - crit * se[1]
    ci.ub[1] <- yi.f[1] + crit * se[1]
    QE[1] <- 0
    QEp[1] <- 1
    if (transf) {
        b <- exp(b)
        se <- rep(NA, x$k.f)
        ci.lb <- exp(ci.lb)
        ci.ub <- exp(ci.ub)
    }
    if (na.act == "na.omit") {
        out <- list(estimate = b[not.na], se = se[not.na], zval = zval[not.na], 
            pval = pval[not.na], ci.lb = ci.lb[not.na], ci.ub = ci.ub[not.na], 
            Q = QE[not.na], Qp = QEp[not.na])
        out$slab <- slab[not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(estimate = b, se = se, zval = zval, pval = pval, 
            ci.lb = ci.lb, ci.ub = ci.ub, Q = QE, Qp = QEp)
        out$slab <- slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    out$slab.null <- x$slab.null
    out$level <- x$level
    class(out) <- c("list.rma", "cumul.rma")
    return(out)
}
cumul.rma.uni <-
function (x, order = NULL, digits = x$digits, transf = FALSE, 
    targs = NULL, ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (!x$int.only) 
        stop("Method only applicable for models without moderators.")
    if (is.null(order)) 
        order <- 1:x$k.f
    yi.f <- x$yi.f[order]
    vi.f <- x$vi.f[order]
    X.f <- cbind(x$X.f[order, ])
    not.na <- x$not.na[order]
    slab <- x$slab[order]
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    b <- rep(NA, x$k.f)
    se <- rep(NA, x$k.f)
    zval <- rep(NA, x$k.f)
    pval <- rep(NA, x$k.f)
    ci.lb <- rep(NA, x$k.f)
    ci.ub <- rep(NA, x$k.f)
    QE <- rep(NA, x$k.f)
    QEp <- rep(NA, x$k.f)
    tau2 <- rep(NA, x$k.f)
    I2 <- rep(NA, x$k.f)
    H2 <- rep(NA, x$k.f)
    for (i in (1:x$k.f)[not.na]) {
        res <- try(rma(yi.f[1:i], vi.f[1:i], method = x$method, 
            weighted = x$weighted, intercept = TRUE, knha = x$knha, 
            control = x$control, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        b[i] <- res$b
        se[i] <- res$se
        zval[i] <- res$zval
        pval[i] <- res$pval
        ci.lb[i] <- res$ci.lb
        ci.ub[i] <- res$ci.ub
        QE[i] <- res$QE
        QEp[i] <- res$QEp
        tau2[i] <- res$tau2
        I2[i] <- res$I2
        H2[i] <- res$H2
    }
    alpha <- (100 - x$level)/100
    crit <- qnorm(1 - alpha/2)
    b[1] <- yi.f[1]
    se[1] <- sqrt(vi.f[1])
    zval[1] <- yi.f[1]/se[1]
    pval[1] <- 2 * pnorm(abs(zval[1]), lower.tail = FALSE)
    ci.lb[1] <- yi.f[1] - crit * se[1]
    ci.ub[1] <- yi.f[1] + crit * se[1]
    QE[1] <- 0
    QEp[1] <- 1
    tau2[1] <- 0
    I2[1] <- 0
    H2[1] <- 1
    if (is.function(transf)) {
        if (is.null(targs)) {
            b <- sapply(b, transf)
            se <- rep(NA, x$k.f)
            ci.lb <- sapply(ci.lb, transf)
            ci.ub <- sapply(ci.ub, transf)
        }
        else {
            b <- sapply(b, transf, targs)
            se <- rep(NA, x$k.f)
            ci.lb <- sapply(ci.lb, transf, targs)
            ci.ub <- sapply(ci.ub, transf, targs)
        }
    }
    if (na.act == "na.omit") {
        out <- list(estimate = b[not.na], se = se[not.na], zval = zval[not.na], 
            pvals = pval[not.na], ci.lb = ci.lb[not.na], ci.ub = ci.ub[not.na], 
            tau2 = tau2[not.na], QE = QE[not.na], Qp = QEp[not.na], 
            I2 = I2[not.na], H2 = H2[not.na])
        out$slab <- slab[not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(estimate = b, se = se, zval = zval, pvals = pval, 
            ci.lb = ci.lb, ci.ub = ci.ub, tau2 = tau2, QE = QE, 
            Qp = QEp, I2 = I2, H2 = H2)
        out$slab <- slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    if (x$method == "FE") 
        out <- out[-c(9, 10, 11)]
    out$digits <- digits
    out$slab.null <- x$slab.null
    out$level <- x$level
    class(out) <- c("list.rma", "cumul.rma")
    return(out)
}
escalc <-
function (measure, ai, bi, ci, di, n1i, n2i, m1i, m2i, sd1i, 
    sd2i, xi, mi, ri, ni, data = NULL, add = 1/2, to = "only0", 
    vtype = "LS") 
{
    if (!is.element(measure, c("MD", "SMD", "RR", "OR", "PETO", 
        "RD", "AS", "PHI", "YUQ", "YUY", "PR", "PLN", "PLO", 
        "PAS", "PFT", "COR", "UCOR", "ZCOR"))) 
        stop("Unknown 'measure' specified.")
    if (!is.element(to, c("all", "only0", "if0all", "none"))) 
        stop("Unknown 'to' argument specified.")
    if (!is.element(vtype, c("UB", "LS", "HS"))) 
        stop("Unknown 'vtype' argument specified.")
    if (is.null(data)) {
        data <- sys.frame(sys.parent())
    }
    else {
        if (!is.data.frame(data)) {
            data <- data.frame(data)
        }
    }
    mf <- match.call()
    if (is.element(measure, c("MD", "SMD"))) {
        mf.m1i <- mf[[match("m1i", names(mf))]]
        mf.m2i <- mf[[match("m2i", names(mf))]]
        mf.sd1i <- mf[[match("sd1i", names(mf))]]
        mf.sd2i <- mf[[match("sd2i", names(mf))]]
        mf.n1i <- mf[[match("n1i", names(mf))]]
        mf.n2i <- mf[[match("n2i", names(mf))]]
        m1i <- eval(mf.m1i, data)
        m2i <- eval(mf.m2i, data)
        sd1i <- eval(mf.sd1i, data)
        sd2i <- eval(mf.sd2i, data)
        n1i <- eval(mf.n1i, data)
        n2i <- eval(mf.n2i, data)
        if (measure == "MD") {
            yi <- m1i - m2i
            vi <- sd1i^2/n1i + sd2i^2/n2i
        }
        if (measure == "SMD") {
            cNm2ifunc <- function(Nm2i) {
                cNm2i <- gamma(Nm2i/2)/(sqrt(Nm2i/2) * gamma((Nm2i - 
                  1)/2))
                isna <- is.na(cNm2i)
                cNm2i[isna] <- 1 - 3/(4 * Nm2i[isna] - 1)
                cNm2i
            }
            Nm2i <- n1i + n2i - 2
            warn.before <- getOption("warn")
            options(warn = -1)
            cNm2i <- cNm2ifunc(Nm2i)
            options(warn = warn.before)
            nti <- (n1i * n2i)/(n1i + n2i)
            yi <- cNm2i * (m1i - m2i)/sqrt(((n1i - 1) * sd1i^2 + 
                (n2i - 1) * sd2i^2)/Nm2i)
            if (vtype == "UB") {
                vi <- 1/nti + (1 - (Nm2i - 2)/(Nm2i * cNm2i^2)) * 
                  yi^2
            }
            if (vtype == "LS") {
                vi <- 1/nti + yi^2/(2 * (n1i + n2i))
            }
            if (vtype == "HS") {
                md <- sum((n1i + n2i) * yi)/sum(n1i + n2i)
                vi <- 1/nti + md^2/(2 * (n1i + n2i))
            }
        }
    }
    if (is.element(measure, c("RR", "OR", "RD", "AS", "PETO", 
        "PHI", "YUQ", "YUY"))) {
        mf.ai <- mf[[match("ai", names(mf))]]
        mf.bi <- mf[[match("bi", names(mf))]]
        mf.ci <- mf[[match("ci", names(mf))]]
        mf.di <- mf[[match("di", names(mf))]]
        mf.n1i <- mf[[match("n1i", names(mf))]]
        mf.n2i <- mf[[match("n2i", names(mf))]]
        ai <- eval(mf.ai, data)
        bi <- eval(mf.bi, data)
        ci <- eval(mf.ci, data)
        di <- eval(mf.di, data)
        n1i <- eval(mf.n1i, data)
        n2i <- eval(mf.n2i, data)
        if (is.null(bi)) {
            bi <- n1i - ai
        }
        if (is.null(di)) {
            di <- n2i - ci
        }
        if (to == "all") {
            ai <- ai + add
            ci <- ci + add
            bi <- bi + add
            di <- di + add
        }
        if (to == "only0") {
            id0 <- c(ai == 0L | ci == 0L | bi == 0L | di == 0L)
            id0[is.na(id0)] <- FALSE
            ai[id0] <- ai[id0] + add
            ci[id0] <- ci[id0] + add
            bi[id0] <- bi[id0] + add
            di[id0] <- di[id0] + add
        }
        if (to == "if0all") {
            id0 <- c(ai == 0L | ci == 0L | bi == 0L | di == 0L)
            id0[is.na(id0)] <- FALSE
            if (any(id0)) {
                ai <- ai + add
                ci <- ci + add
                bi <- bi + add
                di <- di + add
            }
        }
        n1i <- ai + bi
        n2i <- ci + di
        p1 <- ai/n1i
        p2 <- ci/n2i
        if (measure == "RR") {
            yi <- log(p1) - log(p2)
            vi <- 1/ai - 1/n1i + 1/ci - 1/n2i
        }
        if (measure == "OR") {
            yi <- log(p1/(1 - p1)) - log(p2/(1 - p2))
            vi <- 1/ai + 1/bi + 1/ci + 1/di
        }
        if (measure == "PETO") {
            xt <- ai + ci
            yt <- bi + di
            Ni <- ai + ci + bi + di
            Oi <- ai
            Ei <- xt * n1i/Ni
            Vi <- xt * yt * (n1i/Ni) * (n2i/Ni)/(Ni - 1)
            yi <- (ai - Ei)/Vi
            vi <- 1/Vi
        }
        if (measure == "RD") {
            yi <- p1 - p2
            vi <- p1 * (1 - p1)/n1i + p2 * (1 - p2)/n2i
        }
        if (measure == "AS") {
            yi <- asin(sqrt(p1)) - asin(sqrt(p2))
            vi <- 1/(4 * n1i) + 1/(4 * n2i)
        }
        if (measure == "PHI") {
            yi <- (ai * di - bi * ci)/sqrt((ai + bi) * (ci + 
                di) * (ai + ci) * (bi + di))
            Ni <- ai + ci + bi + di
            p1. <- (ai + bi)/Ni
            p2. <- (ci + di)/Ni
            p.1 <- (ai + ci)/Ni
            p.2 <- (bi + di)/Ni
            vi <- 1/Ni * (1 - yi^2 + yi * (1 + yi^2/2) * (p1. - 
                p2.) * (p.1 - p.2)/sqrt(p1. * p2. * p.1 * p.2) - 
                3/4 * yi^2 * ((p1. - p2.)^2/(p1. * p2.) + (p.1 - 
                  p.2)/(p.1 * p.2)))
        }
        if (measure == "YUQ") {
            ori <- ai * di/(bi * ci)
            yi <- (ori - 1)/(ori + 1)
            vi <- 1/4 * (1 - yi^2)^2 * (1/ai + 1/bi + 1/ci + 
                1/di)
        }
        if (measure == "YUY") {
            ori <- ai * di/(bi * ci)
            yi <- (sqrt(ori) - 1)/(sqrt(ori) + 1)
            vi <- 1/16 * (1 - yi^2)^2 * (1/ai + 1/bi + 1/ci + 
                1/di)
        }
    }
    if (is.element(measure, c("PR", "PLN", "PLO", "PAS", "PFT"))) {
        mf.xi <- mf[[match("xi", names(mf))]]
        mf.mi <- mf[[match("mi", names(mf))]]
        mf.ni <- mf[[match("ni", names(mf))]]
        xi <- eval(mf.xi, data)
        mi <- eval(mf.mi, data)
        ni <- eval(mf.ni, data)
        if (is.null(mi)) {
            mi <- ni - xi
        }
        if (to == "all") {
            xi <- xi + add
            mi <- mi + add
        }
        if (to == "only0") {
            id0 <- c(xi == 0L | mi == 0L)
            id0[is.na(id0)] <- FALSE
            xi[id0] <- xi[id0] + add
            mi[id0] <- mi[id0] + add
        }
        if (to == "if0all") {
            id0 <- c(xi == 0L | mi == 0L)
            id0[is.na(id0)] <- FALSE
            if (any(id0)) {
                xi <- xi + add
                mi <- mi + add
            }
        }
        ni <- xi + mi
        pri <- xi/ni
        if (measure == "PR") {
            yi <- pri
            vi <- pri * (1 - pri)/ni
        }
        if (measure == "PLN") {
            yi <- log(pri)
            vi <- 1/xi - 1/ni
        }
        if (measure == "PLO") {
            yi <- log(pri/(1 - pri))
            vi <- 1/xi + 1/mi
        }
        if (measure == "PAS") {
            yi <- asin(sqrt(pri))
            vi <- 1/(4 * ni)
        }
        if (measure == "PFT") {
            yi <- 1/2 * (asin(sqrt(xi/(ni + 1))) + asin(sqrt((xi + 
                1)/(ni + 1))))
            vi <- 1/(4 * ni + 2)
        }
    }
    if (is.element(measure, c("COR", "UCOR", "ZCOR"))) {
        mf.ri <- mf[[match("ri", names(mf))]]
        mf.ni <- mf[[match("ni", names(mf))]]
        ri <- eval(mf.ri, data)
        ni <- eval(mf.ni, data)
        if (measure == "COR") {
            yi <- ri
            if (vtype == "UB") {
                vi <- (ri + ri * (1 - ri^2)/(2 * (ni - 4)))^2 - 
                  1 + (ni - 3)/(ni - 2) * ((1 - ri^2) + 2 * (1 - 
                  ri^2)^2/ni + 8 * (1 - ri^2)^3/(ni * (ni + 2)) + 
                  48 * (1 - ri^2)^4/(ni * (ni + 2) * (ni + 4)))
            }
            if (vtype == "LS") {
                vi <- (1 - ri^2)^2/(ni - 1)
            }
            if (vtype == "HS") {
                mr <- sum(ni * ri)/sum(ni)
                vi <- (1 - mr^2)^2/(ni - 1)
            }
        }
        if (measure == "UCOR") {
            yi <- ri + ri * (1 - ri^2)/(2 * (ni - 4))
            if (vtype == "UB") {
                vi <- yi^2 - 1 + (ni - 3)/(ni - 2) * ((1 - ri^2) + 
                  2 * (1 - ri^2)^2/ni + 8 * (1 - ri^2)^3/(ni * 
                  (ni + 2)) + 48 * (1 - ri^2)^4/(ni * (ni + 2) * 
                  (ni + 4)))
            }
            if (vtype == "LS") {
                vi <- (1 - yi^2)^2/(ni - 1)
            }
            if (vtype == "HS") {
                mr <- sum(ni * yi)/sum(ni)
                vi <- (1 - mr^2)^2/(ni - 1)
            }
        }
        if (measure == "ZCOR") {
            yi <- 1/2 * log((1 + ri)/(1 - ri))
            vi <- 1/(ni - 3)
        }
    }
    if (any(is.infinite(c(yi, vi)))) {
        warning("Some yi and/or vi equal to +-Inf. Recoded to NAs.")
        k <- length(yi)
        is.inf <- (1:k)[is.infinite(yi) == TRUE | is.infinite(vi) == 
            TRUE]
        yi[is.inf] <- NA
        vi[is.inf] <- NA
    }
    dat <- data.frame(cbind(yi, vi))
    return(dat)
}
fitstats <-
function (x, ...) 
UseMethod("fitstats")
fitstats.rma <-
function (x, REML = NULL, ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    if (is.null(REML)) {
        if (x$method == "REML") {
            REML <- TRUE
        }
        else {
            REML <- FALSE
        }
    }
    if (REML) {
        out <- cbind(x$fit.stats$REML)
        dimnames(out)[[1]] <- c("Log-Likelihood: ", "Deviance (-2RLL): ", 
            "AIC: ", "BIC: ")
        dimnames(out)[[2]] <- c("REML")
    }
    else {
        out <- cbind(x$fit.stats$ML)
        dimnames(out)[[1]] <- c("Log-Likelihood: ", "Deviance (-2LL): ", 
            "AIC: ", "BIC: ")
        dimnames(out)[[2]] <- c("ML")
    }
    return(out)
}
fitted.rma <-
function (object, ...) 
{
    if (!is.element("rma", class(object))) 
        stop("Argument 'object' must be an object of class \"rma\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    out <- c(object$X.f %*% object$b)
    names(out) <- object$slab
    not.na <- !is.na(out)
    if (na.act == "na.omit") {
        out <- out[not.na]
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    return(out)
}
forest <-
function (x, ...) 
UseMethod("forest")
forest.cumul.rma <-
function (x, annotate = TRUE, xlim = NULL, alim = NULL, ylim = NULL, 
    at = NULL, steps = 5, level = x$level, digits = 2, refline = 0, 
    xlab = NULL, ilab = NULL, ilab.xpos = NULL, ilab.pos = NULL, 
    transf = FALSE, atransf = FALSE, targs = NULL, addrows = 0, 
    efac = 1, pch = 15, psize = 1, cex = NULL, cex.lab = NULL, 
    cex.axis = NULL, ...) 
{
    if (!is.element("cumul.rma", class(x))) 
        stop("Argument 'x' must be an object of class \"cumul.rma\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (x$slab.null) {
        slab <- paste("+ Study ", x$slab)
        slab[1] <- paste("Study ", x$slab[1])
    }
    else {
        slab <- paste("+", x$slab)
        slab[1] <- paste(x$slab[1])
    }
    forest.default(x = x$estimate, sei = x$se, annotate = annotate, 
        xlim = xlim, alim = alim, ylim = ylim, at = at, steps = steps, 
        level = level, digits = digits, refline = refline, xlab = xlab, 
        slab = slab, ilab = ilab, ilab.xpos = ilab.xpos, ilab.pos = ilab.pos, 
        transf = transf, atransf = atransf, targs = targs, addrows = addrows, 
        efac = efac, pch = pch, psize = psize, cex = cex, cex.lab = cex.lab, 
        cex.axis = cex.axis, ...)
}
forest.default <-
function (x, vi, sei, annotate = TRUE, xlim = NULL, alim = NULL, 
    ylim = NULL, at = NULL, steps = 5, level = 95, digits = 2, 
    refline = 0, xlab = NULL, slab = NULL, ilab = NULL, ilab.xpos = NULL, 
    ilab.pos = NULL, subset = NULL, transf = FALSE, atransf = FALSE, 
    targs = NULL, addrows = 0, efac = 1, pch = 15, psize = NULL, 
    cex = NULL, cex.lab = NULL, cex.axis = NULL, ...) 
{
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    yi <- x
    if (missing(vi)) 
        vi <- sei^2
    if (length(yi) != length(vi)) 
        stop("Length of yi and vi (or sei) is not the same.")
    k <- length(yi)
    if (is.null(slab)) 
        slab <- paste("Study ", 1:k)
    if (is.vector(ilab)) 
        ilab <- cbind(ilab)
    if (length(pch) == 1L) 
        pch <- rep(pch, k)
    if (is.null(psize)) {
        wi <- 1/vi
        wi[is.infinite(wi)] <- 2 * max(wi, na.rm = TRUE)
        psize <- wi/sum(wi, na.rm = TRUE)
        psize <- (psize - min(psize, na.rm = TRUE))/(max(psize, 
            na.rm = TRUE) - min(psize, na.rm = TRUE))
        psize <- (psize * 0.9) + 0.6
    }
    else {
        if (length(psize) == 1L) 
            psize <- rep(psize, k)
    }
    if (is.null(subset)) {
        subset <- k:1
    }
    yi <- yi[subset]
    vi <- vi[subset]
    slab <- slab[subset]
    psize <- psize[subset]
    pch <- pch[subset]
    ilab <- ilab[subset, , drop = FALSE]
    yivi.na <- is.na(cbind(yi, vi))
    if (any(yivi.na)) {
        not.na <- apply(yivi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit") {
            yi <- yi[not.na]
            vi <- vi[not.na]
            slab <- slab[not.na]
            psize <- psize[not.na]
            ilab <- ilab[not.na, , drop = FALSE]
        }
        if (na.act == "na.fail") 
            stop("Missing values in results.")
    }
    k <- length(yi)
    alpha <- (100 - level)/100
    ci.lb <- yi - qnorm(1 - alpha/2) * sqrt(vi)
    ci.ub <- yi + qnorm(1 - alpha/2) * sqrt(vi)
    if (is.function(transf)) {
        if (is.null(targs)) {
            yi <- sapply(yi, transf)
            ci.lb <- sapply(ci.lb, transf)
            ci.ub <- sapply(ci.ub, transf)
        }
        else {
            yi <- sapply(yi, transf, targs)
            ci.lb <- sapply(ci.lb, transf, targs)
            ci.ub <- sapply(ci.ub, transf, targs)
        }
    }
    rng <- max(ci.ub, na.rm = TRUE) - min(ci.lb, na.rm = TRUE)
    if (is.null(xlim)) {
        xlim <- c(min(ci.lb, na.rm = TRUE) - rng * 1.2, max(ci.ub, 
            na.rm = TRUE) + rng * 1.2)
        xlim <- round(xlim, digits)
    }
    if (is.null(alim)) {
        if (is.null(at)) {
            alim <- c(min(ci.lb, na.rm = TRUE) - rng * 0.2, max(ci.ub, 
                na.rm = TRUE) + rng * 0.2)
            alim <- round(alim, digits)
        }
        else {
            alim <- range(at)
        }
    }
    alim <- sort(alim)
    xlim <- sort(xlim)
    if (xlim[1] > min(yi, na.rm = TRUE)) {
        xlim[1] <- min(yi, na.rm = TRUE)
    }
    if (xlim[2] < max(yi, na.rm = TRUE)) {
        xlim[2] <- max(yi, na.rm = TRUE)
    }
    if (alim[1] > min(yi, na.rm = TRUE)) {
        alim[1] <- min(yi, na.rm = TRUE)
    }
    if (alim[2] < max(yi, na.rm = TRUE)) {
        alim[2] <- max(yi, na.rm = TRUE)
    }
    if (alim[1] < xlim[1]) {
        xlim[1] <- alim[1]
    }
    if (alim[2] > xlim[2]) {
        xlim[2] <- alim[2]
    }
    addrows <- round(addrows)
    addrows[addrows < 0] <- 0
    if (addrows > 0) 
        addrows <- addrows + 1
    if (is.null(ylim)) {
        ylim <- c(0.5 - addrows, k + 2)
    }
    else {
        ylim <- sort(ylim)
    }
    if (is.null(at)) {
        at <- seq(alim[1], alim[2], length = steps)
    }
    else {
        at[at < alim[1]] <- alim[1]
        at[at > alim[2]] <- alim[2]
        at <- unique(at)
    }
    at.lab <- at
    if (is.function(atransf)) {
        if (is.null(targs)) {
            at.lab <- formatC(sapply(at.lab, atransf), digits = digits, 
                format = "f")
        }
        else {
            at.lab <- formatC(sapply(at.lab, atransf, targs), 
                digits = digits, format = "f")
        }
    }
    else {
        at.lab <- round(at, digits)
    }
    par.mar <- par("mar")
    par.mar.adj <- par.mar - c(0, 3, 0, 1)
    par.mar.adj[par.mar.adj < 1] <- 1
    par(mar = par.mar.adj)
    on.exit(par(mar = par.mar))
    plot(NA, NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", 
        yaxt = "n", xaxt = "n", xaxs = "i", bty = "n", ...)
    abline(h = k + 1, ...)
    par.usr <- par("usr")
    height <- par.usr[4] - par.usr[3]
    lheight <- strheight("O")
    cex.adj <- ifelse(k * lheight > height * 0.8, height/(1.25 * 
        k * lheight), 1)
    if (is.null(cex)) {
        cex <- par("cex") * cex.adj
    }
    else {
        if (is.null(cex.lab)) 
            cex.lab <- cex
        if (is.null(cex.axis)) 
            cex.axis <- cex
    }
    if (is.null(cex.lab)) 
        cex.lab <- par("cex.lab") * cex.adj
    if (is.null(cex.axis)) 
        cex.axis <- par("cex.axis") * cex.adj
    axis(side = 1, at = at, labels = at.lab, cex.axis = cex.axis, 
        ...)
    if (!is.null(xlab)) 
        mtext(xlab, side = 1, at = min(at) + (max(at) - min(at))/2, 
            line = 2.75, cex = cex.lab, ...)
    for (i in 1:k) {
        if (is.na(yi[i]) || is.na(vi)[i]) 
            next
        segments(max(ci.lb[i], alim[1]), i, min(ci.ub[i], alim[2]), 
            i, ...)
        if (ci.lb[i] >= alim[1]) {
            segments(ci.lb[i], i - (k/100) * cex * efac, ci.lb[i], 
                i + (k/100) * cex * efac, ...)
        }
        else {
            polygon(x = c(alim[1], alim[1] + (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[1] + (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[1]), y = c(i, i + (k/100) * 
                cex * efac, i - (k/100) * cex * efac, i), col = "black", 
                ...)
        }
        if (ci.ub[i] <= alim[2]) {
            segments(ci.ub[i], i - (k/100) * cex * efac, ci.ub[i], 
                i + (k/100) * cex * efac, ...)
        }
        else {
            polygon(x = c(alim[2], alim[2] - (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[2] - (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[2]), y = c(i, i + (k/100) * 
                cex * efac, i - (k/100) * cex * efac, i), col = "black", 
                ...)
        }
    }
    text(xlim[1], 1:k, slab, pos = 4, cex = cex, ...)
    if (!is.null(ilab)) {
        for (l in 1:dim(ilab)[2]) {
            text(ilab.xpos[l], 1:k, ilab[, l], offset = 0, pos = ilab.pos[l], 
                cex = cex, ...)
        }
    }
    if (is.numeric(refline)) 
        segments(refline, ylim[1] - 1, refline, (k + 1), lty = "dotted", 
            ...)
    if (annotate) {
        if (is.function(atransf)) {
            if (is.null(targs)) {
                text(x = xlim[2], 1:k, labels = paste(formatC(sapply(yi, 
                  atransf), digits = digits, format = "f", flag = " "), 
                  "[", formatC(sapply(ci.lb, atransf), digits = digits, 
                    format = "f", flag = " "), ",", formatC(sapply(ci.ub, 
                    atransf), digits = digits, format = "f", 
                    flag = " "), "]"), pos = 2, cex = cex, ...)
            }
            else {
                text(x = xlim[2], 1:k, labels = paste(formatC(sapply(yi, 
                  atransf, targs), digits = digits, format = "f", 
                  flag = " "), "[", formatC(sapply(ci.lb, atransf, 
                  targs), digits = digits, format = "f", flag = " "), 
                  ",", formatC(sapply(ci.ub, atransf, targs), 
                    digits = digits, format = "f", flag = " "), 
                  "]"), pos = 2, cex = cex, ...)
            }
        }
        else {
            text(x = xlim[2], 1:k, labels = paste(formatC(yi, 
                digits = digits, format = "f", flag = " "), "[", 
                formatC(ci.lb, digits = digits, format = "f", 
                  flag = " "), ",", formatC(ci.ub, digits = digits, 
                  format = "f", flag = " "), "]"), pos = 2, cex = cex, 
                ...)
        }
    }
    points(yi, 1:k, pch = pch, cex = cex * psize, ...)
    if (addrows > 0) 
        abline(h = 0, ...)
    invisible()
}
forest.rma <-
function (x, annotate = TRUE, addfit = TRUE, xlim = NULL, alim = NULL, 
    ylim = NULL, at = NULL, steps = 5, level = x$level, digits = 2, 
    refline = 0, xlab = NULL, slab = NULL, mlab = NULL, ilab = NULL, 
    ilab.xpos = NULL, ilab.pos = NULL, order = NULL, transf = FALSE, 
    atransf = FALSE, targs = NULL, addrows = 0, efac = 1, pch = 15, 
    psize = NULL, col = "darkgray", border = "darkgray", cex = NULL, 
    cex.lab = NULL, cex.axis = NULL, ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    yi <- x$yi.f
    vi <- x$vi.f
    X <- x$X.f
    if (is.null(slab)) {
        if (x$slab.null) {
            slab <- paste("Study ", x$slab)
        }
        else {
            slab <- x$slab
        }
    }
    if (is.vector(ilab)) 
        ilab <- cbind(ilab)
    k <- length(yi)
    options(na.action = "na.exclude")
    if (x$int.only) {
        pred <- fitted(x)
        pred.ci.lb <- rep(NA, k)
        pred.ci.ub <- rep(NA, k)
    }
    else {
        temp <- predict(x, level = level)
        pred <- temp$pred
        pred.ci.lb <- temp$ci.lb
        pred.ci.ub <- temp$ci.ub
    }
    options(na.action = na.act)
    if (length(pch) == 1L) 
        pch <- rep(pch, k)
    if (is.null(psize)) {
        wi <- 1/vi
        wi[is.infinite(wi)] <- 2 * max(wi, na.rm = TRUE)
        psize <- wi/sum(wi, na.rm = TRUE)
        psize <- (psize - min(psize, na.rm = TRUE))/(max(psize, 
            na.rm = TRUE) - min(psize, na.rm = TRUE))
        psize <- (psize * 0.9) + 0.6
    }
    else {
        if (length(psize) == 1L) 
            psize <- rep(psize, k)
    }
    if (!is.null(order)) {
        sort.vec <- 1:k
        if (length(order) == k) {
            sort.vec <- order
        }
        else {
            if (order == "obs") 
                sort.vec <- order(yi)
            if (order == "fit") 
                sort.vec <- order(pred)
            if (order == "prec") 
                sort.vec <- order(vi, yi)
            if (order == "resid") 
                sort.vec <- order(yi - pred, yi)
            if (order == "rstandard") 
                sort.vec <- order(rstandard(x)$z, yi)
            if (order == "abs.resid") 
                sort.vec <- order(abs(yi - pred), yi)
            if (order == "abs.rstandard") 
                sort.vec <- order(abs(rstandard(x)$z), yi)
        }
    }
    else {
        sort.vec <- k:1
    }
    yi <- yi[sort.vec]
    vi <- vi[sort.vec]
    X <- X[sort.vec, , drop = FALSE]
    slab <- slab[sort.vec]
    pred <- pred[sort.vec]
    pred.ci.lb <- pred.ci.lb[sort.vec]
    pred.ci.ub <- pred.ci.ub[sort.vec]
    psize <- psize[sort.vec]
    pch <- pch[sort.vec]
    ilab <- ilab[sort.vec, , drop = FALSE]
    yiviX.na <- is.na(cbind(yi, vi, X))
    if (any(yiviX.na)) {
        not.na <- apply(yiviX.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit") {
            yi <- yi[not.na]
            vi <- vi[not.na]
            X <- X[not.na, , drop = FALSE]
            slab <- slab[not.na]
            pred <- pred[not.na]
            pred.ci.lb <- pred.ci.lb[not.na]
            pred.ci.ub <- pred.ci.ub[not.na]
            psize <- psize[not.na]
            ilab <- ilab[not.na, , drop = FALSE]
        }
        if (na.act == "na.fail") 
            stop("Missing values in results.")
    }
    k <- length(yi)
    alpha <- (100 - level)/100
    ci.lb <- yi - qnorm(1 - alpha/2) * sqrt(vi)
    ci.ub <- yi + qnorm(1 - alpha/2) * sqrt(vi)
    if (is.function(transf)) {
        if (is.null(targs)) {
            yi <- sapply(yi, transf)
            ci.lb <- sapply(ci.lb, transf)
            ci.ub <- sapply(ci.ub, transf)
            pred <- sapply(pred, transf)
            pred.ci.lb <- sapply(pred.ci.lb, transf)
            pred.ci.ub <- sapply(pred.ci.ub, transf)
        }
        else {
            yi <- sapply(yi, transf, targs)
            ci.lb <- sapply(ci.lb, transf, targs)
            ci.ub <- sapply(ci.ub, transf, targs)
            pred <- sapply(pred, transf, targs)
            pred.ci.lb <- sapply(pred.ci.lb, transf, targs)
            pred.ci.ub <- sapply(pred.ci.ub, transf, targs)
        }
    }
    rng <- max(ci.ub, na.rm = TRUE) - min(ci.lb, na.rm = TRUE)
    if (is.null(xlim)) {
        xlim <- c(min(ci.lb, na.rm = TRUE) - rng * 1.2, max(ci.ub, 
            na.rm = TRUE) + rng * 1.2)
        xlim <- round(xlim, digits)
    }
    if (is.null(alim)) {
        if (is.null(at)) {
            alim <- c(min(ci.lb, na.rm = TRUE) - rng * 0.2, max(ci.ub, 
                na.rm = TRUE) + rng * 0.2)
            alim <- round(alim, digits)
        }
        else {
            alim <- range(at)
        }
    }
    alim <- sort(alim)
    xlim <- sort(xlim)
    if (xlim[1] > min(yi, na.rm = TRUE)) {
        xlim[1] <- min(yi, na.rm = TRUE)
    }
    if (xlim[2] < max(yi, na.rm = TRUE)) {
        xlim[2] <- max(yi, na.rm = TRUE)
    }
    if (alim[1] > min(yi, na.rm = TRUE)) {
        alim[1] <- min(yi, na.rm = TRUE)
    }
    if (alim[2] < max(yi, na.rm = TRUE)) {
        alim[2] <- max(yi, na.rm = TRUE)
    }
    if (alim[1] < xlim[1]) {
        xlim[1] <- alim[1]
    }
    if (alim[2] > xlim[2]) {
        xlim[2] <- alim[2]
    }
    addrows <- round(addrows)
    addrows[addrows < 0] <- 0
    if (x$int.only && addfit) {
        addrows <- addrows + 2
    }
    else {
        if (addrows > 0) 
            addrows <- addrows + 1
    }
    if (is.null(ylim)) {
        ylim <- c(0.5 - addrows, k + 2)
    }
    else {
        ylim <- sort(ylim)
    }
    if (is.null(at)) {
        at <- seq(alim[1], alim[2], length = steps)
    }
    else {
        at[at < alim[1]] <- alim[1]
        at[at > alim[2]] <- alim[2]
        at <- unique(at)
    }
    at.lab <- at
    if (is.function(atransf)) {
        if (is.null(targs)) {
            at.lab <- formatC(sapply(at.lab, atransf), digits = digits, 
                format = "f")
        }
        else {
            at.lab <- formatC(sapply(at.lab, atransf, targs), 
                digits = digits, format = "f")
        }
    }
    else {
        at.lab <- round(at, digits)
    }
    par.mar <- par("mar")
    par.mar.adj <- par.mar - c(0, 3, 0, 1)
    par.mar.adj[par.mar.adj < 1] <- 1
    par(mar = par.mar.adj)
    on.exit(par(mar = par.mar))
    plot(NA, NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", 
        yaxt = "n", xaxt = "n", xaxs = "i", bty = "n", ...)
    abline(h = k + 1, ...)
    par.usr <- par("usr")
    height <- par.usr[4] - par.usr[3]
    lheight <- strheight("O")
    cex.adj <- ifelse(k * lheight > height * 0.8, height/(1.25 * 
        k * lheight), 1)
    if (is.null(cex)) {
        cex <- par("cex") * cex.adj
    }
    else {
        if (is.null(cex.lab)) 
            cex.lab <- cex
        if (is.null(cex.axis)) 
            cex.axis <- cex
    }
    if (is.null(cex.lab)) 
        cex.lab <- par("cex.lab") * cex.adj
    if (is.null(cex.axis)) 
        cex.axis <- par("cex.axis") * cex.adj
    if (addfit && !x$int.only) {
        for (i in 1:k) {
            if (is.na(pred[i])) 
                next
            if ((pred.ci.lb[i] > alim[1]) & (pred.ci.ub[i] < 
                alim[2])) 
                polygon(x = c(pred.ci.lb[i], pred[i], pred.ci.ub[i], 
                  pred[i]), y = c(i, i + (height/100) * cex * 
                  efac, i, i - (height/100) * cex * efac), col = col, 
                  border = border, ...)
        }
    }
    if (addfit && x$int.only) {
        b <- x$b
        b.ci.lb <- x$ci.lb
        b.ci.ub <- x$ci.ub
        if (is.function(transf)) {
            if (is.null(targs)) {
                b <- sapply(b, transf)
                b.ci.lb <- sapply(b.ci.lb, transf)
                b.ci.ub <- sapply(b.ci.ub, transf)
            }
            else {
                b <- sapply(b, transf, targs)
                b.ci.lb <- sapply(b.ci.lb, transf, targs)
                b.ci.ub <- sapply(b.ci.ub, transf, targs)
            }
        }
        polygon(x = c(b.ci.lb, b, b.ci.ub, b), y = c(-1, -1 + 
            (height/100) * cex * efac, -1, -1 - (height/100) * 
            cex * efac), col = "black", ...)
        if (annotate) {
            if (is.function(atransf)) {
                if (is.null(targs)) {
                  text(x = xlim[2], -1, labels = paste(formatC(sapply(b, 
                    atransf), digits = digits, format = "f", 
                    flag = " "), "[", formatC(sapply(b.ci.lb, 
                    atransf), digits = digits, format = "f", 
                    flag = " "), ",", formatC(sapply(b.ci.ub, 
                    atransf), digits = digits, format = "f", 
                    flag = " "), "]"), pos = 2, cex = cex, ...)
                }
                else {
                  text(x = xlim[2], -1, labels = paste(formatC(sapply(b, 
                    atransf, targs), digits = digits, format = "f", 
                    flag = " "), "[", formatC(sapply(b.ci.lb, 
                    atransf, targs), digits = digits, format = "f", 
                    flag = " "), ",", formatC(sapply(b.ci.ub, 
                    atransf, targs), digits = digits, format = "f", 
                    flag = " "), "]"), pos = 2, cex = cex, ...)
                }
            }
            else {
                text(x = xlim[2], -1, labels = paste(formatC(b, 
                  digits = digits, format = "f", flag = " "), 
                  "[", formatC(b.ci.lb, digits = digits, format = "f", 
                    flag = " "), ",", formatC(b.ci.ub, digits = digits, 
                    format = "f", flag = " "), "]"), pos = 2, 
                  cex = cex, ...)
            }
        }
        if (is.null(mlab)) 
            mlab <- ifelse((x$method == "FE"), "FE Model", "RE Model")
        text(xlim[1], -1, mlab, pos = 4, cex = cex, ...)
    }
    axis(side = 1, at = at, labels = at.lab, cex.axis = cex.axis, 
        ...)
    if (!is.null(xlab)) 
        mtext(xlab, side = 1, at = min(at) + (max(at) - min(at))/2, 
            line = 2.75, cex = cex.lab, ...)
    for (i in 1:k) {
        if (is.na(yi[i]) || is.na(vi)[i]) 
            next
        segments(max(ci.lb[i], alim[1]), i, min(ci.ub[i], alim[2]), 
            i, ...)
        if (ci.lb[i] >= alim[1]) {
            segments(ci.lb[i], i - (k/100) * cex * efac, ci.lb[i], 
                i + (k/100) * cex * efac, ...)
        }
        else {
            polygon(x = c(alim[1], alim[1] + (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[1] + (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[1]), y = c(i, i + (k/100) * 
                cex * efac, i - (k/100) * cex * efac, i), col = "black", 
                ...)
        }
        if (ci.ub[i] <= alim[2]) {
            segments(ci.ub[i], i - (k/100) * cex * efac, ci.ub[i], 
                i + (k/100) * cex * efac, ...)
        }
        else {
            polygon(x = c(alim[2], alim[2] - (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[2] - (1.4/100) * cex * 
                (xlim[2] - xlim[1]), alim[2]), y = c(i, i + (k/100) * 
                cex * efac, i - (k/100) * cex * efac, i), col = "black", 
                ...)
        }
    }
    text(xlim[1], 1:k, slab, pos = 4, cex = cex, ...)
    if (!is.null(ilab)) {
        for (l in 1:dim(ilab)[2]) {
            text(ilab.xpos[l], 1:k, ilab[, l], offset = 0, pos = ilab.pos[l], 
                cex = cex, ...)
        }
    }
    if (is.numeric(refline)) 
        segments(refline, ylim[1] - 1, refline, (k + 1), lty = "dotted", 
            ...)
    if (annotate) {
        if (is.function(atransf)) {
            if (is.null(targs)) {
                text(x = xlim[2], 1:k, labels = paste(formatC(sapply(yi, 
                  atransf), digits = digits, format = "f", flag = " "), 
                  "[", formatC(sapply(ci.lb, atransf), digits = digits, 
                    format = "f", flag = " "), ",", formatC(sapply(ci.ub, 
                    atransf), digits = digits, format = "f", 
                    flag = " "), "]"), pos = 2, cex = cex, ...)
            }
            else {
                text(x = xlim[2], 1:k, labels = paste(formatC(sapply(yi, 
                  atransf, targs), digits = digits, format = "f", 
                  flag = " "), "[", formatC(sapply(ci.lb, atransf, 
                  targs), digits = digits, format = "f", flag = " "), 
                  ",", formatC(sapply(ci.ub, atransf, targs), 
                    digits = digits, format = "f", flag = " "), 
                  "]"), pos = 2, cex = cex, ...)
            }
        }
        else {
            text(x = xlim[2], 1:k, labels = paste(formatC(yi, 
                digits = digits, format = "f", flag = " "), "[", 
                formatC(ci.lb, digits = digits, format = "f", 
                  flag = " "), ",", formatC(ci.ub, digits = digits, 
                  format = "f", flag = " "), "]"), pos = 2, cex = cex, 
                ...)
        }
    }
    points(yi, 1:k, pch = pch, cex = cex * psize, ...)
    if (addrows > 0) 
        abline(h = 0, ...)
    invisible()
}
fsn <-
function (yi, vi, sei, data = NULL, type = "Rosenthal", alpha = 0.05, 
    target = NULL, digits = 4, subset = NULL) 
{
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    type <- match.arg(type, c("Rosenthal", "Orwin", "Rosenberg"))
    if (is.null(data)) {
        data <- sys.frame(sys.parent())
    }
    else {
        if (!is.data.frame(data)) {
            data <- data.frame(data)
        }
    }
    mf <- match.call()
    mf.yi <- mf[[match("yi", names(mf))]]
    mf.vi <- mf[[match("vi", names(mf))]]
    mf.sei <- mf[[match("sei", names(mf))]]
    mf.subset <- mf[[match("subset", names(mf))]]
    yi <- eval(mf.yi, data)
    vi <- eval(mf.vi, data)
    sei <- eval(mf.sei, data)
    subset <- eval(mf.subset, data)
    if (missing(vi)) 
        vi <- sei^2
    if (length(yi) != length(vi)) 
        stop("Length of yi and vi (or sei) is not the same.")
    if (!is.null(subset)) {
        yi <- yi[subset]
        vi <- vi[subset]
    }
    yivi.na <- is.na(cbind(yi, vi))
    if (any(yivi.na)) {
        not.na <- apply(yivi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            yi <- yi[not.na]
            vi <- vi[not.na]
        }
        if (na.act == "na.fail") 
            stop("Missing values in results.")
    }
    if (type == "Rosenthal") {
        k <- length(yi)
        zi <- yi/sqrt(vi)
        z.avg <- abs(sum(zi)/sqrt(k))
        pval <- pnorm(z.avg, lower.tail = FALSE)
        fsnum <- ceiling(max(0, k * (z.avg/qnorm(1 - alpha))^2 - 
            k))
        meanes <- NA
        target <- NA
    }
    if (type == "Orwin") {
        k <- length(yi)
        meanes <- mean(yi)
        if (is.null(target)) {
            target <- meanes/2
        }
        if (identical(target, 0)) {
            fsnum <- Inf
        }
        else {
            fsnum <- ceiling(max(0, k * (meanes - target)/target))
        }
        pval <- NA
    }
    if (type == "Rosenberg") {
        k <- length(yi)
        wi <- 1/vi
        meanes <- sum(wi * yi)/sum(wi)
        zval <- meanes/sqrt(1/sum(wi))
        w.p <- (sum(wi * yi)/qnorm(1 - alpha/2))^2 - sum(wi)
        pval <- 2 * (1 - pnorm(abs(zval)))
        fsnum <- ceiling(max(0, k * w.p/sum(wi)))
        target <- NA
    }
    res <- list(type, fsnum, alpha, pval, meanes, target, digits)
    names(res) <- c("type", "fsnum", "alpha", "pval", "meanes", 
        "target", "digits")
    class(res) <- c("fsn")
    return(res)
}
funnel <-
function (x, ...) 
UseMethod("funnel")
funnel.rma <-
function (x, xlim = NULL, ylim = NULL, xlab = NULL, ylab = "Standard Error", 
    steps = 5, level = x$level, digits = 3, addtau2 = FALSE, 
    type = "rstandard", back = "lightgray", shade = "white", 
    hlines = "white", refline = NULL, pch = 19, pch.fill = 21, 
    ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    type <- match.arg(type, c("rstandard", "rstudent"))
    if (x$int.only) {
        if (is.null(refline)) 
            refline <- x$b
        if (addtau2) {
            tau2 <- x$tau2
        }
        else {
            tau2 <- 0
        }
        yi <- x$yi
        vi <- x$vi
        sei <- sqrt(vi)
        if (is.null(xlab)) 
            xlab <- "Observed Outcome"
    }
    else {
        if (is.null(refline)) 
            refline <- 0
        tau2 <- 0
        if (type == "rstandard") {
            res <- rstandard(x)
        }
        else {
            res <- rstudent(x)
        }
        not.na <- !is.na(res$resid)
        yi <- res$resid[not.na]
        sei <- res$se[not.na]
        if (is.null(xlab)) 
            xlab <- "Residual Value"
    }
    if (is.null(ylim)) {
        ylim <- c(0, max(sei) * 1)
    }
    else {
        ylim <- sort(ylim)
        if (ylim[1] < 0 || ylim[2] < 0) 
            stop("Both limits for the y-axis must be >= 0.")
    }
    alpha <- (100 - level)/100
    alpha.min <- min(alpha)
    x.lb.bot <- refline - qnorm(1 - alpha.min/2) * sqrt(ylim[2]^2 + 
        tau2)
    x.ub.bot <- refline + qnorm(1 - alpha.min/2) * sqrt(ylim[2]^2 + 
        tau2)
    x.lb.top <- refline - qnorm(1 - alpha.min/2) * sqrt(ylim[1]^2 + 
        tau2)
    x.ub.top <- refline + qnorm(1 - alpha.min/2) * sqrt(ylim[1]^2 + 
        tau2)
    if (is.null(xlim)) {
        xlim <- c(min(x.lb.bot, min(yi)), max(x.ub.bot, max(yi)))
        rxlim <- xlim[2] - xlim[1]
        xlim[1] <- xlim[1] - (rxlim * 0.1)
        xlim[2] <- xlim[2] + (rxlim * 0.1)
    }
    else {
        xlim <- sort(xlim)
    }
    plot(NA, NA, xlim = xlim, ylim = max(sei) - c(ylim[2], ylim[1]), 
        xlab = xlab, ylab = ylab, xaxt = "n", yaxt = "n", bty = "n", 
        ...)
    par.usr <- par("usr")
    rect(par.usr[1], par.usr[3], par.usr[2], par.usr[4], col = back, 
        border = NA, ...)
    axis(side = 2, at = max(sei) - seq(ylim[2], ylim[1], length = steps), 
        labels = formatC(seq(ylim[2], ylim[1], length = steps), 
            digits = digits, format = "f"), ...)
    abline(h = max(sei) - seq(ylim[2], ylim[1], length = steps), 
        col = hlines, ...)
    avals <- length(alpha)
    rylim <- ylim[2] - ylim[1]
    ylim[1] <- max(0, ylim[1] - (rylim * 0.1))
    ylim[2] <- ylim[2] + (rylim * 0.1)
    if (x$method == "FE") {
        for (m in avals:1) {
            x.lb.bot <- refline - qnorm(1 - alpha[m]/2) * sqrt(ylim[2]^2 + 
                tau2)
            x.ub.bot <- refline + qnorm(1 - alpha[m]/2) * sqrt(ylim[2]^2 + 
                tau2)
            x.lb.top <- refline - qnorm(1 - alpha[m]/2) * sqrt(ylim[1]^2 + 
                tau2)
            x.ub.top <- refline + qnorm(1 - alpha[m]/2) * sqrt(ylim[1]^2 + 
                tau2)
            polygon(c(x.lb.bot, x.lb.top, x.ub.top, x.ub.bot), 
                c(max(sei) - ylim[2], max(sei) - ylim[1], max(sei) - 
                  ylim[1], max(sei) - ylim[2]), border = NA, 
                col = shade[m], ...)
            segments(refline, max(sei) - ylim[1], refline, max(sei) - 
                ylim[2], ...)
            segments(x.lb.bot, max(sei) - ylim[2], x.lb.top, 
                max(sei) - ylim[1], lty = "dotted", ...)
            segments(x.ub.bot, max(sei) - ylim[2], x.ub.top, 
                max(sei) - ylim[1], lty = "dotted", ...)
        }
    }
    else {
        for (m in avals:1) {
            vi.vals <- seq(ylim[1]^2, ylim[2]^2, length = 100)
            ci.left <- refline - qnorm(1 - alpha[m]/2) * sqrt(vi.vals + 
                tau2)
            ci.right <- refline + qnorm(1 - alpha[m]/2) * sqrt(vi.vals + 
                tau2)
            lvi <- length(vi.vals)
            polygon(c(ci.left[lvi:1], ci.right), c(max(sei) - 
                sqrt(vi.vals)[lvi:1], max(sei) - sqrt(vi.vals)), 
                border = NA, col = shade[m], ...)
            segments(refline, max(sei), refline, max(sei) - ylim[2], 
                ...)
            lines(ci.left, max(sei) - sqrt(vi.vals), lty = "dotted", 
                ...)
            lines(ci.right, max(sei) - sqrt(vi.vals), lty = "dotted", 
                ...)
        }
    }
    box(bty = "l", ...)
    axis(side = 1, ...)
    points(yi, max(sei) - sei, pch = pch, ...)
    if (is.element("rma.uni.trimfill", class(x))) 
        points(yi[x$fill == 1], max(sei) - sei[x$fill == 1], 
            pch = pch.fill, bg = "white", ...)
    invisible()
}
galbraith <-
function (x, ...) 
UseMethod("radial")
hatvalues.rma.uni <-
function (model, ...) 
{
    if (!is.element("rma.uni", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    if (x$weighted) {
        wi <- 1/(x$vi + x$tau2)
        W <- diag(wi)
        stXWX <- .invcalc(X = x$X, W = W, k = x$k)
        H <- x$X %*% stXWX %*% crossprod(x$X, W)
    }
    else {
        stXX <- .invcalc(X = x$X, W = diag(x$k), k = x$k)
        H <- x$X %*% tcrossprod(stXX, x$X)
    }
    hii <- rep(NA, x$k.f)
    hii[x$not.na] <- diag(H)
    hii[hii > 1 - 10 * .Machine$double.eps] <- 1
    names(hii) <- x$slab
    if (na.act == "na.omit") {
        hii <- hii[x$not.na]
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    return(hii)
}
influence.rma.uni <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.uni", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    tau2.del <- rep(NA, x$k.f)
    delpred <- rep(NA, x$k.f)
    vdelpred <- rep(NA, x$k.f)
    QE.del <- rep(NA, x$k.f)
    dffits <- rep(NA, x$k.f)
    dfbetas <- matrix(NA, nrow = x$k.f, ncol = length(x$b))
    cooks.d <- rep(NA, x$k.f)
    covratio <- rep(NA, x$k.f)
    detx <- det(x$vb)
    pred <- x$X.f %*% x$b
    if (x$weighted) {
        wi <- 1/(x$vi + x$tau2)
        W <- diag(wi)
        svb <- crossprod(x$X, W) %*% x$X/x$s2w
    }
    else {
        svb <- chol2inv(chol(x$vb))
        stXX <- .invcalc(X = x$X, W = diag(x$k), k = x$k)
        H <- x$X %*% stXX %*% t(x$X)
    }
    options(na.action = "na.exclude")
    hii <- hatvalues(x)
    options(na.action = na.act)
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma(x$yi.f[-i], x$vi.f[-i], mods = cbind(x$X.f[-i, 
            ]), method = x$method, weighted = x$weighted, intercept = FALSE, 
            knha = x$knha, control = x$control, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        tau2.del[i] <- res$tau2
        Xi <- matrix(x$X.f[i, ], nrow = 1)
        delpred[i] <- Xi %*% res$b
        vdelpred[i] <- Xi %*% tcrossprod(res$vb, Xi)
        QE.del[i] <- res$QE
        if (x$weighted) {
            dffits[i] <- (pred[i] - delpred[i])/sqrt(res$s2w * 
                hii[i] * (tau2.del[i] + x$vi.f[i]))
        }
        else {
            dffits[i] <- (pred[i] - delpred[i])/(sqrt(res$s2w * 
                diag(H %*% diag(tau2.del[i] + x$vi) %*% t(H))))[i - 
                x$k.f + sum(x$not.na)]
        }
        dfbeta <- x$b - res$b
        if (x$weighted) {
            vb.del <- .invcalc(X = x$X, W = diag(1/(x$vi + tau2.del[i])), 
                k = x$k)
            dfbetas[i, ] <- dfbeta/sqrt(res$s2w * diag(vb.del))
        }
        else {
            vb.del <- tcrossprod(stXX, x$X) %*% diag(x$vi + tau2.del[i]) %*% 
                x$X %*% stXX
            dfbetas[i, ] <- dfbeta/sqrt(res$s2w * diag(vb.del))
        }
        cooks.d[i] <- (crossprod(dfbeta, svb) %*% dfbeta)
        covratio[i] <- det(res$vb)/detx
    }
    delresid <- x$yi.f - delpred
    sedelresid <- sqrt(x$vi.f + vdelpred + tau2.del)
    standelres <- delresid/sedelresid
    weight <- rep(NA, x$k.f)
    if (x$weighted) {
        weight[x$not.na] <- wi/sum(wi) * 100
    }
    else {
        weight[x$not.na] <- 1/x$k * 100
    }
    if (na.act == "na.omit") {
        inf <- cbind(standelres[x$not.na], dffits[x$not.na], 
            cooks.d[x$not.na], covratio[x$not.na], tau2.del[x$not.na], 
            QE.del[x$not.na], hii[x$not.na], weight[x$not.na])
        dfb <- cbind(dfbetas[x$not.na, ])
        out <- list(inf = inf, dfb = dfb, tau2 = x$tau2, QE = x$QE, 
            ids = x$ids[x$not.na], not.na = x$not.na[x$not.na], 
            k = x$k, p = x$p, digits = digits)
        dimnames(out$inf)[[1]] <- x$slab[x$not.na]
        dimnames(out$dfb)[[1]] <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        inf <- cbind(standelres, dffits, cooks.d, covratio, tau2.del, 
            QE.del, hii, weight)
        dfb <- cbind(dfbetas)
        out <- list(inf = inf, dfb = dfb, tau2 = x$tau2, QE = x$QE, 
            ids = x$ids, not.na = x$not.na, k = x$k, p = x$p, 
            digits = digits)
        dimnames(out$inf)[[1]] <- x$slab
        dimnames(out$dfb)[[1]] <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    dimnames(out$dfb)[[2]] <- dimnames(x$b)[[1]]
    dimnames(out$inf)[[2]] <- c("rstudent", "dffits", "cook.d", 
        "cov.r", "tau2.del", "QE.del", "hat", "weight")
    out$inf <- data.frame(out$inf)
    out$dfb <- data.frame(out$dfb)
    class(out) <- "infl.rma.uni"
    return(out)
}
leave1out <-
function (x, ...) 
UseMethod("leave1out")
leave1out.rma.mh <-
function (x, digits = x$digits, transf = FALSE, ...) 
{
    if (!is.element("rma.mh", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.mh\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    b <- rep(NA, x$k.f)
    se <- rep(NA, x$k.f)
    zval <- rep(NA, x$k.f)
    pval <- rep(NA, x$k.f)
    ci.lb <- rep(NA, x$k.f)
    ci.ub <- rep(NA, x$k.f)
    QE <- rep(NA, x$k.f)
    QEp <- rep(NA, x$k.f)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma.mh(ai = x$ai.f[-i], bi = x$bi.f[-i], ci = x$ci.f[-i], 
            di = x$di.f[-i], measure = x$measure, add = x$add, 
            to = x$to, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        b[i] <- res$b
        se[i] <- res$se
        zval[i] <- res$zval
        pval[i] <- res$pval
        ci.lb[i] <- res$ci.lb
        ci.ub[i] <- res$ci.ub
        QE[i] <- res$QE
        QEp[i] <- res$QEp
    }
    if (transf) {
        if (x$measure == "OR" || x$measure == "RR") {
            b <- exp(b)
            se <- rep(NA, x$k.f)
            ci.lb <- exp(ci.lb)
            ci.ub <- exp(ci.ub)
        }
    }
    if (na.act == "na.omit") {
        out <- list(estimate = b[x$not.na], se = se[x$not.na], 
            zval = zval[x$not.na], pval = pval[x$not.na], ci.lb = ci.lb[x$not.na], 
            ci.ub = ci.ub[x$not.na], Q = QE[x$not.na], Qp = QEp[x$not.na])
        out$slab <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(estimate = b, se = se, zval = zval, pval = pval, 
            ci.lb = ci.lb, ci.ub = ci.ub, Q = QE, Qp = QEp)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
leave1out.rma.peto <-
function (x, digits = x$digits, transf = FALSE, ...) 
{
    if (!is.element("rma.peto", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.peto\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    b <- rep(NA, x$k.f)
    se <- rep(NA, x$k.f)
    zval <- rep(NA, x$k.f)
    pval <- rep(NA, x$k.f)
    ci.lb <- rep(NA, x$k.f)
    ci.ub <- rep(NA, x$k.f)
    QE <- rep(NA, x$k.f)
    QEp <- rep(NA, x$k.f)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma.peto(ai = x$ai.f[-i], bi = x$bi.f[-i], 
            ci = x$ci.f[-i], di = x$di.f[-i], add = x$add, to = x$to, 
            ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        b[i] <- res$b
        se[i] <- res$se
        zval[i] <- res$zval
        pval[i] <- res$pval
        ci.lb[i] <- res$ci.lb
        ci.ub[i] <- res$ci.ub
        QE[i] <- res$QE
        QEp[i] <- res$QEp
    }
    if (transf) {
        b <- exp(b)
        se <- rep(NA, x$k.f)
        ci.lb <- exp(ci.lb)
        ci.ub <- exp(ci.ub)
    }
    if (na.act == "na.omit") {
        out <- list(estimate = b[x$not.na], se = se[x$not.na], 
            zval = zval[x$not.na], pval = pval[x$not.na], ci.lb = ci.lb[x$not.na], 
            ci.ub = ci.ub[x$not.na], Q = QE[x$not.na], Qp = QEp[x$not.na])
        out$slab <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(estimate = b, se = se, zval = zval, pval = pval, 
            ci.lb = ci.lb, ci.ub = ci.ub, Q = QE, Qp = QEp)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
leave1out.rma.uni <-
function (x, digits = x$digits, transf = FALSE, targs = NULL, 
    ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (!x$int.only) 
        stop("Method only applicable for models without moderators.")
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    b <- rep(NA, x$k.f)
    se <- rep(NA, x$k.f)
    zval <- rep(NA, x$k.f)
    pval <- rep(NA, x$k.f)
    ci.lb <- rep(NA, x$k.f)
    ci.ub <- rep(NA, x$k.f)
    QE <- rep(NA, x$k.f)
    QEp <- rep(NA, x$k.f)
    tau2 <- rep(NA, x$k.f)
    I2 <- rep(NA, x$k.f)
    H2 <- rep(NA, x$k.f)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma(x$yi.f[-i], x$vi.f[-i], method = x$method, 
            weighted = x$weighted, intercept = TRUE, knha = x$knha, 
            control = x$control, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        b[i] <- res$b
        se[i] <- res$se
        zval[i] <- res$zval
        pval[i] <- res$pval
        ci.lb[i] <- res$ci.lb
        ci.ub[i] <- res$ci.ub
        QE[i] <- res$QE
        QEp[i] <- res$QEp
        tau2[i] <- res$tau2
        I2[i] <- res$I2
        H2[i] <- res$H2
    }
    if (is.function(transf)) {
        if (is.null(targs)) {
            b <- sapply(b, transf)
            se <- rep(NA, x$k.f)
            ci.lb <- sapply(ci.lb, transf)
            ci.ub <- sapply(ci.ub, transf)
        }
        else {
            b <- sapply(b, transf, targs)
            se <- rep(NA, x$k.f)
            ci.lb <- sapply(ci.lb, transf, targs)
            ci.ub <- sapply(ci.ub, transf, targs)
        }
    }
    if (na.act == "na.omit") {
        out <- list(estimate = b[x$not.na], se = se[x$not.na], 
            zval = zval[x$not.na], pvals = pval[x$not.na], ci.lb = ci.lb[x$not.na], 
            ci.ub = ci.ub[x$not.na], Q = QE[x$not.na], Qp = QEp[x$not.na], 
            tau2 = tau2[x$not.na], I2 = I2[x$not.na], H2 = H2[x$not.na])
        out$slab <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(estimate = b, se = se, zval = zval, pvals = pval, 
            ci.lb = ci.lb, ci.ub = ci.ub, Q = QE, Qp = QEp, tau2 = tau2, 
            I2 = I2, H2 = H2)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    if (x$method == "FE") 
        out <- out[-c(9, 10, 11)]
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
logLik.rma <-
function (object, REML = NULL, ...) 
{
    if (!is.element("rma", class(object))) 
        stop("Argument 'object' must be an object of class \"rma\".")
    if (is.null(REML)) {
        if (object$method == "REML") {
            REML <- TRUE
        }
        else {
            REML <- FALSE
        }
    }
    if (REML) {
        out <- object$fit.stats$REML[1]
        names(out) <- c("ll (REML)")
    }
    else {
        out <- object$fit.stats$ML[1]
        names(out) <- c("ll (ML)")
    }
    return(out)
}
metafor.news <-
function (...) 
{
    newsfile <- file.path(system.file(package = "metafor"), "NEWS")
    file.show(newsfile, ...)
}
permutest <-
function (x, ...) 
UseMethod("permutest")
permutest.rma.uni <-
function (x, iter = 1000, progbar = TRUE, digits = x$digits, 
    ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    if (progbar) 
        pbar <- txtProgressBar(min = 0, max = iter, style = 3)
    if (x$int.only) {
        zval.perm <- rep(NA, iter)
        QM.perm <- rep(NA, iter)
        zval.orig.abs <- abs(x$zval)
        i <- 1
        while (i <= iter) {
            signs <- 2 * rbinom(x$k, 1, 0.5) - 1
            res <- try(rma(signs * x$yi, x$vi, method = x$method, 
                weighted = x$weighted, intercept = TRUE, knha = x$knha, 
                control = x$control, btt = x$btt, ...), silent = FALSE)
            if (is.element("try-error", class(res))) 
                next
            zval.perm[i] <- abs(res$zval) > zval.orig.abs
            QM.perm[i] <- res$QM > x$QM
            i <- i + 1
            if (progbar) 
                setTxtProgressBar(pbar, i)
        }
        pval <- mean(zval.perm)
        QMp <- mean(QM.perm)
    }
    else {
        zval.perm <- matrix(NA, nrow = iter, ncol = x$p)
        QM.perm <- rep(NA, iter)
        zval.orig.abs <- abs(x$zval)
        if (x$intercept) {
            X <- x$X[, 2:res$p, drop = FALSE]
        }
        else {
            X <- x$X
        }
        i <- 1
        while (i <= iter) {
            res <- try(rma(x$yi, x$vi, mods = cbind(X[sample(1:x$k), 
                ]), method = x$method, weighted = x$weighted, 
                intercept = x$intercept, knha = x$knha, control = x$control, 
                btt = x$btt, ...), silent = FALSE)
            if (is.element("try-error", class(res))) 
                next
            zval.perm[i, ] <- abs(res$zval) > zval.orig.abs
            QM.perm[i] <- res$QM > x$QM
            i <- i + 1
            if (progbar) 
                setTxtProgressBar(pbar, i)
        }
        pval <- apply(zval.perm, 2, mean)
        QMp <- mean(QM.perm)
    }
    if (progbar) 
        close(pbar)
    out <- list(pval = pval, QMp = QMp, b = x$b, se = x$se, zval = x$zval, 
        ci.lb = x$ci.lb, ci.ub = x$ci.ub, QM = x$QM, k = x$k, 
        p = x$p, btt = x$btt, m = x$m, knha = x$knha, int.only = x$int.only, 
        digits = digits)
    class(out) <- "permutest.rma.uni"
    return(out)
}
plot.infl.rma.uni <-
function (x, plotdfb = FALSE, dfbnew = FALSE, logcov = TRUE, 
    las = 0, pch = 21, bg = "black", bg.infl = "red", col.na = "lightgray", 
    ...) 
{
    if (class(x) != "infl.rma.uni") 
        stop("Argument 'x' must be an object of class \"infl.rma.uni\".")
    ids <- x$ids
    lids <- length(ids)
    not.na <- x$not.na
    ids.infl <- abs(x$inf$dffits) > 3 * sqrt(x$p/(x$k - x$p)) | 
        pchisq(x$inf$cook.d, df = x$p) > 0.5 | x$inf$hat > 3 * 
        x$p/x$k | apply(abs(x$dfb) > 1, 1, any)
    par.mfrow <- par("mfrow")
    par(mfrow = c(4, 2))
    on.exit(par(mfrow = par.mfrow))
    par.mar <- par("mar")
    par.mar.adj <- par.mar - c(2, 2, 2, 1)
    par.mar.adj[par.mar.adj < 1] <- 1
    par(mar = par.mar.adj)
    on.exit(par(mar = par.mar), add = TRUE)
    plot(NA, NA, xlim = c(1, lids), ylim = c(min(x$inf$rstudent, 
        -2, na.rm = TRUE), max(x$inf$rstudent, 2, na.rm = TRUE)), 
        xaxt = "n", main = "rstudent", xlab = "", ylab = "", 
        las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = 0, lty = "dashed", ...)
    abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted", 
        ...)
    lines((1:lids)[x$not.na], x$inf$rstudent[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$rstudent, ...)
    points(1:lids, x$inf$rstudent, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$rstudent[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    plot(NA, NA, xlim = c(1, lids), ylim = range(x$inf$dffits, 
        na.rm = TRUE), xaxt = "n", main = "dffits", xlab = "", 
        ylab = "", las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = 0, lty = "dashed", ...)
    abline(h = 3 * sqrt(x$p/(x$k - x$p)), lty = "dotted", ...)
    abline(h = -3 * sqrt(x$p/(x$k - x$p)), lty = "dotted", ...)
    lines((1:lids)[x$not.na], x$inf$dffits[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$dffits, ...)
    points(1:lids, x$inf$dffits, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$dffits[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    plot(NA, NA, xlim = c(1, lids), ylim = range(x$inf$cook.d, 
        na.rm = TRUE), xaxt = "n", main = "cook.d", xlab = "", 
        ylab = "", las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = qchisq(0.5, df = x$p), lty = "dotted", ...)
    lines((1:lids)[x$not.na], x$inf$cook.d[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$cook.d, ...)
    points(1:lids, x$inf$cook.d, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$cook.d[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    if (logcov) {
        plot(NA, NA, xlim = c(1, lids), ylim = range(x$inf$cov.r, 
            na.rm = TRUE), xaxt = "n", main = "cov.r", xlab = "", 
            ylab = "", log = "y", las = las, ...)
        axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
            ...)
        abline(h = 1, lty = "dashed", ...)
        lines((1:lids)[x$not.na], x$inf$cov.r[x$not.na], col = col.na, 
            ...)
        lines(1:lids, x$inf$cov.r, ...)
        points(1:lids, x$inf$cov.r, pch = pch, bg = bg, ...)
        points((1:lids)[ids.infl], x$inf$cov.r[ids.infl], bg = bg.infl, 
            pch = pch, ...)
    }
    else {
        plot(NA, NA, xlim = c(1, lids), ylim = range(x$inf$cov.r, 
            na.rm = TRUE), xaxt = "n", main = "cov.r", xlab = "", 
            ylab = "", las = las, ...)
        axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
            ...)
        abline(h = 1, lty = "dashed", ...)
        lines((1:lids)[x$not.na], x$inf$cov.r[x$not.na], col = col.na, 
            ...)
        lines(1:lids, x$inf$cov.r, ...)
        points(1:lids, x$inf$cov.r, pch = pch, bg = bg, ...)
        points((1:lids)[ids.infl], x$inf$cov.r[ids.infl], bg = bg.infl, 
            pch = pch, ...)
    }
    plot(NA, NA, xlim = c(1, lids), ylim = range(x$inf$tau2.del, 
        na.rm = TRUE), xaxt = "n", main = "tau2.del", xlab = "", 
        ylab = "", las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = x$tau2, lty = "dashed", ...)
    lines((1:lids)[x$not.na], x$inf$tau2.del[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$tau2.del, ...)
    points(1:lids, x$inf$tau2.del, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$tau2.del[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    plot(NA, NA, xlim = c(1, lids), ylim = range(x$inf$QE.del, 
        na.rm = TRUE), xaxt = "n", main = "QE.del", xlab = "", 
        ylab = "", las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = x$QE, lty = "dashed", ...)
    lines((1:lids)[x$not.na], x$inf$QE.del[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$QE.del, ...)
    points(1:lids, x$inf$QE.del, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$QE.del[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    plot(NA, NA, xlim = c(1, lids), ylim = c(0, max(x$inf$hat, 
        na.rm = TRUE)), xaxt = "n", main = "hat", xlab = "", 
        ylab = "", las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = x$p/x$k, lty = "dashed", ...)
    abline(h = 3 * x$p/x$k, lty = "dotted", ...)
    lines((1:lids)[x$not.na], x$inf$hat[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$hat, ...)
    points(1:lids, x$inf$hat, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$hat[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    plot(NA, NA, xlim = c(1, lids), ylim = c(0, max(x$inf$weight, 
        na.rm = TRUE)), xaxt = "n", main = "weight", xlab = "", 
        ylab = "", las = las, ...)
    axis(side = 1, at = 1:lids, label = ids, xlab = "", las = las, 
        ...)
    abline(h = 100/x$k, lty = "dashed", ...)
    lines((1:lids)[x$not.na], x$inf$weight[x$not.na], col = col.na, 
        ...)
    lines(1:lids, x$inf$weight, ...)
    points(1:lids, x$inf$weight, pch = pch, bg = bg, ...)
    points((1:lids)[ids.infl], x$inf$weight[ids.infl], bg = bg.infl, 
        pch = pch, ...)
    if (dfbnew) 
        plotdfb <- TRUE
    if (plotdfb) {
        if (dfbnew) {
            dev.new()
            par.mar <- par("mar")
            par.mar.adj <- par.mar - c(2, 2, 2, 1)
            par.mar.adj[par.mar.adj < 1] <- 1
            par(mar = par.mar.adj)
            on.exit(par(mar = par.mar), add = TRUE)
        }
        else {
            par.ask <- par("ask")
            par(ask = TRUE)
        }
        par(mfrow = c(x$p, 1))
        for (i in 1:x$p) {
            plot(NA, NA, xlim = c(1, lids), ylim = range(x$dfb[, 
                i], na.rm = TRUE), xaxt = "n", main = paste("dfb: ", 
                dimnames(x$dfb)[[2]][i]), xlab = "", ylab = "", 
                las = las, ...)
            axis(side = 1, at = 1:lids, label = ids, xlab = "", 
                las = las, ...)
            abline(h = 0, lty = "dashed", ...)
            abline(h = 1, lty = "dotted", ...)
            abline(h = -1, lty = "dotted", ...)
            lines((1:lids)[x$not.na], x$dfb[x$not.na, i], col = col.na, 
                ...)
            lines(1:lids, x$dfb[, i], ...)
            points(1:lids, x$dfb[, i], pch = pch, bg = bg, ...)
            points((1:lids)[ids.infl], x$dfb[ids.infl, i], bg = bg.infl, 
                pch = pch, ...)
        }
        if (!dfbnew) {
            par(ask = par.ask)
        }
    }
    invisible()
}
plot.rma.mh <-
function (x, qqplot = FALSE, ...) 
{
    if (!is.element("rma.mh", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.mh\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    par.mfrow <- par("mfrow")
    par(mfrow = c(2, 2))
    on.exit(par(mfrow = par.mfrow))
    forest(x, ...)
    title("Forest Plot", ...)
    funnel(x, ...)
    title("Funnel Plot", ...)
    radial(x, ...)
    title("Radial Plot", ...)
    if (qqplot) {
        qqnorm(x, ...)
    }
    else {
        options(na.action = "na.exclude")
        z <- rstandard(x)$z
        options(na.action = na.act)
        not.na <- !is.na(z)
        if (na.act == "na.omit") {
            z <- z[not.na]
            ids <- x$ids[not.na]
            not.na <- not.na[not.na]
        }
        if (na.act == "na.exclude") 
            ids <- x$ids
        k <- length(z)
        plot(NA, NA, xlim = c(1, k), ylim = c(min(z, -2, na.rm = TRUE), 
            max(z, 2, na.rm = TRUE)), xaxt = "n", xlab = "Study", 
            ylab = "", bty = "l", ...)
        lines((1:k)[not.na], z[not.na], col = "lightgray", ...)
        lines(1:k, z, ...)
        points(1:k, z, pch = 21, bg = "black", ...)
        axis(side = 1, at = 1:k, label = ids, ...)
        abline(h = 0, lty = "dashed", ...)
        abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted", 
            ...)
        title("Standardized Residuals", ...)
    }
    invisible()
}
plot.rma.peto <-
function (x, qqplot = FALSE, ...) 
{
    if (!is.element("rma.peto", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.peto\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    par.mfrow <- par("mfrow")
    par(mfrow = c(2, 2))
    on.exit(par(mfrow = par.mfrow))
    forest(x, ...)
    title("Forest Plot", ...)
    funnel(x, ...)
    title("Funnel Plot", ...)
    radial(x, ...)
    title("Radial Plot", ...)
    if (qqplot) {
        qqnorm(x, ...)
    }
    else {
        options(na.action = "na.exclude")
        z <- rstandard(x)$z
        options(na.action = na.act)
        not.na <- !is.na(z)
        if (na.act == "na.omit") {
            z <- z[not.na]
            ids <- x$ids[not.na]
            not.na <- not.na[not.na]
        }
        if (na.act == "na.exclude") 
            ids <- x$ids
        k <- length(z)
        plot(NA, NA, xlim = c(1, k), ylim = c(min(z, -2, na.rm = TRUE), 
            max(z, 2, na.rm = TRUE)), xaxt = "n", xlab = "Study", 
            ylab = "", bty = "l", ...)
        lines((1:k)[not.na], z[not.na], col = "lightgray", ...)
        lines(1:k, z, ...)
        points(1:k, z, pch = 21, bg = "black", ...)
        axis(side = 1, at = 1:k, label = ids, ...)
        abline(h = 0, lty = "dashed", ...)
        abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted", 
            ...)
        title("Standardized Residuals", ...)
    }
    invisible()
}
plot.rma.uni <-
function (x, qqplot = FALSE, ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    par.mfrow <- par("mfrow")
    par(mfrow = c(2, 2))
    on.exit(par(mfrow = par.mfrow))
    if (x$int.only) {
        forest(x, ...)
        title("Forest Plot", ...)
        funnel(x, ...)
        title("Funnel Plot", ...)
        radial(x, ...)
        title("Radial Plot", ...)
        if (qqplot) {
            qqnorm(x, ...)
        }
        else {
            options(na.action = "na.exclude")
            z <- rstandard(x)$z
            options(na.action = na.act)
            not.na <- !is.na(z)
            if (na.act == "na.omit") {
                z <- z[not.na]
                ids <- x$ids[not.na]
                not.na <- not.na[not.na]
            }
            if (na.act == "na.exclude") 
                ids <- x$ids
            k <- length(z)
            plot(NA, NA, xlim = c(1, k), ylim = c(min(z, -2, 
                na.rm = TRUE), max(z, 2, na.rm = TRUE)), xaxt = "n", 
                xlab = "Study", ylab = "", bty = "l", ...)
            lines((1:k)[not.na], z[not.na], col = "lightgray", 
                ...)
            lines(1:k, z, ...)
            points(1:k, z, pch = 21, bg = "black", ...)
            axis(side = 1, at = 1:k, label = ids, ...)
            abline(h = 0, lty = "dashed", ...)
            abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted", 
                ...)
            title("Standardized Residuals", ...)
        }
    }
    else {
        forest(x, ...)
        title("Forest Plot", ...)
        funnel(x, ...)
        title("Residual Funnel Plot", ...)
        options(na.action = "na.exclude")
        z <- rstandard(x)$z
        pred <- fitted(x)
        options(na.action = na.act)
        plot(pred, z, ylim = c(min(z, -2, na.rm = TRUE), max(z, 
            2, na.rm = TRUE)), pch = 19, bty = "l", xlab = "Fitted Value", 
            ylab = "Standardized Residual", ...)
        abline(h = 0, lty = "dashed", ...)
        abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted", 
            ...)
        title("Fitted vs. Standardized Residuals", ...)
        if (qqplot) {
            qqnorm(x, ...)
        }
        else {
            options(na.action = "na.exclude")
            z <- rstandard(x)$z
            options(na.action = na.act)
            not.na <- !is.na(z)
            if (na.act == "na.omit") {
                z <- z[not.na]
                ids <- x$ids[not.na]
                not.na <- not.na[not.na]
            }
            if (na.act == "na.exclude") {
                z <- z
                ids <- x$ids
            }
            k <- length(z)
            plot(NA, NA, xlim = c(1, k), ylim = c(min(z, -2, 
                na.rm = TRUE), max(z, 2, na.rm = TRUE)), xaxt = "n", 
                xlab = "Study", ylab = "", bty = "l", ...)
            lines((1:k)[not.na], z[not.na], col = "lightgray", 
                ...)
            lines(1:k, z, ...)
            points(1:k, z, pch = 21, bg = "black", ...)
            axis(side = 1, at = 1:k, label = ids, ...)
            abline(h = 0, lty = "dashed", ...)
            abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted", 
                ...)
            title("Standardized Residuals", ...)
        }
    }
    invisible()
}
predict.rma.uni <-
function (object, newmods = NULL, level = object$level, digits = object$digits, 
    transf = FALSE, targs = NULL, ...) 
{
    if (!is.element("rma.uni", class(object))) 
        stop("Argument 'object' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- object
    alpha <- (100 - level)/100
    if (x$knha) {
        crit <- qt(1 - alpha/2, df = x$k - x$p)
    }
    else {
        crit <- qnorm(1 - alpha/2)
    }
    if (x$int.only && !is.null(newmods)) 
        stop("Cannot specify new moderator values for models without moderators.")
    if (is.null(newmods)) {
        if (x$int.only) {
            knew <- 1
            Xnew <- cbind(1)
        }
        else {
            knew <- x$k.f
            Xnew <- x$X.f
        }
    }
    else {
        if (x$intercept && x$p == 2L) {
            knew <- length(newmods)
            Xnew <- cbind(c(newmods))
        }
        else {
            if (is.vector(newmods) || nrow(newmods) == 1L) {
                knew <- 1
                Xnew <- rbind(newmods)
            }
            else {
                knew <- dim(newmods)[1]
                Xnew <- cbind(newmods)
            }
        }
        if (x$intercept) {
            Xnew <- cbind(rep(1, knew), Xnew)
        }
    }
    pred <- rep(NA, knew)
    vpred <- rep(NA, knew)
    for (i in 1:knew) {
        Xinew <- matrix(Xnew[i, ], nrow = 1)
        pred[i] <- Xinew %*% x$b
        vpred[i] <- Xinew %*% tcrossprod(x$vb, Xinew)
    }
    se <- sqrt(vpred)
    ci.lb <- pred - crit * se
    ci.ub <- pred + crit * se
    cr.lb <- pred - crit * sqrt(vpred + x$tau2)
    cr.ub <- pred + crit * sqrt(vpred + x$tau2)
    if (is.function(transf)) {
        if (is.null(targs)) {
            pred <- sapply(pred, transf)
            se <- rep(NA, knew)
            ci.lb <- sapply(ci.lb, transf)
            ci.ub <- sapply(ci.ub, transf)
            cr.lb <- sapply(cr.lb, transf)
            cr.ub <- sapply(cr.ub, transf)
        }
        else {
            pred <- sapply(pred, transf, targs)
            se <- rep(NA, knew)
            ci.lb <- sapply(ci.lb, transf, targs)
            ci.ub <- sapply(ci.ub, transf, targs)
            cr.lb <- sapply(cr.lb, transf, targs)
            cr.ub <- sapply(cr.ub, transf, targs)
        }
    }
    if (is.null(newmods) && !x$int.only) {
        slab <- x$slab
    }
    else {
        slab <- 1:knew
    }
    if (x$int.only) 
        slab <- ""
    if (na.act == "na.omit") {
        not.na <- !is.na(pred)
        out <- list(pred = pred[not.na], se = se[not.na], ci.lb = ci.lb[not.na], 
            ci.ub = ci.ub[not.na], cr.lb = cr.lb[not.na], cr.ub = cr.ub[not.na])
        out$slab <- slab[not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(pred = pred, se = se, ci.lb = ci.lb, ci.ub = ci.ub, 
            cr.lb = cr.lb, cr.ub = cr.ub)
        out$slab <- slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    if (x$method == "FE") {
        out$cr.lb <- NULL
        out$cr.ub <- NULL
    }
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
print.anova.rma.uni <-
function (x, digits = x$digits, ...) 
{
    if (class(x) != "anova.rma.uni") 
        stop("Argument 'x' must be an object of class \"anova.rma.uni\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    res.table <- rbind(c(x$p.f, x$fit.stats.f[3], x$fit.stats.f[4], 
        x$fit.stats.f[1], NA, NA, x$QE.f, x$tau2.f, NA), c(x$p.r, 
        x$fit.stats.r[3], x$fit.stats.r[4], x$fit.stats.r[1], 
        x$LRT, x$pval, x$QE.r, x$tau2.r, NA))
    res.table[, 2:9] <- formatC(res.table[, 2:9], digits = digits, 
        format = "f")
    colnames(res.table) <- c("df", "AIC", "BIC", "logLik", "LRT", 
        "pval", "QE", "tau^2", "VAF")
    rownames(res.table) <- c("Full", "Reduced")
    pval <- x$pval
    if (pval > ncutoff) {
        res.table[2, 6] <- formatC(pval, digits = digits, format = "f")
    }
    else {
        res.table[2, 6] <- paste("<", cutoff, sep = "", collapse = "")
    }
    res.table[1, 5:6] <- ""
    res.table[1, 9] <- ""
    res.table[2, 9] <- paste(x$VAF, "%", sep = "")
    if (x$method == "FE") {
        res.table <- res.table[, 1:7]
    }
    print(res.table, quote = FALSE, right = TRUE)
    invisible()
}
print.fsn <-
function (x, digits = x$digits, ...) 
{
    if (class(x) != "fsn") 
        stop("Argument 'x' must be an object of class \"fsn\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    cat("\n")
    cat("Fail-safe N Calculation Using the", x$type, "Approach", 
        "\n\n")
    if (x$type == "Rosenthal") {
        pval <- x$pval
        if (pval > ncutoff) {
            pval <- formatC(pval, digits = digits, format = "f")
        }
        else {
            pval <- paste("<", cutoff, sep = "", collapse = "")
        }
        cat("Observed Significance Level:", formatC(pval, digits = digits, 
            format = "f"), "\n")
        cat("Target Significance Level:  ", x$alpha, "\n\n")
        cat("Fail-safe N:", x$fsnum, "\n\n")
    }
    if (x$type == "Orwin") {
        cat("Average Effect Size:", formatC(x$meanes, digits = digits, 
            format = "f"), "\n")
        cat("Target Effect Size: ", formatC(x$target, digits = digits, 
            format = "f"), "\n\n")
        cat("Fail-safe N:", x$fsnum, "\n\n")
    }
    if (x$type == "Rosenberg") {
        pval <- x$pval
        if (pval > ncutoff) {
            pval <- formatC(pval, digits = digits, format = "f")
        }
        else {
            pval <- paste("<", cutoff, sep = "", collapse = "")
        }
        cat("Average Effect Size:        ", formatC(x$meanes, 
            digits = digits, format = "f"), "\n")
        cat("Observed Significance Level:", formatC(pval, digits = digits, 
            format = "f"), "\n")
        cat("Target Significance Level:  ", x$alpha, "\n\n")
        cat("Fail-safe N:", x$fsnum, "\n\n")
    }
    invisible()
}
print.infl.rma.uni <-
function (x, digits = x$digits, ...) 
{
    if (class(x) != "infl.rma.uni") 
        stop("Argument 'x' must be an object of class \"infl.rma.uni\".")
    inf <- round(x$inf, digits)
    dfb <- round(x$dfb, digits)
    x <- list(inf = inf, dfb = dfb)
    print(x)
}
print.list.rma <-
function (x, digits = x$digits, ...) 
{
    if (!is.element("list.rma", class(x))) 
        stop("Argument 'x' must be an object of class \"list.rma\".")
    force(digits)
    attr(x, "class") <- NULL
    out <- x[1:(which(names(x) == "slab") - 1)]
    out <- data.frame(out, row.names = x$slab)
    out <- apply(out, 2, formatC, digits = digits, format = "f")
    print(out, quote = FALSE, right = TRUE)
}
print.permutest.rma.uni <-
function (x, digits = x$digits, signif.legend = TRUE, ...) 
{
    if (class(x) != "permutest.rma.uni") 
        stop("Argument 'x' must be an object of class \"permutest.rma.uni\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    QMp <- x$QMp
    if (QMp > ncutoff) {
        QMp <- paste("=", formatC(QMp, digits = digits, format = "f"))
    }
    else {
        QMp <- paste("< ", cutoff, sep = "", collapse = "")
    }
    cat("\n")
    if (!x$int.only) {
        cat("Test of Moderators (coefficient(s) ", paste(x$btt, 
            collapse = ","), "): \n", sep = "")
        if (!x$knha) {
            cat("QM(df = ", x$m, ") = ", formatC(x$QM, digits = digits, 
                format = "f"), ", p-val* ", QMp, "\n\n", sep = "")
        }
        else {
            cat("F(df1 = ", x$m, ", df2 = ", x$k - x$p, ") = ", 
                formatC(x$QM, digits = digits, format = "f"), 
                ", p-val* ", QMp, "\n\n", sep = "")
        }
    }
    res.table <- cbind(x$b, x$se, x$zval, x$pval, x$ci.lb, x$ci.ub)
    dimnames(res.table)[[2]] <- c("estimate", "se", "zval", "pval*", 
        "ci.lb", "ci.ub")
    if (x$knha) {
        dimnames(res.table)[[2]][3] <- c("tval")
    }
    signif <- symnum(x$pval, corr = FALSE, na = FALSE, cutpoints = c(0, 
        0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
        "*", ".", " "))
    res.table <- cbind(formatC(res.table, digits = digits, format = "f"), 
        signif)
    dimnames(res.table)[[2]][7] <- ""
    res.table[x$pval > ncutoff, 4] <- formatC(x$pval[x$pval > 
        ncutoff], digits = digits, format = "f")
    res.table[x$pval < ncutoff, 4] <- paste("<", cutoff, sep = "", 
        collapse = "")
    cat("Model Results:")
    cat("\n\n")
    print(res.table, quote = FALSE, right = TRUE, print.gap = 2)
    cat("\n")
    if (signif.legend == TRUE) {
        cat("---\nSignif. codes: ", attr(signif, "legend"), "\n\n")
    }
    invisible()
}
print.ranktest.rma <-
function (x, digits = x$digits, ...) 
{
    if (class(x) != "ranktest.rma") 
        stop("Argument 'x' must be an object of class \"ranktest.rma\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    pval <- x$pval
    if (pval > ncutoff) {
        pval <- paste("=", formatC(pval, digits = digits, format = "f"))
    }
    else {
        pval <- paste("< ", cutoff, sep = "", collapse = "")
    }
    cat("\n")
    cat("Rank Correlation Test for Funnel Plot Asymmetry\n\n")
    cat("Kendall's tau = ", formatC(x$tau, digits = digits, format = "f"), 
        ", p ", pval, "\n\n", sep = "")
    invisible()
}
print.regtest.rma <-
function (x, digits = x$digits, ...) 
{
    if (class(x) != "regtest.rma") 
        stop("Argument 'x' must be an object of class \"regtest.rma\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    pval <- x$pval
    if (pval > ncutoff) {
        pval <- paste("=", formatC(pval, digits = digits, format = "f"))
    }
    else {
        pval <- paste("< ", cutoff, sep = "", collapse = "")
    }
    cat("\n")
    cat("Regression Test for Funnel Plot Asymmetry\n\n")
    if (x$model == "lm") {
        cat("model:     weighted regression with multiplicative dispersion\n")
    }
    else {
        cat("model:    ", ifelse(x$method == "FE", "fixed-effects", 
            "mixed-effects"), "meta-regression model\n")
    }
    if (x$predictor == "sei") 
        cat("predictor: standard error\n\n")
    if (x$predictor == "vi") 
        cat("predictor: sampling variance\n\n")
    if (x$predictor == "ni") 
        cat("predictor: total sample size\n\n")
    if (x$predictor == "ninv") 
        cat("predictor: inverse of the total sample size\n\n")
    if (is.na(x$dfs)) {
        cat("z = ", formatC(x$zval, digits = digits, format = "f"), 
            ", p ", pval, "\n\n", sep = "")
    }
    else {
        cat("t = ", formatC(x$zval, digits = digits, format = "f"), 
            ", df = ", x$dfs, ", p ", pval, "\n\n", sep = "")
    }
    invisible()
}
print.rma.mh <-
function (x, digits = x$digits, showfit = FALSE, ...) 
{
    if (!is.element("rma.mh", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.mh\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    cat("\n")
    cat("Fixed-Effects Model (k = ", x$k, ")", sep = "")
    if (showfit) {
        cat("\n")
        fs <- c(formatC(x$fit.stats$ML, digits = digits, format = "f"))
        names(fs) <- c("logLik", "Deviance", "AIC", "BIC")
        cat("\n")
        print(fs, quote = FALSE, print.gap = 2)
        cat("\n")
    }
    else {
        cat("\n\n")
    }
    if (!is.na(x$QE)) {
        QEp <- x$QEp
        if (QEp > ncutoff) {
            QEp <- paste("=", formatC(QEp, digits = digits, format = "f"))
        }
        else {
            QEp <- paste("< ", cutoff, sep = "", collapse = "")
        }
        cat("Test for Heterogeneity: \n")
        cat("Q(df = ", x$k.yi - 1, ") = ", formatC(x$QE, digits = digits, 
            format = "f"), ", p-val ", QEp, sep = "")
    }
    if (x$measure == "OR" || x$measure == "RR") {
        res.table <- c(x$b, x$se, x$zval, x$pval, x$ci.lb, x$ci.ub)
        res.table.exp <- c(exp(x$b), exp(x$ci.lb), exp(x$ci.ub))
        if (!is.na(x$b)) {
            res.table <- formatC(res.table, digits = digits, 
                format = "f")
            res.table[4][x$pval > ncutoff] <- formatC(x$pval[x$pval > 
                ncutoff], digits = digits, format = "f")
            res.table[4][x$pval < ncutoff] <- paste("<", cutoff, 
                sep = "", collapse = "")
        }
        if (!is.na(x$b)) {
            res.table.exp <- formatC(res.table.exp, digits = digits, 
                format = "f")
        }
        names(res.table) <- c("estimate", "se", "zval", "pval", 
            "ci.lb", "ci.ub")
        names(res.table.exp) <- c("estimate", "ci.lb", "ci.ub")
        cat("\n\n")
        cat("Model Results (log scale):")
        cat("\n\n")
        print(res.table, quote = FALSE, right = TRUE)
        cat("\n")
        cat("Model Results (", x$measure, " scale):", sep = "")
        cat("\n\n")
        print(res.table.exp, quote = FALSE, right = TRUE)
        cat("\n")
        if (x$measure == "OR") {
            if (is.na(x$CMH)) {
                cat("Cochran-Mantel-Haenszel Test:     CMH Test not defined for these data \n", 
                  sep = "")
            }
            else {
                pval <- x$CMHp
                if (pval > ncutoff) {
                  pval <- paste("=", formatC(pval, digits = digits, 
                    format = "f"))
                }
                else {
                  pval <- paste("< ", cutoff, sep = "", collapse = "")
                }
                cat("Cochran-Mantel-Haenszel Test:     CMH = ", 
                  formatC(x$CMH, digits, format = "f"), ", df = 1,", 
                  paste(rep(" ", nchar(x$k.pos) - 1, collapse = "")), 
                  " p-val ", pval, "\n", sep = "")
            }
            if (is.na(x$TAp)) {
                cat("Tarone's Test for Heterogeneity:  Tarone's Test not defined for these data \n\n", 
                  sep = "")
            }
            else {
                pval <- x$TAp
                if (pval > ncutoff) {
                  pval <- paste("=", formatC(pval, digits = digits, 
                    format = "f"))
                }
                else {
                  pval <- paste("< ", cutoff, sep = "", collapse = "")
                }
                cat("Tarone's Test for Heterogeneity:  X^2 = ", 
                  formatC(x$TA, digits, format = "f"), ", df = ", 
                  x$k.pos - 1, ", p-val ", pval, "\n\n", sep = "")
            }
        }
    }
    else {
        res.table <- c(x$b, x$se, x$zval, x$pval, x$ci.lb, x$ci.ub)
        if (!is.na(x$b)) {
            res.table <- formatC(res.table, digits = digits, 
                format = "f")
            res.table[4][x$pval > ncutoff] <- formatC(x$pval[x$pval > 
                ncutoff], digits = digits, format = "f")
            res.table[4][x$pval < ncutoff] <- paste("<", cutoff, 
                sep = "", collapse = "")
        }
        names(res.table) <- c("estimate", "se", "zval", "pval", 
            "ci.lb", "ci.ub")
        cat("\n\n")
        cat("Model Results:")
        cat("\n\n")
        print(res.table, quote = FALSE, right = TRUE)
    }
    invisible()
}
print.rma.peto <-
function (x, digits = x$digits, showfit = FALSE, ...) 
{
    if (!is.element("rma.peto", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.peto\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    cat("\n")
    cat("Fixed-Effects Model (k = ", x$k, ")", sep = "")
    if (showfit) {
        cat("\n")
        fs <- c(formatC(x$fit.stats$ML, digits = digits, format = "f"))
        names(fs) <- c("logLik", "Deviance", "AIC", "BIC")
        cat("\n")
        print(fs, quote = FALSE, print.gap = 2)
        cat("\n")
    }
    else {
        cat("\n\n")
    }
    if (!is.na(x$QE)) {
        QEp <- x$QEp
        if (QEp > ncutoff) {
            QEp <- paste("=", formatC(QEp, digits = digits, format = "f"))
        }
        else {
            QEp <- paste("< ", cutoff, sep = "", collapse = "")
        }
        cat("Test for Heterogeneity: \n")
        cat("Q(df = ", x$k.yi - 1, ") = ", formatC(x$QE, digits = digits, 
            format = "f"), ", p-val ", QEp, sep = "")
    }
    res.table <- c(x$b, x$se, x$zval, x$pval, x$ci.lb, x$ci.ub)
    res.table.exp <- c(exp(x$b), exp(x$ci.lb), exp(x$ci.ub))
    if (!is.na(x$b)) {
        res.table <- formatC(res.table, digits = digits, format = "f")
        res.table[4][x$pval > ncutoff] <- formatC(x$pval[x$pval > 
            ncutoff], digits = digits, format = "f")
        res.table[4][x$pval < ncutoff] <- paste("<", cutoff, 
            sep = "", collapse = "")
    }
    if (!is.na(x$b)) {
        res.table.exp <- formatC(res.table.exp, digits = digits, 
            format = "f")
    }
    names(res.table) <- c("estimate", "se", "zval", "pval", "ci.lb", 
        "ci.ub")
    names(res.table.exp) <- c("estimate", "ci.lb", "ci.ub")
    cat("\n\n")
    cat("Model Results (log scale):")
    cat("\n\n")
    print(res.table, quote = FALSE, right = TRUE)
    cat("\n")
    cat("Model Results (OR scale):", sep = "")
    cat("\n\n")
    print(res.table.exp, quote = FALSE, right = TRUE)
    cat("\n")
    invisible()
}
print.rma.uni <-
function (x, digits = x$digits, showfit = FALSE, signif.legend = TRUE, 
    ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    cutoff <- paste(c(".", rep(0, digits - 1), 1), collapse = "")
    ncutoff <- as.numeric(cutoff)
    cat("\n")
    if (x$method == "FE") {
        if (x$int.only) {
            cat("Fixed-Effects Model (k = ", x$k, ")", sep = "")
        }
        else {
            cat("Fixed-Effects with Moderators Model (k = ", 
                x$k, ")", sep = "")
        }
    }
    else {
        if (x$int.only) {
            cat("Random-Effects Model (k = ", x$k, "; ", sep = "")
        }
        else {
            cat("Mixed-Effects Model (k = ", x$k, "; ", sep = "")
        }
        cat("tau^2 estimator: ", x$method, ")", sep = "")
    }
    if (showfit) {
        cat("\n")
        if (x$method == "REML") {
            fs <- c(formatC(x$fit.stats$REML, digits = digits, 
                format = "f"))
            names(fs) <- c("logLik", "Deviance", "AIC", "BIC")
        }
        else {
            fs <- c(formatC(x$fit.stats$ML, digits = digits, 
                format = "f"))
            names(fs) <- c("logLik", "Deviance", "AIC", "BIC")
        }
        cat("\n")
        print(fs, quote = FALSE, print.gap = 2)
        cat("\n")
    }
    else {
        cat("\n\n")
    }
    if (x$method != "FE") {
        if (x$int.only) {
            if (x$method == "ML" || x$method == "REML") {
                cat("tau^2 (estimate of total amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), " (SE = ", 
                  ifelse(is.na(x$se.tau2), NA, formatC(x$se.tau2, 
                    digits = digits, format = "f")), ")", "\n", 
                  sep = "")
            }
            else {
                cat("tau^2 (estimate of total amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), "\n", sep = "")
            }
            cat("tau (sqrt of the estimate of total heterogeneity): ", 
                ifelse(x$tau2 >= 0, formatC(sqrt(x$tau2), digits = ifelse(x$tau2 <= 
                  .Machine$double.eps * 10, 0, digits), format = "f"), 
                  NA), "\n", sep = "")
            cat("I^2 (% of total variability due to heterogeneity): ", 
                ifelse(is.na(x$I2), NA, formatC(x$I2, digits = 2, 
                  format = "f")), "%", "\n", sep = "")
            cat("H^2 (total variability / within-study variance):   ", 
                ifelse(is.na(x$H2), NA, formatC(x$H2, digits = 2, 
                  format = "f")), sep = "")
        }
        else {
            if (x$method == "ML" || x$method == "REML") {
                cat("tau^2 (estimate of residual amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), " (SE = ", 
                  ifelse(is.na(x$se.tau2), NA, formatC(x$se.tau2, 
                    digits = digits, format = "f")), ")", "\n", 
                  sep = "")
            }
            else {
                cat("tau^2 (estimate of residual amount of heterogeneity): ", 
                  formatC(x$tau2, digits = ifelse(x$tau2 <= .Machine$double.eps * 
                    10, 0, digits), format = "f"), "\n", sep = "")
            }
            cat("tau (sqrt of the estimate of residual heterogeneity): ", 
                ifelse(x$tau2 >= 0, formatC(sqrt(x$tau2), digits = ifelse(x$tau2 <= 
                  .Machine$double.eps * 10, 0, digits), format = "f"), 
                  NA), sep = "")
        }
        cat("\n\n")
    }
    if (!is.na(x$QE)) {
        QEp <- x$QEp
        if (QEp > ncutoff) {
            QEp <- paste("=", formatC(QEp, digits = digits, format = "f"))
        }
        else {
            QEp <- paste("< ", cutoff, sep = "", collapse = "")
        }
        if (x$int.only) {
            cat("Test for Heterogeneity: \n")
            cat("Q(df = ", x$k - x$p, ") = ", formatC(x$QE, digits = digits, 
                format = "f"), ", p-val ", QEp, "\n\n", sep = "")
        }
        else {
            cat("Test for Residual Heterogeneity: \n")
            cat("QE(df = ", x$k - x$p, ") = ", formatC(x$QE, 
                digits = digits, format = "f"), ", p-val ", QEp, 
                "\n\n", sep = "")
        }
    }
    QMp <- x$QMp
    if (QMp > ncutoff) {
        QMp <- paste("=", formatC(QMp, digits = digits, format = "f"))
    }
    else {
        QMp <- paste("< ", cutoff, sep = "", collapse = "")
    }
    if (x$p > 1) {
        cat("Test of Moderators (coefficient(s) ", paste(x$btt, 
            collapse = ","), "): \n", sep = "")
        if (!x$knha) {
            cat("QM(df = ", x$m, ") = ", formatC(x$QM, digits = digits, 
                format = "f"), ", p-val ", QMp, "\n\n", sep = "")
        }
        else {
            cat("F(df1 = ", x$m, ", df2 = ", x$k - x$p, ") = ", 
                formatC(x$QM, digits = digits, format = "f"), 
                ", p-val ", QMp, "\n\n", sep = "")
        }
    }
    if (x$int.only) {
        res.table <- c(x$b, x$se, x$zval, x$pval, x$ci.lb, x$ci.ub)
        names(res.table) <- c("estimate", "se", "zval", "pval", 
            "ci.lb", "ci.ub")
        if (x$knha) {
            names(res.table)[3] <- c("tval")
        }
        res.table <- formatC(res.table, digits = digits, format = "f")
        signif <- symnum(x$pval, corr = FALSE, na = FALSE, cutpoints = c(0, 
            0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
            "*", ".", " "))
        res.table <- c(formatC(res.table, digits = digits, format = "f"), 
            signif)
        names(res.table)[7] <- ""
        res.table[4][x$pval > ncutoff] <- formatC(x$pval[x$pval > 
            ncutoff], digits = digits, format = "f")
        res.table[4][x$pval < ncutoff] <- paste("<", cutoff, 
            sep = "", collapse = "")
    }
    else {
        res.table <- cbind(x$b, x$se, x$zval, x$pval, x$ci.lb, 
            x$ci.ub)
        dimnames(res.table)[[2]] <- c("estimate", "se", "zval", 
            "pval", "ci.lb", "ci.ub")
        if (x$knha) {
            dimnames(res.table)[[2]][3] <- c("tval")
        }
        signif <- symnum(x$pval, corr = FALSE, na = FALSE, cutpoints = c(0, 
            0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
            "*", ".", " "))
        res.table <- cbind(formatC(res.table, digits = digits, 
            format = "f"), signif)
        dimnames(res.table)[[2]][7] <- ""
        res.table[x$pval > ncutoff, 4] <- formatC(x$pval[x$pval > 
            ncutoff], digits = digits, format = "f")
        res.table[x$pval < ncutoff, 4] <- paste("<", cutoff, 
            sep = "", collapse = "")
    }
    cat("Model Results:")
    cat("\n\n")
    if (x$int.only) {
        print(res.table, quote = FALSE, right = TRUE)
    }
    else {
        print(res.table, quote = FALSE, right = TRUE, print.gap = 2)
    }
    cat("\n")
    if (signif.legend == TRUE) {
        cat("---\nSignif. codes: ", attr(signif, "legend"), "\n\n")
    }
    invisible()
}
qqnorm.rma.mh <-
function (y, type = "rstandard", pch = 19, ...) 
{
    if (!is.element("rma.mh", class(y))) 
        stop("Argument 'y' must be an object of class \"rma.mh\".")
    type <- match.arg(type, c("rstandard", "rstudent"))
    if (type == "rstandard") {
        res <- rstandard(y)
        not.na <- !is.na(res$z)
        zi <- res$z[not.na]
    }
    else {
        res <- rstudent(y)
        not.na <- !is.na(res$z)
        zi <- res$z[not.na]
    }
    qqnorm(zi, pch = pch, bty = "l", ...)
    abline(a = 0, b = 1, lty = "solid", ...)
    invisible()
}
qqnorm.rma.peto <-
function (y, type = "rstandard", pch = 19, ...) 
{
    if (!is.element("rma.peto", class(y))) 
        stop("Argument 'y' must be an y of class \"rma.peto\".")
    type <- match.arg(type, c("rstandard", "rstudent"))
    if (type == "rstandard") {
        res <- rstandard(y)
        not.na <- !is.na(res$z)
        zi <- res$z[not.na]
    }
    else {
        res <- rstudent(y)
        not.na <- !is.na(res$z)
        zi <- res$z[not.na]
    }
    qqnorm(zi, pch = pch, bty = "l", ...)
    abline(a = 0, b = 1, lty = "solid", ...)
    invisible()
}
qqnorm.rma.uni <-
function (y, type = "rstandard", pch = 19, envelope = TRUE, level = y$level, 
    bonferroni = FALSE, reps = 1000, smooth = TRUE, bass = 0, 
    ...) 
{
    if (!is.element("rma.uni", class(y))) 
        stop("Argument 'y' must be an y of class \"rma.uni\".")
    type <- match.arg(type, c("rstandard", "rstudent"))
    if (type == "rstandard") {
        res <- rstandard(y)
        not.na <- !is.na(res$z)
        zi <- res$z[not.na]
    }
    else {
        res <- rstudent(y)
        not.na <- !is.na(res$z)
        zi <- res$z[not.na]
    }
    qqnorm(zi, pch = pch, bty = "l", ...)
    abline(a = 0, b = 1, lty = "solid", ...)
    if (envelope) {
        alpha <- (100 - level)/100
        x <- y
        dat <- matrix(rnorm(x$k * reps), nrow = x$k, ncol = reps)
        if (x$weighted) {
            wi <- 1/(x$vi + x$tau2)
            W <- diag(wi)
            stXWX <- .invcalc(X = x$X, W = W, k = x$k)
            H <- x$X %*% stXWX %*% crossprod(x$X, W)
        }
        else {
            stXX <- .invcalc(X = x$X, W = diag(x$k), k = x$k)
            H <- x$X %*% tcrossprod(stXX, x$X)
        }
        ImH <- diag(x$k) - H
        ei <- ImH %*% dat
        ei <- apply(ei, 2, sort)
        if (bonferroni) {
            lb <- apply(ei, 1, quantile, (alpha/2)/x$k)
            ub <- apply(ei, 1, quantile, 1 - (alpha/2)/x$k)
        }
        else {
            lb <- apply(ei, 1, quantile, (alpha/2))
            ub <- apply(ei, 1, quantile, 1 - (alpha/2))
        }
        temp <- qqnorm(lb, plot.it = FALSE)
        if (smooth) 
            temp <- supsmu(temp$x, temp$y, bass = bass)
        lines(temp$x, temp$y, lty = "dotted", ...)
        temp <- qqnorm(ub, plot.it = FALSE)
        if (smooth) 
            temp <- supsmu(temp$x, temp$y, bass = bass)
        lines(temp$x, temp$y, lty = "dotted", ...)
    }
    invisible()
}
radial <-
function (x, ...) 
UseMethod("radial")
radial.rma <-
function (x, center = FALSE, xlim = NULL, zlim = NULL, xlab = NULL, 
    zlab = NULL, atz = NULL, aty = NULL, steps = 7, level = x$level, 
    digits = 2, back = "lightgray", transf = FALSE, targs = NULL, 
    pch = 19, arc.res = 100, cex = NULL, ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    if (x$int.only) {
        yi <- x$yi
        yi.c <- yi
        vi <- x$vi
        b <- c(x$b)
        ci.lb <- x$ci.lb
        ci.ub <- x$ci.ub
        tau2 <- c(x$tau2)
        if (is.null(aty)) {
            atyis <- range(yi)
        }
        else {
            atyis <- range(aty)
            aty.c <- aty
        }
    }
    else {
        stop("Radial plots only applicable for models without moderators.")
    }
    if (center) {
        yi <- yi - x$b
        b <- 0
        ci.lb <- ci.lb - x$b
        ci.ub <- ci.ub - x$b
        atyis <- atyis - x$b
        if (!is.null(aty)) 
            aty <- aty - x$b
    }
    alpha <- (100 - level)/100
    zcrit <- qnorm(alpha/2, lower.tail = FALSE)
    zi <- yi/sqrt(vi + tau2)
    xi <- 1/sqrt(vi + tau2)
    if (is.null(xlim)) {
        xlims <- c(0, (1.3 * max(xi)))
    }
    else {
        xlims <- sort(xlim)
    }
    ci.xpos <- xlims[2] + 0.12 * (xlims[2] - xlims[1])
    ya.xpos <- xlims[2] + 0.14 * (xlims[2] - xlims[1])
    xaxismax <- xlims[2]
    if (is.null(zlim)) {
        zlims <- c(min(-5, 1.1 * min(zi), 1.1 * ci.lb * ci.xpos, 
            1.1 * min(atyis) * ya.xpos, 1.1 * min(yi) * ya.xpos, 
            -1.1 * zcrit + xaxismax * b), max(5, 1.1 * max(zi), 
            1.1 * ci.ub * ci.xpos, 1.1 * max(atyis) * ya.xpos, 
            1.1 * max(yi) * ya.xpos, 1.1 * zcrit + xaxismax * 
                b))
    }
    else {
        zlims <- sort(zlim)
    }
    par.mar <- par("mar")
    par.mar.adj <- par.mar - c(0, -3, 0, -5)
    par.mar.adj[par.mar.adj < 1] <- 1
    par(mar = par.mar.adj)
    on.exit(par(mar = par.mar))
    if (is.null(xlab)) {
        if (x$method == "FE") {
            xlab <- expression(x[i] == 1/sqrt(v[i]), ...)
        }
        else {
            xlab <- expression(x[i] == 1/sqrt(v[i] + tau^2), 
                ...)
        }
    }
    par.pty <- par("pty")
    par(pty = "s")
    on.exit(par(pty = par.pty), add = TRUE)
    plot(NA, NA, ylim = zlims, xlim = xlims, bty = "n", xaxt = "n", 
        yaxt = "n", xlab = xlab, ylab = "", xaxs = "i", yaxs = "i", 
        ...)
    if (is.null(cex)) 
        cex <- par("cex")
    polygon(c(0, xaxismax, xaxismax, 0), c(zcrit, zcrit + xaxismax * 
        b, -zcrit + xaxismax * b, -zcrit), border = NA, col = back, 
        ...)
    segments(0, 0, xaxismax, xaxismax * b, lty = "solid", ...)
    segments(0, -zcrit, xaxismax, -zcrit + xaxismax * b, lty = "dotted", 
        ...)
    segments(0, zcrit, xaxismax, zcrit + xaxismax * b, lty = "dotted", 
        ...)
    axis(side = 1, ...)
    if (is.null(atz)) {
        axis(side = 2, at = seq(-4, 4, length = 9), labels = NA, 
            las = 1, tcl = par("tcl")/2, ...)
        axis(side = 2, at = seq(-2, 2, length = 3), las = 1, 
            ...)
    }
    else {
        axis(side = 2, at = atz, labels = atz, las = 1, ...)
    }
    if (is.null(zlab)) {
        if (center) {
            if (x$method == "FE") {
                mtext(expression(z[i] == frac(y[i] - hat(theta), 
                  sqrt(v[i]))), side = 2, line = par.mar.adj[2] - 
                  1, at = 0, adj = 0, las = 1, cex = cex, ...)
            }
            else {
                mtext(expression(z[i] == frac(y[i] - hat(mu), 
                  sqrt(v[i] + tau^2))), side = 2, line = par.mar.adj[2] - 
                  1, adj = 0, at = 0, las = 1, cex = cex, ...)
            }
        }
        else {
            if (x$method == "FE") {
                mtext(expression(z[i] == frac(y[i], sqrt(v[i]))), 
                  side = 2, line = par.mar.adj[2] - 2, at = 0, 
                  adj = 0, las = 1, cex = cex, ...)
            }
            else {
                mtext(expression(z[i] == frac(y[i], sqrt(v[i] + 
                  tau^2))), side = 2, line = par.mar.adj[2] - 
                  1, at = 0, adj = 0, las = 1, cex = cex, ...)
            }
        }
    }
    else {
        mtext(zlab, side = 2, line = par.mar.adj[2] - 4, at = 0, 
            cex = cex, ...)
    }
    par.xpd <- par("xpd")
    par(xpd = TRUE)
    par.usr <- par("usr")
    asp.rat <- (par.usr[4] - par.usr[3])/(par.usr[2] - par.usr[1])
    if (length(arc.res) == 1L) 
        arc.res <- c(arc.res, arc.res/4)
    if (is.null(aty)) {
        atyis <- seq(min(yi), max(yi), length = arc.res[1])
    }
    else {
        atyis <- seq(min(aty), max(aty), length = arc.res[1])
    }
    len <- ya.xpos
    xis <- rep(NA, length(atyis))
    zis <- rep(NA, length(atyis))
    for (i in 1:length(atyis)) {
        xis[i] <- sqrt(len^2/(1 + (atyis[i]/asp.rat)^2))
        zis[i] <- xis[i] * atyis[i]
    }
    valid <- zis > zlims[1] & zis < zlims[2]
    lines(xis[valid], zis[valid], ...)
    if (is.null(aty)) {
        atyis <- seq(min(yi), max(yi), length = steps)
    }
    else {
        atyis <- aty
    }
    len.l <- ya.xpos
    len.u <- ya.xpos + 0.015 * (xlims[2] - xlims[1])
    xis.l <- rep(NA, length(atyis))
    zis.l <- rep(NA, length(atyis))
    xis.u <- rep(NA, length(atyis))
    zis.u <- rep(NA, length(atyis))
    for (i in 1:length(atyis)) {
        xis.l[i] <- sqrt(len.l^2/(1 + (atyis[i]/asp.rat)^2))
        zis.l[i] <- xis.l[i] * atyis[i]
        xis.u[i] <- sqrt(len.u^2/(1 + (atyis[i]/asp.rat)^2))
        zis.u[i] <- xis.u[i] * atyis[i]
    }
    valid <- zis.l > zlims[1] & zis.u > zlims[1] & zis.l < zlims[2] & 
        zis.u < zlims[2]
    if (any(valid)) 
        segments(xis.l[valid], zis.l[valid], xis.u[valid], (xis.u * 
            atyis)[valid], ...)
    if (is.null(aty)) {
        atyis <- seq(min(yi), max(yi), length = steps)
        atyis.lab <- seq(min(yi.c), max(yi.c), length = steps)
    }
    else {
        atyis <- aty
        atyis.lab <- aty.c
    }
    len <- ya.xpos + 0.02 * (xlims[2] - xlims[1])
    xis <- rep(NA, length(atyis))
    zis <- rep(NA, length(atyis))
    for (i in 1:length(atyis)) {
        xis[i] <- sqrt(len^2/(1 + (atyis[i]/asp.rat)^2))
        zis[i] <- xis[i] * atyis[i]
    }
    if (is.function(transf)) {
        if (is.null(targs)) {
            atyis.lab <- sapply(atyis.lab, transf)
        }
        else {
            atyis.lab <- sapply(atyis.lab, transf, targs)
        }
    }
    valid <- zis > zlims[1] & zis < zlims[2]
    if (any(valid)) 
        text(xis[valid], zis[valid], formatC(atyis.lab[valid], 
            digits = digits, format = "f"), pos = 4, cex = cex, 
            ...)
    atyis <- seq(ci.lb, ci.ub, length = arc.res[2])
    len <- ci.xpos
    xis <- rep(NA, length(atyis))
    zis <- rep(NA, length(atyis))
    for (i in 1:length(atyis)) {
        xis[i] <- sqrt(len^2/(1 + (atyis[i]/asp.rat)^2))
        zis[i] <- xis[i] * atyis[i]
    }
    valid <- zis > zlims[1] & zis < zlims[2]
    if (any(valid)) 
        lines(xis[valid], zis[valid], ...)
    atyis <- c(ci.lb, b, ci.ub)
    len.l <- ci.xpos - 0.007 * (xlims[2] - xlims[1])
    len.u <- ci.xpos + 0.007 * (xlims[2] - xlims[1])
    xis.l <- rep(NA, 3)
    zis.l <- rep(NA, 3)
    xis.u <- rep(NA, 3)
    zis.u <- rep(NA, 3)
    for (i in 1:length(atyis)) {
        xis.l[i] <- sqrt(len.l^2/(1 + (atyis[i]/asp.rat)^2))
        zis.l[i] <- xis.l[i] * atyis[i]
        xis.u[i] <- sqrt(len.u^2/(1 + (atyis[i]/asp.rat)^2))
        zis.u[i] <- xis.u[i] * atyis[i]
    }
    valid <- zis.l > zlims[1] & zis.u > zlims[1] & zis.l < zlims[2] & 
        zis.u < zlims[2]
    if (any(valid)) 
        segments(xis.l[valid], zis.l[valid], xis.u[valid], (xis.u * 
            atyis)[valid], ...)
    par(xpd = par.xpd)
    points(xi, zi, pch = pch, cex = cex, ...)
    invisible()
}
ranktest <-
function (x, ...) 
UseMethod("ranktest")
ranktest.rma <-
function (x, ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    yi <- x$yi
    vi <- x$vi
    res <- rma(yi, vi, method = "FE")
    b <- res$b
    vb <- res$vb
    vi.star <- vi - c(vb)
    yi.star <- (yi - c(b))/sqrt(vi.star)
    res <- cor.test(yi.star, vi, method = "kendall", exact = TRUE)
    pval <- res$p.value
    tau <- c(res$estimate)
    res <- list(tau, pval, x$digits)
    names(res) <- c("tau", "pval", "digits")
    class(res) <- c("ranktest.rma")
    return(res)
}
regtest <-
function (x, ...) 
UseMethod("regtest")
regtest.rma <-
function (x, model = "rma", predictor = "sei", ni = NULL, ...) 
{
    if (!is.element("rma", class(x))) 
        stop("Argument 'x' must be an object of class \"rma\".")
    model <- match.arg(model, c("lm", "rma"))
    predictor <- match.arg(predictor, c("sei", "vi", "ni", "ninv"))
    yi <- x$yi
    vi <- x$vi
    X <- x$X
    p <- x$p
    if (is.null(ni)) {
        ni <- x$ni
    }
    if (predictor == "sei") {
        X <- cbind(X, sei = sqrt(vi))
    }
    if (predictor == "vi") {
        X <- cbind(X, vi = vi)
    }
    if (predictor == "ni" || predictor == "ninv") {
        if (is.null(ni)) {
            stop("Need total sample size (ni) to carry out the test.")
        }
        else {
            ni <- c(scale(ni))
            if (predictor == "ni") {
                X <- cbind(X, ni = ni)
            }
            else {
                X <- cbind(X, ninv = 1/ni)
            }
        }
    }
    if (model == "rma") {
        res <- rma(yi, vi, mods = X, method = x$method, weighted = x$weighted, 
            intercept = FALSE, knha = x$knha, control = x$control, 
            ...)
        zval <- res$zval[p + 1]
        pval <- res$pval[p + 1]
        dfs <- ifelse(x$knha == TRUE, res$k - res$p, NA)
    }
    else {
        res <- lm(yi ~ X - 1, weights = 1/vi)
        res <- summary(res)
        zval <- coef(res)[p + 1, 3]
        pval <- coef(res)[p + 1, 4]
        dfs <- x$k - x$p - 1
    }
    res <- list(model, predictor, zval, pval, dfs, x$method, 
        x$digits)
    names(res) <- c("model", "predictor", "zval", "pval", "dfs", 
        "method", "digits")
    class(res) <- c("regtest.rma")
    return(res)
}
residuals.rma <-
function (object, ...) 
{
    if (!is.element("rma", class(object))) 
        stop("Argument 'object' must be an object of class \"rma\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- object
    out <- c(x$yi.f - x$X.f %*% x$b)
    out[abs(out) < 100 * .Machine$double.eps] <- 0
    names(out) <- x$slab
    if (na.act == "na.omit") {
        out <- na.omit(out)
        attr(out, "na.action") <- NULL
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    return(out)
}
rma <-
function (yi, vi, sei, ai, bi, ci, di, n1i, n2i, m1i, m2i, sd1i, 
    sd2i, xi, mi, ri, ni, mods = NULL, data = NULL, intercept = TRUE, 
    slab = NULL, subset = NULL, measure = "GEN", add = 1/2, to = "only0", 
    vtype = "LS", method = "REML", weighted = TRUE, level = 95, 
    digits = 4, btt = NULL, tau2 = NULL, knha = FALSE, control = list()) 
{
    if (!is.element(measure, c("GEN", "MD", "SMD", "RR", "OR", 
        "PETO", "RD", "AS", "PHI", "YUQ", "YUY", "PR", "PLN", 
        "PLO", "PAS", "PFT", "COR", "UCOR", "ZCOR"))) 
        stop("Unknown 'measure' specified.")
    if (!is.element(method, c("FE", "HS", "HE", "DL", 
        "SJ", "ML", "REML", "EB"))) 
        stop("Unknown 'method' specified.")
    if (is.null(data)) {
        data <- sys.frame(sys.parent())
    }
    else {
        if (!is.data.frame(data)) {
            data <- data.frame(data)
        }
    }
    mf <- match.call()
    mf.slab <- mf[[match("slab", names(mf))]]
    mf.subset <- mf[[match("subset", names(mf))]]
    mf.mods <- mf[[match("mods", names(mf))]]
    slab <- eval(mf.slab, data)
    subset <- eval(mf.subset, data)
    mods <- eval(mf.mods, data)
    if (measure == "GEN") {
        mf.yi <- mf[[match("yi", names(mf))]]
        mf.vi <- mf[[match("vi", names(mf))]]
        mf.sei <- mf[[match("sei", names(mf))]]
        yi <- eval(mf.yi, data)
        vi <- eval(mf.vi, data)
        sei <- eval(mf.sei, data)
        ni <- NULL
        if (is.null(vi)) {
            vi <- sei^2
        }
        if (length(yi) != length(vi)) 
            stop("Length of yi and vi (or sei) is not the same.")
    }
    else {
        if (is.element(measure, c("RR", "OR", "PETO", "RD", "AS", 
            "PHI", "YUQ", "YUY"))) {
            mf.ai <- mf[[match("ai", names(mf))]]
            mf.bi <- mf[[match("bi", names(mf))]]
            mf.ci <- mf[[match("ci", names(mf))]]
            mf.di <- mf[[match("di", names(mf))]]
            mf.n1i <- mf[[match("n1i", names(mf))]]
            mf.n2i <- mf[[match("n2i", names(mf))]]
            ai <- eval(mf.ai, data)
            bi <- eval(mf.bi, data)
            ci <- eval(mf.ci, data)
            di <- eval(mf.di, data)
            n1i <- eval(mf.n1i, data)
            n2i <- eval(mf.n2i, data)
            if (is.null(bi)) {
                bi <- n1i - ai
            }
            if (is.null(di)) {
                di <- n2i - ci
            }
            ni <- ai + bi + ci + di
            dat <- escalc(measure, ai = ai, bi = bi, ci = ci, 
                di = di, add = add, to = to)
        }
        if (is.element(measure, c("MD", "SMD"))) {
            mf.m1i <- mf[[match("m1i", names(mf))]]
            mf.m2i <- mf[[match("m2i", names(mf))]]
            mf.sd1i <- mf[[match("sd1i", names(mf))]]
            mf.sd2i <- mf[[match("sd2i", names(mf))]]
            mf.n1i <- mf[[match("n1i", names(mf))]]
            mf.n2i <- mf[[match("n2i", names(mf))]]
            m1i <- eval(mf.m1i, data)
            m2i <- eval(mf.m2i, data)
            sd1i <- eval(mf.sd1i, data)
            sd2i <- eval(mf.sd2i, data)
            n1i <- eval(mf.n1i, data)
            n2i <- eval(mf.n2i, data)
            ni <- n1i + n2i
            dat <- escalc(measure, m1i = m1i, m2i = m2i, sd1i = sd1i, 
                sd2i = sd2i, n1i = n1i, n2i = n2i, vtype = vtype)
        }
        if (is.element(measure, c("PR", "PLN", "PLO", "PAS", 
            "PFT"))) {
            mf.xi <- mf[[match("xi", names(mf))]]
            mf.mi <- mf[[match("mi", names(mf))]]
            mf.ni <- mf[[match("ni", names(mf))]]
            xi <- eval(mf.xi, data)
            mi <- eval(mf.mi, data)
            ni <- eval(mf.ni, data)
            if (is.null(mi)) {
                mi <- ni - xi
            }
            ni <- xi + mi
            dat <- escalc(measure, xi = xi, mi = mi, add = add, 
                to = to)
        }
        if (is.element(measure, c("COR", "UCOR", "ZCOR"))) {
            mf.ri <- mf[[match("ri", names(mf))]]
            mf.ni <- mf[[match("ni", names(mf))]]
            ri <- eval(mf.ri, data)
            ni <- eval(mf.ni, data)
            dat <- escalc(measure, ri = ri, ni = ni, vtype = vtype)
        }
        yi <- dat$yi
        vi <- dat$vi
    }
    if (is.vector(mods)) 
        mods <- cbind(mods)
    if (is.data.frame(mods)) 
        mods <- as.matrix(mods)
    k <- length(yi)
    ids <- 1:k
    if (is.null(slab)) {
        slab.null <- TRUE
        slab <- 1:k
    }
    else {
        if (length(slab) != unique(length(slab))) 
            stop("Study labels must be unique.")
        if (length(slab) != length(yi)) 
            stop("Study labels not of same length as data.")
        slab.null <- FALSE
    }
    if (!is.null(subset)) {
        yi <- yi[subset]
        vi <- vi[subset]
        ni <- ni[subset]
        mods <- mods[subset, , drop = FALSE]
        slab <- slab[subset]
        ids <- ids[subset]
        k <- length(yi)
    }
    yi.f <- yi
    vi.f <- vi
    ni.f <- ni
    mods.f <- mods
    k.f <- k
    YVM.na <- is.na(cbind(yi, vi, mods))
    if (any(YVM.na)) {
        na.act <- getOption("na.action")
        if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
            stop("Unknwn 'na.action' specified under options().")
        not.na <- apply(YVM.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            yi <- yi[not.na]
            vi <- vi[not.na]
            ni <- ni[not.na]
            mods <- mods[not.na, , drop = FALSE]
            k <- length(yi)
            warning("Cases with NAs omitted from model fitting.")
        }
        if (na.act == "na.fail") 
            stop("Missing values in data.")
    }
    else {
        not.na <- rep(TRUE, k)
    }
    if (k <= 1) 
        stop("Processing terminated since k <= 1.")
    if (any(vi <= 0)) {
        allvipos <- FALSE
        warning("There are outcomes with non-positive sampling variances.")
        if (any(vi < 0)) {
            vi[vi <= 0] <- 0
            warning("Negative sampling variances constrained to zero.")
        }
    }
    else {
        allvipos <- TRUE
    }
    if (is.null(mods) && !intercept) {
        warning("Must either include an intercept (intercept=TRUE) and/or moderators in model.\n  Coerced intercept into the model.")
        intercept <- TRUE
    }
    if (intercept) {
        X <- cbind(intrcpt = rep(1, k), mods)
        X.f <- cbind(intrcpt = rep(1, k.f), mods.f)
    }
    else {
        X <- mods
        X.f <- mods.f
    }
    p <- dim(X)[2]
    if (method == "FE") {
        if (p > k) {
            stop("The number of parameters to be estimated is larger than the number of observations.")
        }
    }
    else {
        if (!is.numeric(tau2)) {
            if (p + 1 > k) {
                stop("The number of parameters to be estimated is larger than the number of observations.")
            }
        }
        else {
            if (p > k) {
                stop("The number of parameters to be estimated is larger than the number of observations.")
            }
        }
    }
    if ((p == 1L) && (all(sapply(X, identical, 1)))) {
        int.only <- TRUE
    }
    else {
        int.only <- FALSE
    }
    if (is.null(btt)) {
        if (p > 1) {
            if (intercept) {
                btt <- 2:p
            }
            else {
                btt <- 1:p
            }
        }
        else {
            btt <- 1
        }
    }
    else {
        btt <- btt[(btt >= 1) & (btt <= p)]
        btt <- unique(round(btt))
        if (length(intersect(btt, 1:p)) == 0L) {
            stop("Non-existent coefficients specified with 'btt'.")
        }
    }
    bntt <- setdiff(1:p, btt)
    m <- length(btt)
    con <- list(tau2.init = NULL, tau2.min = 0, tau2.max = 50, 
        threshold = 10^-5, maxit = 50, stepadj = 1, verbose = FALSE)
    con[pmatch(names(control), names(con))] <- control
    se.tau2 <- I2 <- H2 <- QE <- QEp <- NA
    s2w <- 1
    Y <- as.matrix(yi)
    alpha <- (100 - level)/100
    if (!is.numeric(tau2)) {
        if (method == "HS") {
            if (!allvipos) 
                stop("HS estimator cannot be used with non-positive sampling variances.")
            wi <- 1/vi
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            RSS <- crossprod(Y, P) %*% Y
            tau2 <- RSS/sum(wi) - k/sum(wi)
            se.tau2 <- sqrt(1/sum(wi)^2 * (2 * (k - p) + 4 * 
                max(tau2, 0) * .tr(P) + 2 * max(tau2, 0)^2 * 
                .tr(P %*% P)))
        }
        if (is.element(method, c("HE", "ML", "REML", "EB"))) {
            stXX <- .invcalc(X = X, W = diag(k), k = k)
            P <- diag(k) - X %*% tcrossprod(stXX, X)
            RSS <- crossprod(Y, P) %*% Y
            trPV <- .tr(P %*% diag(vi))
            tau2 <- (RSS - trPV)/(k - p)
            se.tau2 <- sqrt(1/(k - p)^2 * (2 * .tr(P %*% diag(vi) %*% 
                P %*% diag(vi)) + 4 * max(tau2, 0) * trPV + 2 * 
                max(tau2, 0)^2 * (k - p)))
        }
        if (method == "DL") {
            if (!allvipos) 
                stop("DL estimator cannot be used with non-positive sampling variances.")
            wi <- 1/vi
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            RSS <- crossprod(Y, P) %*% Y
            trP <- .tr(P)
            tau2 <- (RSS - (k - p))/trP
            se.tau2 <- sqrt(1/trP^2 * (2 * (k - p) + 4 * max(tau2, 
                0) * trP + 2 * max(tau2, 0)^2 * .tr(P %*% P)))
        }
        if (method == "SJ") {
            if (is.null(con$tau2.init)) {
                tau2 <- var(yi) * (k - 1)/k
            }
            else {
                tau2 <- con$tau2.init
            }
            wi <- 1/(vi + tau2)
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            RSS <- crossprod(Y, P) %*% Y
            tau2 <- tau2 * RSS/(k - p)
        }
        if (is.element(method, c("ML", "REML", "EB"))) {
            conv <- 1
            change <- con$threshold + 1
            iter <- 0
            if (is.null(con$tau2.init)) {
                tau2 <- max(0, tau2)
            }
            else {
                tau2 <- con$tau2.init
            }
            while (change > con$threshold) {
                if (con$verbose) 
                  cat("Iteration", iter, "\ttau^2 =", round(tau2, 
                    digits), "\n")
                iter <- iter + 1
                tau2.old <- tau2
                wi <- 1/(vi + tau2)
                W <- diag(wi)
                stXWX <- .invcalc(X = X, W = W, k = k)
                P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
                if (method == "ML") {
                  PP <- P %*% P
                  adj <- 1/sum(wi^2) * (crossprod(Y, PP) %*% 
                    Y - sum(wi))
                }
                if (method == "REML") {
                  PP <- P %*% P
                  adj <- 1/.tr(PP) * (crossprod(Y, PP) %*% Y - 
                    .tr(P))
                }
                if (method == "EB") {
                  adj <- 1/sum(wi) * (crossprod(Y, P) %*% Y * 
                    k/(k - p) - k)
                }
                adj <- adj * con$stepadj
                while (tau2 + adj < con$tau2.min) {
                  adj <- adj/2
                }
                tau2 <- tau2 + adj
                change <- abs(tau2.old - tau2)
                if (iter > con$maxit) {
                  conv <- 0
                  break
                }
            }
            if (conv == 0L) 
                stop("Fisher scoring algorithm did not converge. Try increasing the number of iterations (maxit), adjust the threshold (threshold), or use a different estimator for tau^2.")
            if (method == "ML") {
                se.tau2 <- sqrt(2/sum(wi^2))
            }
            if (method == "REML") {
                se.tau2 <- sqrt(2/.tr(PP))
            }
        }
        tau2 <- max(con$tau2.min, c(tau2))
    }
    else {
        if (method == "ML") {
            wi <- 1/(vi + tau2)
            se.tau2 <- sqrt(2/sum(wi^2))
        }
        if (method == "REML") {
            wi <- 1/(vi + tau2)
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            se.tau2 <- sqrt(2/.tr(P %*% P))
        }
    }
    if (method == "FE") {
        tau2 <- 0
        if (!allvipos && weighted) 
            stop("Weighted estimation cannot be used with a fixed-effects\n  model when there are non-positive sampling variances.")
    }
    if (con$verbose) 
        cat("Fisher scoring algorithm converged after", iter, 
            "iterations.", "\n\n")
    if (allvipos) {
        wi <- 1/vi
        W <- diag(wi)
        stXWX <- .invcalc(X = X, W = W, k = k)
        P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
        QE <- max(0, c(crossprod(Y, P) %*% Y))
        QEp <- 1 - pchisq(QE, df = k - p)
        if (int.only) {
            sumwi <- sum(wi)
            vi.avg <- (k - 1)/(sumwi - sum(wi^2)/sumwi)
            I2 <- 100 * tau2/(vi.avg + tau2)
            H2 <- tau2/vi.avg + 1
        }
    }
    wi <- 1/(vi + tau2)
    W <- diag(wi)
    if (weighted) {
        stXWX <- .invcalc(X = X, W = W, k = k)
        b <- stXWX %*% crossprod(X, W) %*% Y
        vb <- stXWX
        P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
        RSS.f <- crossprod(Y, P) %*% Y
        if (knha) {
            s2w <- c(RSS.f)/(k - p)
            vb <- s2w * vb
            if (method == "FE") 
                warning("The Knapp & Hartung (2003) method is not meant to be used in the context of fixed-effects models.")
        }
        if (length(bntt) == 0L) {
            QM <- c(sum(wi * yi^2) - RSS.f)/s2w
        }
        else {
            Xr <- X[, bntt, drop = FALSE]
            stXWX <- .invcalc(X = Xr, W = W, k = k)
            P <- W - W %*% Xr %*% stXWX %*% crossprod(Xr, W)
            RSS.r <- crossprod(Y, P) %*% Y
            QM <- c(RSS.r - RSS.f)/s2w
        }
    }
    else {
        stXX <- .invcalc(X = X, W = diag(k), k = k)
        b <- stXX %*% crossprod(X, Y)
        vb <- tcrossprod(stXX, X) %*% diag(vi + tau2) %*% X %*% 
            stXX
        P <- W - W %*% X %*% tcrossprod(stXX, X) - X %*% stXX %*% 
            crossprod(X, W) + X %*% stXX %*% crossprod(X, W) %*% 
            X %*% tcrossprod(stXX, X)
        RSS.f <- crossprod(Y, P) %*% Y
        if (knha) {
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            s2w <- c(crossprod(Y, P) %*% Y)/(k - p)
            vb <- s2w * vb
            if (method == "FE") 
                warning("The Knapp & Hartung (2003) method is not meant to be used in the context of fixed-effects models.")
        }
        QM <- t(b)[btt] %*% chol2inv(chol(vb[btt, btt])) %*% 
            b[btt]
    }
    se <- sqrt(diag(vb))
    names(se) <- NULL
    zval <- c(b/se)
    if (knha) {
        QM <- QM/m
        QMp <- 1 - pf(QM, df1 = m, df2 = k - p)
        pval <- 2 * (1 - pt(abs(zval), df = k - p))
        crit <- qt(1 - alpha/2, df = k - p)
    }
    else {
        QMp <- 1 - pchisq(QM, df = m)
        pval <- 2 * (1 - pnorm(abs(zval)))
        crit <- qnorm(1 - alpha/2)
    }
    ci.lb <- c(b - crit * se)
    ci.ub <- c(b + crit * se)
    ll.ML <- -1/2 * (k) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi + tau2)) - 1/2 * RSS.f
    ll.REML <- -1/2 * (k - p) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi + tau2)) - 1/2 * determinant(crossprod(X, 
        W) %*% X, logarithm = TRUE)$modulus - 1/2 * RSS.f
    dev.ML <- -2 * ll.ML
    dev.REML <- -2 * ll.REML
    AIC.ML <- -2 * ll.ML + 2 * (p + ifelse(method == "FE", 0, 
        1))
    BIC.ML <- -2 * ll.ML + (p + ifelse(method == "FE", 0, 1)) * 
        log(k)
    AIC.REML <- -2 * ll.REML + 2 * (p + ifelse(method == "FE", 
        0, 1))
    BIC.REML <- -2 * ll.REML + (p + ifelse(method == "FE", 0, 
        1)) * log(k - p)
    fit.stats <- matrix(c(ll.ML, dev.ML, AIC.ML, BIC.ML, ll.REML, 
        dev.REML, AIC.REML, BIC.REML), ncol = 2, byrow = FALSE)
    dimnames(fit.stats) <- list(c("ll", "dev", "AIC", "BIC"), 
        c("ML", "REML"))
    fit.stats <- data.frame(fit.stats)
    res <- list(b, se, zval, pval, ci.lb, ci.ub, vb, tau2, se.tau2, 
        k, k.f, p, m, QE, QEp, QM, QMp, I2, H2, int.only, yi, 
        vi, X, yi.f, vi.f, X.f, ni, ni.f, ids, not.na, slab, 
        slab.null, measure, method, weighted, knha, s2w, btt, 
        intercept, digits, level, con, fit.stats)
    names(res) <- c("b", "se", "zval", "pval", "ci.lb", "ci.ub", 
        "vb", "tau2", "se.tau2", "k", "k.f", "p", "m", "QE", 
        "QEp", "QM", "QMp", "I2", "H2", "int.only", "yi", "vi", 
        "X", "yi.f", "vi.f", "X.f", "ni", "ni.f", "ids", "not.na", 
        "slab", "slab.null", "measure", "method", "weighted", 
        "knha", "s2w", "btt", "intercept", "digits", "level", 
        "control", "fit.stats")
    class(res) <- c("rma.uni", "rma")
    return(res)
}
rma.mh <-
function (ai, bi, ci, di, n1i, n2i, data = NULL, slab = NULL, 
    subset = NULL, measure = "OR", add = c(1/2, 0), to = c("only0", 
        "none"), level = 95, digits = 4) 
{
    if (!is.element(measure, c("OR", "RR", "RD"))) 
        stop("Mantel-Haenszel method can only be used with measures OR, RR, and RD.")
    if (length(add) != 2) 
        stop("Argument 'add' should specify two values (see 'help(rma.mh)').")
    if (length(to) != 2) 
        stop("Argument 'to' should specify two values (see 'help(rma.mh)').")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (!is.element(to[1], c("all", "only0", "if0all", "none"))) 
        stop("Unknown 'to' argument specified.")
    if (!is.element(to[2], c("all", "only0", "if0all", "none"))) 
        stop("Unknown 'to' argument specified.")
    if (is.null(data)) {
        data <- sys.frame(sys.parent())
    }
    else {
        if (!is.data.frame(data)) {
            data <- data.frame(data)
        }
    }
    mf <- match.call()
    mf.slab <- mf[[match("slab", names(mf))]]
    mf.subset <- mf[[match("subset", names(mf))]]
    slab <- eval(mf.slab, data)
    subset <- eval(mf.subset, data)
    mf.ai <- mf[[match("ai", names(mf))]]
    mf.bi <- mf[[match("bi", names(mf))]]
    mf.ci <- mf[[match("ci", names(mf))]]
    mf.di <- mf[[match("di", names(mf))]]
    mf.n1i <- mf[[match("n1i", names(mf))]]
    mf.n2i <- mf[[match("n2i", names(mf))]]
    ai <- eval(mf.ai, data)
    bi <- eval(mf.bi, data)
    ci <- eval(mf.ci, data)
    di <- eval(mf.di, data)
    n1i <- eval(mf.n1i, data)
    n2i <- eval(mf.n2i, data)
    if (is.null(bi)) {
        bi <- n1i - ai
    }
    if (is.null(di)) {
        di <- n2i - ci
    }
    ni <- ai + bi + ci + di
    k <- length(ai)
    ids <- 1:k
    if (is.null(slab)) {
        slab.null <- TRUE
        slab <- 1:k
    }
    else {
        if (length(slab) != unique(length(slab))) 
            stop("Study labels must be unique.")
        if (length(slab) != length(ai)) 
            stop("Study labels not of same length as data.")
        slab.null <- FALSE
    }
    if (!is.null(subset)) {
        ai <- ai[subset]
        bi <- bi[subset]
        ci <- ci[subset]
        di <- di[subset]
        ni <- ni[subset]
        slab <- slab[subset]
        ids <- ids[subset]
        k <- length(ai)
    }
    k.f <- k
    dat <- escalc(measure, ai = ai, bi = bi, ci = ci, di = di, 
        add = add[1], to = to[1])
    yi <- dat$yi
    vi <- dat$vi
    ai.f <- ai
    bi.f <- bi
    ci.f <- ci
    di.f <- di
    yi.f <- yi
    vi.f <- vi
    ni.f <- ni
    aibicidi.na <- is.na(cbind(ai, bi, ci, di))
    if (any(aibicidi.na)) {
        not.na <- apply(aibicidi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            ai <- ai[not.na]
            bi <- bi[not.na]
            ci <- ci[not.na]
            di <- di[not.na]
            k <- length(ai)
            warning("Tables with NAs omitted from model fitting.")
        }
        if (na.act == "na.fail") 
            stop("Missing values in tables.")
    }
    else {
        not.na <- rep(TRUE, k)
    }
    if (k <= 1) 
        stop("Processing terminated since k <= 1.")
    yivi.na <- is.na(cbind(yi, vi))
    if (any(yivi.na)) {
        not.na.yivi <- apply(yivi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            yi <- yi[not.na.yivi]
            vi <- vi[not.na.yivi]
            ni <- ni[not.na.yivi]
            warning("Some yi/vi values are NA.")
        }
        if (na.act == "na.fail") 
            stop("Missing yi/vi values.")
    }
    else {
        not.na.yivi <- rep(TRUE, k)
    }
    k.yi <- length(yi)
    if (to[2] == "all") {
        ai <- ai + add[2]
        bi <- bi + add[2]
        ci <- ci + add[2]
        di <- di + add[2]
    }
    if (to[2] == "only0") {
        id0 <- c(ai == 0L | bi == 0L | ci == 0L | di == 0L)
        ai[id0] <- ai[id0] + add[2]
        bi[id0] <- bi[id0] + add[2]
        ci[id0] <- ci[id0] + add[2]
        di[id0] <- di[id0] + add[2]
    }
    if (to[2] == "if0all") {
        id0 <- c(ai == 0L | bi == 0L | ci == 0L | di == 0L)
        if (any(id0)) {
            ai <- ai + add[2]
            bi <- bi + add[2]
            ci <- ci + add[2]
            di <- di + add[2]
        }
    }
    alpha <- (100 - level)/100
    n1i <- ai + bi
    n2i <- ci + di
    Ni <- ai + bi + ci + di
    if (measure == "OR") {
        Pi <- ai/Ni + di/Ni
        Qi <- bi/Ni + ci/Ni
        Ri <- (ai/Ni) * di
        Si <- (bi/Ni) * ci
        R <- sum(Ri)
        S <- sum(Si)
        if (identical(R, 0) || identical(S, 0)) {
            b.exp <- NA
            b <- NA
            se <- NA
            zval <- NA
            pval <- NA
            ci.lb <- NA
            ci.ub <- NA
        }
        else {
            b.exp <- R/S
            b <- log(b.exp)
            se <- sqrt(1/2 * (sum(Pi * Ri)/R^2 + sum(Pi * Si + 
                Qi * Ri)/(R * S) + sum(Qi * Si)/S^2))
            zval <- b/se
            pval <- 2 * (1 - pnorm(abs(zval)))
            ci.lb <- b - qnorm(1 - alpha/2) * se
            ci.ub <- b + qnorm(1 - alpha/2) * se
        }
        names(b) <- "intrcpt"
        vb <- matrix(se^2, dimnames = list("intrcpt", "intrcpt"))
        xt <- ai + ci
        yt <- bi + di
        if (identical(sum(xt), 0) || identical(sum(yt), 0)) {
            CO <- NA
            COp <- NA
            CMH <- NA
            CMHp <- NA
        }
        else {
            CO <- (abs(sum(ai - (n1i/Ni) * xt)))^2/sum((n1i/Ni) * 
                (n2i/Ni) * (xt * yt/Ni))
            COp <- pchisq(CO, df = 1, lower.tail = FALSE)
            CMH <- (abs(sum(ai - (n1i/Ni) * xt)) - 0.5)^2/sum((n1i/Ni) * 
                (n2i/Ni) * (xt * yt/(Ni - 1)))
            CMHp <- pchisq(CMH, df = 1, lower.tail = FALSE)
        }
        if (is.na(b)) {
            BD <- NA
            TA <- NA
            BDp <- NA
            TAp <- NA
            k.pos <- 0
        }
        else {
            if (identical(b.exp, 1)) {
                N11 <- (n1i/Ni) * xt
            }
            else {
                A <- b.exp * (n1i + xt) + (n2i - xt)
                B <- sqrt(A^2 - 4 * n1i * xt * b.exp * (b.exp - 
                  1))
                N11 <- (A - B)/(2 * (b.exp - 1))
            }
            pos <- (N11 > 0) & (xt > 0) & (yt > 0)
            k.pos <- sum(pos)
            N11 <- N11[pos]
            N12 <- n1i[pos] - N11
            N21 <- xt[pos] - N11
            N22 <- N11 - n1i[pos] - xt[pos] + Ni[pos]
            BD <- sum((ai[pos] - N11)^2/(1/N11 + 1/N12 + 1/N21 + 
                1/N22)^(-1))
            TA <- BD - sum(ai[pos] - N11)^2/sum((1/N11 + 1/N12 + 
                1/N21 + 1/N22)^(-1))
            if (k.pos > 1) {
                BDp <- pchisq(BD, df = k.pos - 1, lower.tail = FALSE)
                TAp <- pchisq(TA, df = k.pos - 1, lower.tail = FALSE)
            }
            else {
                BDp <- NA
                TAp <- NA
            }
        }
    }
    if (measure == "RR") {
        R <- sum(ai * (n2i/Ni))
        S <- sum(ci * (n1i/Ni))
        if (identical(sum(ai), 0) || identical(sum(ci), 0)) {
            b <- NA
            se <- NA
            zval <- NA
            pval <- NA
            ci.lb <- NA
            ci.ub <- NA
        }
        else {
            b <- log(sum(ai * (n2i/Ni))/sum(ci * (n1i/Ni)))
            se <- sqrt(sum(((n1i/Ni) * (n2i/Ni) * (ai + ci) - 
                (ai/Ni) * ci))/(R * S))
            zval <- b/se
            pval <- 2 * (1 - pnorm(abs(zval)))
            ci.lb <- b - qnorm(1 - alpha/2) * se
            ci.ub <- b + qnorm(1 - alpha/2) * se
        }
        names(b) <- "intrcpt"
        vb <- matrix(se^2, dimnames = list("intrcpt", "intrcpt"))
        CO <- COp <- CMH <- CMHp <- BD <- BDp <- TA <- TAp <- k.pos <- NA
    }
    if (measure == "RD") {
        b <- sum(ai * (n2i/Ni) - ci * (n1i/Ni))/sum(n1i * (n2i/Ni))
        se <- sqrt(sum(((ai/Ni^2) * bi * (n2i^2/n1i) + (ci/Ni^2) * 
            di * (n1i^2/n2i)))/sum(n1i * (n2i/Ni))^2)
        zval <- b/se
        pval <- 2 * (1 - pnorm(abs(zval)))
        ci.lb <- b - qnorm(1 - alpha/2) * se
        ci.ub <- b + qnorm(1 - alpha/2) * se
        names(b) <- "intrcpt"
        vb <- matrix(se^2, dimnames = list("intrcpt", "intrcpt"))
        CO <- COp <- CMH <- CMHp <- BD <- BDp <- TA <- TAp <- k.pos <- NA
    }
    wi <- 1/vi
    QE <- sum(wi * (yi - b)^2)
    QEp <- 1 - pchisq(QE, df = k.yi - 1)
    ll.ML <- -1/2 * (k.yi) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi)) - 1/2 * QE
    ll.REML <- -1/2 * (k.yi - 1) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi)) - 1/2 * log(sum(wi)) - 1/2 * QE
    dev.ML <- -2 * ll.ML
    dev.REML <- -2 * ll.REML
    AIC.ML <- -2 * ll.ML + 2
    BIC.ML <- -2 * ll.ML + log(k.yi)
    AIC.REML <- -2 * ll.REML + 2
    BIC.REML <- -2 * ll.REML + log(k.yi - 1)
    fit.stats <- matrix(c(ll.ML, dev.ML, AIC.ML, BIC.ML, ll.REML, 
        dev.REML, AIC.REML, BIC.REML), ncol = 2, byrow = FALSE)
    dimnames(fit.stats) <- list(c("ll", "dev", "AIC", "BIC"), 
        c("ML", "REML"))
    fit.stats <- data.frame(fit.stats)
    tau2 <- 0
    X.f <- cbind(rep(1, k.f))
    int.only <- TRUE
    method <- "FE"
    weighted <- TRUE
    knha <- FALSE
    res <- list(b, se, zval, pval, ci.lb, ci.ub, vb, tau2, k, 
        k.f, k.yi, k.pos, QE, QEp, CO, COp, CMH, CMHp, BD, BDp, 
        TA, TAp, int.only, yi, vi, yi.f, vi.f, X.f, ai, bi, ci, 
        di, ai.f, bi.f, ci.f, di.f, ni, ni.f, ids, not.na, not.na.yivi, 
        slab, slab.null, measure, method, weighted, knha, digits, 
        level, add, to, fit.stats)
    names(res) <- c("b", "se", "zval", "pval", "ci.lb", "ci.ub", 
        "vb", "tau2", "k", "k.f", "k.yi", "k.pos", "QE", "QEp", 
        "CO", "COp", "CMH", "CMHp", "BD", "BDp", "TA", "TAp", 
        "int.only", "yi", "vi", "yi.f", "vi.f", "X.f", "ai", 
        "bi", "ci", "di", "ai.f", "bi.f", "ci.f", "di.f", "ni", 
        "ni.f", "ids", "not.na", "not.na.yivi", "slab", "slab.null", 
        "measure", "method", "weighted", "knha", "digits", "level", 
        "add", "to", "fit.stats")
    #class(res) <- c("rma.mh", "rma")
    return(res)
}
rma.peto <-
function (ai, bi, ci, di, n1i, n2i, data = NULL, slab = NULL, 
    subset = NULL, add = c(1/2, 0), to = c("only0", "none"), 
    level = 95, digits = 4) 
{
    if (length(add) != 2) 
        stop("Argument 'add' should specify two values (see 'help(rma.peto)').")
    if (length(to) != 2) 
        stop("Argument 'to' should specify two values (see 'help(rma.peto)').")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    if (!is.element(to[1], c("all", "only0", "if0all", "none"))) 
        stop("Unknown 'to' argument specified.")
    if (!is.element(to[2], c("all", "only0", "if0all", "none"))) 
        stop("Unknown 'to' argument specified.")
    if (is.null(data)) {
        data <- sys.frame(sys.parent())
    }
    else {
        if (!is.data.frame(data)) {
            data <- data.frame(data)
        }
    }
    mf <- match.call()
    mf.slab <- mf[[match("slab", names(mf))]]
    mf.subset <- mf[[match("subset", names(mf))]]
    slab <- eval(mf.slab, data)
    subset <- eval(mf.subset, data)
    mf.ai <- mf[[match("ai", names(mf))]]
    mf.bi <- mf[[match("bi", names(mf))]]
    mf.ci <- mf[[match("ci", names(mf))]]
    mf.di <- mf[[match("di", names(mf))]]
    mf.n1i <- mf[[match("n1i", names(mf))]]
    mf.n2i <- mf[[match("n2i", names(mf))]]
    ai <- eval(mf.ai, data)
    bi <- eval(mf.bi, data)
    ci <- eval(mf.ci, data)
    di <- eval(mf.di, data)
    n1i <- eval(mf.n1i, data)
    n2i <- eval(mf.n2i, data)
    if (is.null(bi)) {
        bi <- n1i - ai
    }
    if (is.null(di)) {
        di <- n2i - ci
    }
    ni <- ai + bi + ci + di
    k <- length(ai)
    ids <- 1:k
    if (is.null(slab)) {
        slab.null <- TRUE
        slab <- 1:k
    }
    else {
        if (length(slab) != unique(length(slab))) 
            stop("Study labels must be unique.")
        if (length(slab) != length(ai)) 
            stop("Study labels not of same length as data.")
        slab.null <- FALSE
    }
    if (!is.null(subset)) {
        ai <- ai[subset]
        bi <- bi[subset]
        ci <- ci[subset]
        di <- di[subset]
        ni <- ni[subset]
        slab <- slab[subset]
        ids <- ids[subset]
        k <- length(ai)
    }
    k.f <- k
    dat <- escalc(measure = "PETO", ai = ai, bi = bi, ci = ci, 
        di = di, add = add[1], to = to[1])
    yi <- dat$yi
    vi <- dat$vi
    ai.f <- ai
    bi.f <- bi
    ci.f <- ci
    di.f <- di
    yi.f <- yi
    vi.f <- vi
    ni.f <- ni
    aibicidi.na <- is.na(cbind(ai, bi, ci, di))
    if (any(aibicidi.na)) {
        not.na <- apply(aibicidi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            ai <- ai[not.na]
            bi <- bi[not.na]
            ci <- ci[not.na]
            di <- di[not.na]
            k <- length(ai)
            warning("Tables with NAs omitted from model fitting.")
        }
        if (na.act == "na.fail") 
            stop("Missing values in tables.")
    }
    else {
        not.na <- rep(TRUE, k)
    }
    if (k <= 1) 
        stop("Processing terminated since k <= 1.")
    yivi.na <- is.na(cbind(yi, vi))
    if (any(yivi.na)) {
        not.na.yivi <- apply(yivi.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            yi <- yi[not.na.yivi]
            vi <- vi[not.na.yivi]
            ni <- ni[not.na.yivi]
            warning("Some yi/vi values are NA.")
        }
        if (na.act == "na.fail") 
            stop("Missing yi/vi values.")
    }
    else {
        not.na.yivi <- rep(TRUE, k)
    }
    k.yi <- length(yi)
    if (to[2] == "all") {
        ai <- ai + add[2]
        bi <- bi + add[2]
        ci <- ci + add[2]
        di <- di + add[2]
    }
    if (to[2] == "only0") {
        id0 <- c(ai == 0L | bi == 0L | ci == 0L | di == 0L)
        ai[id0] <- ai[id0] + add[2]
        bi[id0] <- bi[id0] + add[2]
        ci[id0] <- ci[id0] + add[2]
        di[id0] <- di[id0] + add[2]
    }
    if (to[2] == "if0all") {
        id0 <- c(ai == 0L | bi == 0L | ci == 0L | di == 0L)
        if (any(id0)) {
            ai <- ai + add[2]
            bi <- bi + add[2]
            ci <- ci + add[2]
            di <- di + add[2]
        }
    }
    alpha <- (100 - level)/100
    n1i <- ai + bi
    n2i <- ci + di
    Ni <- ai + bi + ci + di
    xt <- ai + ci
    yt <- bi + di
    Ei <- xt * n1i/Ni
    Vi <- xt * yt * (n1i/Ni) * (n2i/Ni)/(Ni - 1)
    sumVi <- sum(Vi)
    if (sumVi == 0L) 
        stop("All tables have either only events or no events at all. Peto's method cannot be used.")
    b <- sum(ai - Ei)/sumVi
    se <- sqrt(1/sumVi)
    zval <- b/se
    pval <- 2 * (1 - pnorm(abs(zval)))
    ci.lb <- b - qnorm(1 - alpha/2) * se
    ci.ub <- b + qnorm(1 - alpha/2) * se
    names(b) <- "intrcpt"
    vb <- matrix(se^2, dimnames = list("intrcpt", "intrcpt"))
    wi <- 1/vi
    QE <- sum(wi * (yi - b)^2)
    QEp <- 1 - pchisq(QE, df = k.yi - 1)
    ll.ML <- -1/2 * (k.yi) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi)) - 1/2 * QE
    ll.REML <- -1/2 * (k.yi - 1) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi)) - 1/2 * log(sum(wi)) - 1/2 * QE
    dev.ML <- -2 * ll.ML
    dev.REML <- -2 * ll.REML
    AIC.ML <- -2 * ll.ML + 2
    BIC.ML <- -2 * ll.ML + log(k.yi)
    AIC.REML <- -2 * ll.REML + 2
    BIC.REML <- -2 * ll.REML + log(k.yi - 1)
    fit.stats <- matrix(c(ll.ML, dev.ML, AIC.ML, BIC.ML, ll.REML, 
        dev.REML, AIC.REML, BIC.REML), ncol = 2, byrow = FALSE)
    dimnames(fit.stats) <- list(c("ll", "dev", "AIC", "BIC"), 
        c("ML", "REML"))
    fit.stats <- data.frame(fit.stats)
    tau2 <- 0
    X.f <- cbind(rep(1, k.f))
    int.only <- TRUE
    measure <- "PETO"
    method <- "FE"
    weighted <- TRUE
    knha <- FALSE
    res <- list(b, se, zval, pval, ci.lb, ci.ub, vb, tau2, k, 
        k.f, k.yi, QE, QEp, int.only, yi, vi, yi.f, vi.f, X.f, 
        ai, bi, ci, di, ai.f, bi.f, ci.f, di.f, ni, ni.f, ids, 
        not.na, not.na.yivi, slab, slab.null, measure, method, 
        weighted, knha, digits, level, add, to, fit.stats)
    names(res) <- c("b", "se", "zval", "pval", "ci.lb", "ci.ub", 
        "vb", "tau2", "k", "k.f", "k.yi", "QE", "QEp", "int.only", 
        "yi", "vi", "yi.f", "vi.f", "X.f", "ai", "bi", "ci", 
        "di", "ai.f", "bi.f", "ci.f", "di.f", "ni", "ni.f", "ids", 
        "not.na", "not.na.yivi", "slab", "slab.null", "measure", 
        "method", "weighted", "knha", "digits", "level", "add", 
        "to", "fit.stats")
    class(res) <- c("rma.peto", "rma")
    return(res)
}
rma.uni <-
function (yi, vi, sei, ai, bi, ci, di, n1i, n2i, m1i, m2i, sd1i, 
    sd2i, xi, mi, ri, ni, mods = NULL, data = NULL, intercept = TRUE, 
    slab = NULL, subset = NULL, measure = "GEN", add = 1/2, to = "only0", 
    vtype = "LS", method = "REML", weighted = TRUE, level = 95, 
    digits = 4, btt = NULL, tau2 = NULL, knha = FALSE, control = list()) 
{
    if (!is.element(measure, c("GEN", "MD", "SMD", "RR", "OR", 
        "PETO", "RD", "AS", "PHI", "YUQ", "YUY", "PR", "PLN", 
        "PLO", "PAS", "PFT", "COR", "UCOR", "ZCOR"))) 
        stop("Unknown 'measure' specified.")
    if (!is.element(method, c("FE", "HS", "HE", "DL", 
        "SJ", "ML", "REML", "EB"))) 
        stop("Unknown 'method' specified.")
    if (is.null(data)) {
        data <- sys.frame(sys.parent())
    }
    else {
        if (!is.data.frame(data)) {
            data <- data.frame(data)
        }
    }
    mf <- match.call()
    mf.slab <- mf[[match("slab", names(mf))]]
    mf.subset <- mf[[match("subset", names(mf))]]
    mf.mods <- mf[[match("mods", names(mf))]]
    slab <- eval(mf.slab, data)
    subset <- eval(mf.subset, data)
    mods <- eval(mf.mods, data)
    if (measure == "GEN") {
        mf.yi <- mf[[match("yi", names(mf))]]
        mf.vi <- mf[[match("vi", names(mf))]]
        mf.sei <- mf[[match("sei", names(mf))]]
        yi <- eval(mf.yi, data)
        vi <- eval(mf.vi, data)
        sei <- eval(mf.sei, data)
        ni <- NULL
        if (is.null(vi)) {
            vi <- sei^2
        }
        if (length(yi) != length(vi)) 
            stop("Length of yi and vi (or sei) is not the same.")
    }
    else {
        if (is.element(measure, c("RR", "OR", "PETO", "RD", "AS", 
            "PHI", "YUQ", "YUY"))) {
            mf.ai <- mf[[match("ai", names(mf))]]
            mf.bi <- mf[[match("bi", names(mf))]]
            mf.ci <- mf[[match("ci", names(mf))]]
            mf.di <- mf[[match("di", names(mf))]]
            mf.n1i <- mf[[match("n1i", names(mf))]]
            mf.n2i <- mf[[match("n2i", names(mf))]]
            ai <- eval(mf.ai, data)
            bi <- eval(mf.bi, data)
            ci <- eval(mf.ci, data)
            di <- eval(mf.di, data)
            n1i <- eval(mf.n1i, data)
            n2i <- eval(mf.n2i, data)
            if (is.null(bi)) {
                bi <- n1i - ai
            }
            if (is.null(di)) {
                di <- n2i - ci
            }
            ni <- ai + bi + ci + di
            dat <- escalc(measure, ai = ai, bi = bi, ci = ci, 
                di = di, add = add, to = to)
        }
        if (is.element(measure, c("MD", "SMD"))) {
            mf.m1i <- mf[[match("m1i", names(mf))]]
            mf.m2i <- mf[[match("m2i", names(mf))]]
            mf.sd1i <- mf[[match("sd1i", names(mf))]]
            mf.sd2i <- mf[[match("sd2i", names(mf))]]
            mf.n1i <- mf[[match("n1i", names(mf))]]
            mf.n2i <- mf[[match("n2i", names(mf))]]
            m1i <- eval(mf.m1i, data)
            m2i <- eval(mf.m2i, data)
            sd1i <- eval(mf.sd1i, data)
            sd2i <- eval(mf.sd2i, data)
            n1i <- eval(mf.n1i, data)
            n2i <- eval(mf.n2i, data)
            ni <- n1i + n2i
            dat <- escalc(measure, m1i = m1i, m2i = m2i, sd1i = sd1i, 
                sd2i = sd2i, n1i = n1i, n2i = n2i, vtype = vtype)
        }
        if (is.element(measure, c("PR", "PLN", "PLO", "PAS", 
            "PFT"))) {
            mf.xi <- mf[[match("xi", names(mf))]]
            mf.mi <- mf[[match("mi", names(mf))]]
            mf.ni <- mf[[match("ni", names(mf))]]
            xi <- eval(mf.xi, data)
            mi <- eval(mf.mi, data)
            ni <- eval(mf.ni, data)
            if (is.null(mi)) {
                mi <- ni - xi
            }
            ni <- xi + mi
            dat <- escalc(measure, xi = xi, mi = mi, add = add, 
                to = to)
        }
        if (is.element(measure, c("COR", "UCOR", "ZCOR"))) {
            mf.ri <- mf[[match("ri", names(mf))]]
            mf.ni <- mf[[match("ni", names(mf))]]
            ri <- eval(mf.ri, data)
            ni <- eval(mf.ni, data)
            dat <- escalc(measure, ri = ri, ni = ni, vtype = vtype)
        }
        yi <- dat$yi
        vi <- dat$vi
    }
    if (is.vector(mods)) 
        mods <- cbind(mods)
    if (is.data.frame(mods)) 
        mods <- as.matrix(mods)
    k <- length(yi)
    ids <- 1:k
    if (is.null(slab)) {
        slab.null <- TRUE
        slab <- 1:k
    }
    else {
        if (length(slab) != unique(length(slab))) 
            stop("Study labels must be unique.")
        if (length(slab) != length(yi)) 
            stop("Study labels not of same length as data.")
        slab.null <- FALSE
    }
    if (!is.null(subset)) {
        yi <- yi[subset]
        vi <- vi[subset]
        ni <- ni[subset]
        mods <- mods[subset, , drop = FALSE]
        slab <- slab[subset]
        ids <- ids[subset]
        k <- length(yi)
    }
    yi.f <- yi
    vi.f <- vi
    ni.f <- ni
    mods.f <- mods
    k.f <- k
    YVM.na <- is.na(cbind(yi, vi, mods))
    if (any(YVM.na)) {
        na.act <- getOption("na.action")
        if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
            stop("Unknwn 'na.action' specified under options().")
        not.na <- apply(YVM.na, MARGIN = 1, sum) == 0L
        if (na.act == "na.omit" || na.act == "na.exclude") {
            yi <- yi[not.na]
            vi <- vi[not.na]
            ni <- ni[not.na]
            mods <- mods[not.na, , drop = FALSE]
            k <- length(yi)
            warning("Cases with NAs omitted from model fitting.")
        }
        if (na.act == "na.fail") 
            stop("Missing values in data.")
    }
    else {
        not.na <- rep(TRUE, k)
    }
    if (k <= 1) 
        stop("Processing terminated since k <= 1.")
    if (any(vi <= 0)) {
        allvipos <- FALSE
        warning("There are outcomes with non-positive sampling variances.")
        if (any(vi < 0)) {
            vi[vi <= 0] <- 0
            warning("Negative sampling variances constrained to zero.")
        }
    }
    else {
        allvipos <- TRUE
    }
    if (is.null(mods) && !intercept) {
        warning("Must either include an intercept (intercept=TRUE) and/or moderators in model.\n  Coerced intercept into the model.")
        intercept <- TRUE
    }
    if (intercept) {
        X <- cbind(intrcpt = rep(1, k), mods)
        X.f <- cbind(intrcpt = rep(1, k.f), mods.f)
    }
    else {
        X <- mods
        X.f <- mods.f
    }
    p <- dim(X)[2]
    if (method == "FE") {
        if (p > k) {
            stop("The number of parameters to be estimated is larger than the number of observations.")
        }
    }
    else {
        if (!is.numeric(tau2)) {
            if (p + 1 > k) {
                stop("The number of parameters to be estimated is larger than the number of observations.")
            }
        }
        else {
            if (p > k) {
                stop("The number of parameters to be estimated is larger than the number of observations.")
            }
        }
    }
    if ((p == 1L) && (all(sapply(X, identical, 1)))) {
        int.only <- TRUE
    }
    else {
        int.only <- FALSE
    }
    if (is.null(btt)) {
        if (p > 1) {
            if (intercept) {
                btt <- 2:p
            }
            else {
                btt <- 1:p
            }
        }
        else {
            btt <- 1
        }
    }
    else {
        btt <- btt[(btt >= 1) & (btt <= p)]
        btt <- unique(round(btt))
        if (length(intersect(btt, 1:p)) == 0L) {
            stop("Non-existent coefficients specified with 'btt'.")
        }
    }
    bntt <- setdiff(1:p, btt)
    m <- length(btt)
    con <- list(tau2.init = NULL, tau2.min = 0, tau2.max = 50, 
        threshold = 10^-5, maxit = 50, stepadj = 1, verbose = FALSE)
    con[pmatch(names(control), names(con))] <- control
    se.tau2 <- I2 <- H2 <- QE <- QEp <- NA
    s2w <- 1
    Y <- as.matrix(yi)
    alpha <- (100 - level)/100
    if (!is.numeric(tau2)) {
        if (method == "HS") {
            if (!allvipos) 
                stop("HS estimator cannot be used with non-positive sampling variances.")
            wi <- 1/vi
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            RSS <- crossprod(Y, P) %*% Y
            tau2 <- RSS/sum(wi) - k/sum(wi)
            se.tau2 <- sqrt(1/sum(wi)^2 * (2 * (k - p) + 4 * 
                max(tau2, 0) * .tr(P) + 2 * max(tau2, 0)^2 * 
                .tr(P %*% P)))
        }
        if (is.element(method, c("HE", "ML", "REML", "EB"))) {
            stXX <- .invcalc(X = X, W = diag(k), k = k)
            P <- diag(k) - X %*% tcrossprod(stXX, X)
            RSS <- crossprod(Y, P) %*% Y
            trPV <- .tr(P %*% diag(vi))
            tau2 <- (RSS - trPV)/(k - p)
            se.tau2 <- sqrt(1/(k - p)^2 * (2 * .tr(P %*% diag(vi) %*% 
                P %*% diag(vi)) + 4 * max(tau2, 0) * trPV + 2 * 
                max(tau2, 0)^2 * (k - p)))
        }
        if (method == "DL") {
            if (!allvipos) 
                stop("DL estimator cannot be used with non-positive sampling variances.")
            wi <- 1/vi
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            RSS <- crossprod(Y, P) %*% Y
            trP <- .tr(P)
            tau2 <- (RSS - (k - p))/trP
            se.tau2 <- sqrt(1/trP^2 * (2 * (k - p) + 4 * max(tau2, 
                0) * trP + 2 * max(tau2, 0)^2 * .tr(P %*% P)))
        }
        if (method == "SJ") {
            if (is.null(con$tau2.init)) {
                tau2 <- var(yi) * (k - 1)/k
            }
            else {
                tau2 <- con$tau2.init
            }
            wi <- 1/(vi + tau2)
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            RSS <- crossprod(Y, P) %*% Y
            tau2 <- tau2 * RSS/(k - p)
        }
        if (is.element(method, c("ML", "REML", "EB"))) {
            conv <- 1
            change <- con$threshold + 1
            iter <- 0
            if (is.null(con$tau2.init)) {
                tau2 <- max(0, tau2)
            }
            else {
                tau2 <- con$tau2.init
            }
            while (change > con$threshold) {
                if (con$verbose) 
                  cat("Iteration", iter, "\ttau^2 =", round(tau2, 
                    digits), "\n")
                iter <- iter + 1
                tau2.old <- tau2
                wi <- 1/(vi + tau2)
                W <- diag(wi)
                stXWX <- .invcalc(X = X, W = W, k = k)
                P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
                if (method == "ML") {
                  PP <- P %*% P
                  adj <- 1/sum(wi^2) * (crossprod(Y, PP) %*% 
                    Y - sum(wi))
                }
                if (method == "REML") {
                  PP <- P %*% P
                  adj <- 1/.tr(PP) * (crossprod(Y, PP) %*% Y - 
                    .tr(P))
                }
                if (method == "EB") {
                  adj <- 1/sum(wi) * (crossprod(Y, P) %*% Y * 
                    k/(k - p) - k)
                }
                adj <- adj * con$stepadj
                while (tau2 + adj < con$tau2.min) {
                  adj <- adj/2
                }
                tau2 <- tau2 + adj
                change <- abs(tau2.old - tau2)
                if (iter > con$maxit) {
                  conv <- 0
                  break
                }
            }
            if (conv == 0L) 
                stop("Fisher scoring algorithm did not converge. Try increasing the number of iterations (maxit), adjust the threshold (threshold), or use a different estimator for tau^2.")
            if (method == "ML") {
                se.tau2 <- sqrt(2/sum(wi^2))
            }
            if (method == "REML") {
                se.tau2 <- sqrt(2/.tr(PP))
            }
        }
        tau2 <- max(con$tau2.min, c(tau2))
    }
    else {
        if (method == "ML") {
            wi <- 1/(vi + tau2)
            se.tau2 <- sqrt(2/sum(wi^2))
        }
        if (method == "REML") {
            wi <- 1/(vi + tau2)
            W <- diag(wi)
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            se.tau2 <- sqrt(2/.tr(P %*% P))
        }
    }
    if (method == "FE") {
        tau2 <- 0
        if (!allvipos && weighted) 
            stop("Weighted estimation cannot be used with a fixed-effects\n  model when there are non-positive sampling variances.")
    }
    if (con$verbose) 
        cat("Fisher scoring algorithm converged after", iter, 
            "iterations.", "\n\n")
    if (allvipos) {
        wi <- 1/vi
        W <- diag(wi)
        stXWX <- .invcalc(X = X, W = W, k = k)
        P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
        QE <- max(0, c(crossprod(Y, P) %*% Y))
        QEp <- 1 - pchisq(QE, df = k - p)
        if (int.only) {
            sumwi <- sum(wi)
            vi.avg <- (k - 1)/(sumwi - sum(wi^2)/sumwi)
            I2 <- 100 * tau2/(vi.avg + tau2)
            H2 <- tau2/vi.avg + 1
        }
    }
    wi <- 1/(vi + tau2)
    W <- diag(wi)
    if (weighted) {
        stXWX <- .invcalc(X = X, W = W, k = k)
        b <- stXWX %*% crossprod(X, W) %*% Y
        vb <- stXWX
        P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
        RSS.f <- crossprod(Y, P) %*% Y
        if (knha) {
            s2w <- c(RSS.f)/(k - p)
            vb <- s2w * vb
            if (method == "FE") 
                warning("The Knapp & Hartung (2003) method is not meant to be used in the context of fixed-effects models.")
        }
        if (length(bntt) == 0L) {
            QM <- c(sum(wi * yi^2) - RSS.f)/s2w
        }
        else {
            Xr <- X[, bntt, drop = FALSE]
            stXWX <- .invcalc(X = Xr, W = W, k = k)
            P <- W - W %*% Xr %*% stXWX %*% crossprod(Xr, W)
            RSS.r <- crossprod(Y, P) %*% Y
            QM <- c(RSS.r - RSS.f)/s2w
        }
    }
    else {
        stXX <- .invcalc(X = X, W = diag(k), k = k)
        b <- stXX %*% crossprod(X, Y)
        vb <- tcrossprod(stXX, X) %*% diag(vi + tau2) %*% X %*% 
            stXX
        P <- W - W %*% X %*% tcrossprod(stXX, X) - X %*% stXX %*% 
            crossprod(X, W) + X %*% stXX %*% crossprod(X, W) %*% 
            X %*% tcrossprod(stXX, X)
        RSS.f <- crossprod(Y, P) %*% Y
        if (knha) {
            stXWX <- .invcalc(X = X, W = W, k = k)
            P <- W - W %*% X %*% stXWX %*% crossprod(X, W)
            s2w <- c(crossprod(Y, P) %*% Y)/(k - p)
            vb <- s2w * vb
            if (method == "FE") 
                warning("The Knapp & Hartung (2003) method is not meant to be used in the context of fixed-effects models.")
        }
        QM <- t(b)[btt] %*% chol2inv(chol(vb[btt, btt])) %*% 
            b[btt]
    }
    se <- sqrt(diag(vb))
    names(se) <- NULL
    zval <- c(b/se)
    if (knha) {
        QM <- QM/m
        QMp <- 1 - pf(QM, df1 = m, df2 = k - p)
        pval <- 2 * (1 - pt(abs(zval), df = k - p))
        crit <- qt(1 - alpha/2, df = k - p)
    }
    else {
        QMp <- 1 - pchisq(QM, df = m)
        pval <- 2 * (1 - pnorm(abs(zval)))
        crit <- qnorm(1 - alpha/2)
    }
    ci.lb <- c(b - crit * se)
    ci.ub <- c(b + crit * se)
    ll.ML <- -1/2 * (k) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi + tau2)) - 1/2 * RSS.f
    ll.REML <- -1/2 * (k - p) * log(2 * get("pi", pos = "package:base")) - 
        1/2 * sum(log(vi + tau2)) - 1/2 * determinant(crossprod(X, 
        W) %*% X, logarithm = TRUE)$modulus - 1/2 * RSS.f
    dev.ML <- -2 * ll.ML
    dev.REML <- -2 * ll.REML
    AIC.ML <- -2 * ll.ML + 2 * (p + ifelse(method == "FE", 0, 
        1))
    BIC.ML <- -2 * ll.ML + (p + ifelse(method == "FE", 0, 1)) * 
        log(k)
    AIC.REML <- -2 * ll.REML + 2 * (p + ifelse(method == "FE", 
        0, 1))
    BIC.REML <- -2 * ll.REML + (p + ifelse(method == "FE", 0, 
        1)) * log(k - p)
    fit.stats <- matrix(c(ll.ML, dev.ML, AIC.ML, BIC.ML, ll.REML, 
        dev.REML, AIC.REML, BIC.REML), ncol = 2, byrow = FALSE)
    dimnames(fit.stats) <- list(c("ll", "dev", "AIC", "BIC"), 
        c("ML", "REML"))
    fit.stats <- data.frame(fit.stats)
    res <- list(b, se, zval, pval, ci.lb, ci.ub, vb, tau2, se.tau2, 
        k, k.f, p, m, QE, QEp, QM, QMp, I2, H2, int.only, yi, 
        vi, X, yi.f, vi.f, X.f, ni, ni.f, ids, not.na, slab, 
        slab.null, measure, method, weighted, knha, s2w, btt, 
        intercept, digits, level, con, fit.stats)
    names(res) <- c("b", "se", "zval", "pval", "ci.lb", "ci.ub", 
        "vb", "tau2", "se.tau2", "k", "k.f", "p", "m", "QE", 
        "QEp", "QM", "QMp", "I2", "H2", "int.only", "yi", "vi", 
        "X", "yi.f", "vi.f", "X.f", "ni", "ni.f", "ids", "not.na", 
        "slab", "slab.null", "measure", "method", "weighted", 
        "knha", "s2w", "btt", "intercept", "digits", "level", 
        "control", "fit.stats")
    class(res) <- c("rma.uni", "rma")
    return(res)
}
rstandard.rma.mh <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.mh", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.mh\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    e <- c(x$yi.f - x$b)
    e[abs(e) < 100 * .Machine$double.eps] <- 0
    se <- sqrt(x$vi.f)
    z <- e/se
    if (na.act == "na.omit") {
        out <- list(resid = e[x$not.na.yivi], se = se[x$not.na.yivi], 
            z = z[x$not.na.yivi])
        out$slab <- x$slab[x$not.na.yivi]
    }
    if (na.act == "na.exclude") {
        out <- list(resid = e, se = se, z = z)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
rstandard.rma.peto <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.peto", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.peto\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    e <- c(x$yi.f - x$b)
    e[abs(e) < 100 * .Machine$double.eps] <- 0
    se <- sqrt(x$vi.f)
    z <- e/se
    if (na.act == "na.omit") {
        out <- list(resid = e[x$not.na.yivi], se = se[x$not.na.yivi], 
            z = z[x$not.na.yivi])
        out$slab <- x$slab[x$not.na.yivi]
    }
    if (na.act == "na.exclude") {
        out <- list(resid = e, se = se, z = z)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
rstandard.rma.uni <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.uni", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    V <- diag(x$vi + x$tau2)
    if (x$weighted) {
        wi <- 1/(x$vi + x$tau2)
        W <- diag(wi)
        stXWX <- .invcalc(X = x$X, W = W, k = x$k)
        H <- x$X %*% stXWX %*% crossprod(x$X, W)
    }
    else {
        stXX <- .invcalc(X = x$X, W = diag(x$k), k = x$k)
        H <- x$X %*% tcrossprod(stXX, x$X)
    }
    ImH <- diag(x$k) - H
    e <- ImH %*% cbind(x$yi)
    e[abs(e) < 100 * .Machine$double.eps] <- 0
    ve <- ImH %*% tcrossprod(V, ImH)
    se <- sqrt(diag(ve))
    resid <- rep(NA, x$k.f)
    seresid <- rep(NA, x$k.f)
    stanres <- rep(NA, x$k.f)
    resid[x$not.na] <- e
    seresid[x$not.na] <- se
    stanres[x$not.na] <- e/se
    if (na.act == "na.omit") {
        out <- list(resid = resid[x$not.na], se = seresid[x$not.na], 
            z = stanres[x$not.na])
        out$slab <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(resid = resid, se = seresid, z = stanres)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
rstudent.rma.mh <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.mh", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.mh\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    delpred <- rep(NA, x$k.f)
    vdelpred <- rep(NA, x$k.f)
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma.mh(ai = x$ai.f[-i], bi = x$bi.f[-i], ci = x$ci.f[-i], 
            di = x$di.f[-i], measure = x$measure, add = x$add, 
            to = x$to, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        delpred[i] <- res$b
        vdelpred[i] <- res$vb
    }
    delresid <- x$yi.f - delpred
    delresid[abs(delresid) < 100 * .Machine$double.eps] <- 0
    sedelresid <- sqrt(x$vi.f + vdelpred)
    standelres <- delresid/sedelresid
    if (na.act == "na.omit") {
        out <- list(resid = delresid[x$not.na.yivi], se = sedelresid[x$not.na.yivi], 
            z = standelres[x$not.na.yivi])
        out$slab <- x$slab[x$not.na.yivi]
    }
    if (na.act == "na.exclude") {
        out <- list(resid = delresid, se = sedelresid, z = standelres)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
rstudent.rma.peto <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.peto", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.peto\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    delpred <- rep(NA, x$k.f)
    vdelpred <- rep(NA, x$k.f)
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma.peto(ai = x$ai.f[-i], bi = x$bi.f[-i], 
            ci = x$ci.f[-i], di = x$di.f[-i], add = x$add, to = x$to, 
            ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        delpred[i] <- res$b
        vdelpred[i] <- res$vb
    }
    delresid <- x$yi.f - delpred
    delresid[abs(delresid) < 100 * .Machine$double.eps] <- 0
    sedelresid <- sqrt(x$vi.f + vdelpred)
    standelres <- delresid/sedelresid
    if (na.act == "na.omit") {
        out <- list(resid = delresid[x$not.na.yivi], se = sedelresid[x$not.na.yivi], 
            z = standelres[x$not.na.yivi])
        out$slab <- x$slab[x$not.na.yivi]
    }
    if (na.act == "na.exclude") {
        out <- list(resid = delresid, se = sedelresid, z = standelres)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
rstudent.rma.uni <-
function (model, digits = model$digits, ...) 
{
    if (!is.element("rma.uni", class(model))) 
        stop("Argument 'model' must be an object of class \"rma.uni\".")
    na.act <- getOption("na.action")
    if (!is.element(na.act, c("na.omit", "na.exclude", "na.fail"))) 
        stop("Unknwn 'na.action' specified under options().")
    x <- model
    tau2.del <- rep(NA, x$k.f)
    delpred <- rep(NA, x$k.f)
    vdelpred <- rep(NA, x$k.f)
    o.warn <- getOption("warn")
    on.exit(options(warn = o.warn))
    options(warn = -1)
    for (i in (1:x$k.f)[x$not.na]) {
        res <- try(rma(x$yi.f[-i], x$vi.f[-i], mods = cbind(x$X.f[-i, 
            ]), method = x$method, weighted = x$weighted, intercept = FALSE, 
            knha = x$knha, control = x$control, ...), silent = TRUE)
        if (is.element("try-error", class(res))) 
            next
        tau2.del[i] <- res$tau2
        Xi <- matrix(x$X.f[i, ], nrow = 1)
        delpred[i] <- Xi %*% res$b
        vdelpred[i] <- Xi %*% tcrossprod(res$vb, Xi)
    }
    delresid <- x$yi.f - delpred
    delresid[abs(delresid) < 100 * .Machine$double.eps] <- 0
    sedelresid <- sqrt(x$vi.f + vdelpred + tau2.del)
    standelres <- delresid/sedelresid
    if (na.act == "na.omit") {
        out <- list(resid = delresid[x$not.na], se = sedelresid[x$not.na], 
            z = standelres[x$not.na])
        out$slab <- x$slab[x$not.na]
    }
    if (na.act == "na.exclude") {
        out <- list(resid = delresid, se = sedelresid, z = standelres)
        out$slab <- x$slab
    }
    if (na.act == "na.fail") 
        stop("Missing values in results.")
    out$digits <- digits
    class(out) <- c("list.rma")
    return(out)
}
summary.rma <-
function (object, digits = object$digits, showfit = TRUE, signif.legend = TRUE, 
    ...) 
{
    if (!is.element("rma", class(object))) 
        stop("Argument 'object' must be an object of class \"rma\".")
    print(x = object, digits = digits, showfit = showfit, signif.legend = signif.legend, 
        ...)
}
transf.exp.int <-
function (x, targs = NULL, ...) 
{
    if (is.null(targs$tau2)) {
        targs$tau2 <- 0
    }
    if (is.null(targs$lower)) {
        targs$lower <- -5 * sqrt(targs$tau2)
    }
    if (is.null(targs$upper)) {
        targs$lower <- +5 * sqrt(targs$tau2)
    }
    toint <- function(zval, x, tau2) {
        exp(zval) * dnorm(zval, mean = x, sd = sqrt(tau2))
    }
    cfunc <- function(x, tau2, lower, upper) {
        integrate(toint, lower = lower, upper = upper, x = x, 
            tau2 = tau2)$value
    }
    z <- sapply(x, FUN = cfunc, tau2 = targs$tau2, lower = targs$lower, 
        upper = targs$upper)
    return(z)
}
transf.ilogit <-
function (x, ...) 
{
    z <- exp(x)/(1 + exp(x))
    return(z)
}
transf.logit <-
function (x, ...) 
{
    z <- log(x/(1 - x))
    return(z)
}
transf.rtoz <-
function (x, ...) 
{
    z <- 1/2 * log((1 + x)/(1 - x))
    return(z)
}
transf.ztor <-
function (x, ...) 
{
    z <- (exp(2 * x) - 1)/(exp(2 * x) + 1)
    z[x == -Inf] <- -1
    z[x == Inf] <- 1
    return(z)
}
transf.ztor.int <-
function (x, targs = NULL, ...) 
{
    if (is.null(targs$tau2)) {
        targs$tau2 <- 0
    }
    if (is.null(targs$lower)) {
        targs$lower <- -5 * sqrt(targs$tau2)
    }
    if (is.null(targs$upper)) {
        targs$lower <- +5 * sqrt(targs$tau2)
    }
    toint <- function(zval, x, tau2) {
        (exp(2 * zval) - 1)/(exp(2 * zval) + 1) * dnorm(zval, 
            mean = x, sd = sqrt(tau2))
    }
    cfunc <- function(x, tau2, lower, upper) {
        integrate(toint, lower = lower, upper = upper, x = x, 
            tau2 = tau2)$value
    }
    z <- sapply(x, FUN = cfunc, tau2 = targs$tau2, lower = targs$lower, 
        upper = targs$upper)
    return(z)
}
trimfill <-
function (x, ...) 
UseMethod("trimfill")
trimfill.rma.uni <-
function (x, estimator = "L0", side = NULL, maxit = 50, verbose = FALSE, 
    ...) 
{
    if (!is.element("rma.uni", class(x))) 
        stop("Argument 'x' must be an object of class \"rma.uni\".")
    if (!x$int.only) 
        stop("Trim-and-fill method only applicable for models without moderators.")
    estimator <- match.arg(estimator, c("L0", "R0"))
    yi <- x$yi
    vi <- x$vi
    if (is.null(side)) {
        res <- rma(yi, vi, mods = sqrt(vi), intercept = TRUE, 
            method = x$method, weighted = x$weighted, ...)
        if (res$b[2] < 0) {
            side <- "right"
        }
        else {
            side <- "left"
        }
    }
    else {
        side <- match.arg(side, c("left", "right"))
    }
    if (side == "right") {
        yi <- -1 * yi
    }
    idix <- sort(yi, index.return = TRUE)$ix
    yi <- yi[idix]
    vi <- vi[idix]
    k <- length(yi)
    k0.sav <- -1
    k0 <- 0
    iter <- 0
    while (abs(k0 - k0.sav) > 0) {
        k0.sav <- k0
        iter <- iter + 1
        if (iter > maxit) 
            stop("Trim and fill algorithm did not converge.")
        yi.t <- yi[1:(k - k0)]
        vi.t <- vi[1:(k - k0)]
        res <- rma(yi.t, vi.t, intercept = TRUE, method = x$method, 
            weighted = x$weighted, ...)
        b <- c(res$b)
        yi.c <- yi - b
        yi.c.r <- rank(abs(yi.c), ties.method = "first")
        yi.c.r.s <- sign(yi.c) * yi.c.r
        if (estimator == "L0") {
            Sr <- sum(yi.c.r.s[yi.c.r.s > 0])
            k0 <- round((4 * Sr - k * (k + 1))/(2 * k - 1))
        }
        if (estimator == "R0") {
            k0 <- (k - max(-1 * yi.c.r.s[yi.c.r.s < 0])) - 1
        }
        k0 <- max(0, k0)
        if (verbose) 
            cat("Iteration:", iter, "\tk0 =", k0, "\t  b =", 
                b, "\n")
        if (k0 <= 0) {
            cat("\nEstimated number of missing studies on the", 
                side, "side is zero.\n\n")
            stop
        }
    }
    if (k0 > 0) {
        if (side == "right") {
            yi.c <- -1 * (yi.c - b)
        }
        else {
            yi.c <- yi.c - b
        }
        yi.fill <- c(x$yi.f, -1 * yi.c[(k - k0 + 1):k])
        vi.fill <- c(x$vi.f, vi[(k - k0 + 1):k])
        cat("\nEstimated number of missing studies on the", side, 
            "side:", k0, "\n")
        res <- rma(yi.fill, vi.fill, intercept = TRUE, method = x$method, 
            weighted = x$weighted, ...)
        res$fill <- c(rep(0, k), rep(1, k0))
        class(res) <- c("rma.uni.trimfill", class(res))
        res$ids <- c(x$ids, (x$k.f + 1):(x$k.f + k0))
        if (!x$slab.null) {
            res$slab <- c(x$slab, paste("Filled", 1:k0))
            res$slab.null <- FALSE
        }
        else {
            res$slab <- c(paste("Study", x$ids), paste("Filled", 
                1:k0))
            res$slab.null <- FALSE
        }
        return(res)
    }
}
vcov.rma <-
function (object, ...) 
{
    if (!is.element("rma", class(object))) 
        stop("Argument 'object' must be an object of class \"rma\".")
    return(object$vb)
}
