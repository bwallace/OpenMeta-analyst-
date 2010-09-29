#### Utility functions for testing covMCD()
#### -------------------------------------- ../tests/tmcd.R

repMCD <- function(x, nrep = 1, method = c("FASTMCD","MASS"))
{
    stopifnot(length(nrep) == 1, nrep >= 1)
    method <- match.arg(method)
    if(method == "MASS") {
	if(paste(R.version$major, R.version$minor, sep=".") < 2.3)
	    cov.rob <- MASS::cov.rob
	for(i in 1:nrep) MASS::cov.mcd(x)
    }
    else for(i in 1:nrep) covMcd(x)
}

doMCDdata <- function(nrep = 1, time = nrep >= 3, short = time, full = !short,
		   method = c("FASTMCD", "MASS"))
{
    ##@bdescr
    ## Test the function covMcd() on the literature datasets:
    ##
    ## Call covMcd() for "all" regression datasets available in robustbase
    ## and print:
    ##  - execution time (if time)
    ##  - objective function
    ##  - best subsample found (if not short)
    ##  - outliers identified (with cutoff 0.975) (if not short)
    ##  - estimated center and covariance matrix (if full)
    ##
    ##@edescr
    ##
    ##@in  nrep              : [integer] number of repetitions to use for estimating the
    ##                                   (average) execution time
    ##@in  time              : [boolean] whether to evaluate the execution time
    ##@in  short             : [boolean] whether to do short output (i.e. only the
    ##                                   objective function value). If short == FALSE,
    ##                                   the best subsample and the identified outliers are
    ##                                   printed. See also the parameter full below
    ##@in  full              : [boolean] whether to print the estimated center and covariance matrix
    ##@in  method            : [character] select a method: one of (FASTMCD, MASS)


    domcd <- function(x, xname, nrep = 1)
    {
        n <- dim(x)[1]
        p <- dim(x)[2]
        if(method == "MASS") {
            mcd <- MASS::cov.mcd(x)
            quan <- as.integer(floor((n + p + 1)/2)) #default: floor((n+p+1)/2)
        }
        else {
            mcd <- covMcd(x) # trace = FALSE
            quan <- as.integer(mcd$quan)
        }

        crit <- if(method == "MASS") mcd$crit else log(mcd$crit)

        xres <- sprintf("%*s %3d %3d %3d %12.6f", lname, xname, n, p, quan, crit)
        if(time) {
            xtime <- system.time(repMCD(x, nrep, method))[1]/nrep
            xres <- sprintf("%s %10.1f", xres, 1000 * xtime)
        }
        cat(xres, "\n")

        if(!short) {
            cat("Best subsample: \n")
            print(mcd$best)

            ibad <- which(mcd$mcd.wt == 0)
            names(ibad) <- NULL
            nbad <- length(ibad)
            cat("Outliers: ",nbad,"\n")
            if(nbad > 0)
                print(ibad)
            if(full) {
                cat("-------------\n")
                print(mcd)
            }
            cat("--------------------------------------------------------\n")
        }
    }

    lname <- 20
    method <- match.arg(method)

    if(method == "MASS" &&
       paste(R.version$major, R.version$minor, sep=".") < 2.3)
	cov.rob <- MASS::cov.rob

    data(heart)
    data(phosphor)
    data(starsCYG)
    data(stackloss)
    data(coleman)
    data(salinity)
    data(wood)
    data(hbk)

    data(Animals, package = "MASS")
    brain <- Animals[c(1:24, 26:25, 27:28),]
    data(milk)
    data(bushfire)

    ##    data(x1000)
    ##    data(x5000)

    tmp <- sys.call()
    cat("\nCall: ", deparse(substitute(tmp)),"\n")

    cat("Data Set               n   p  Half   LOG(obj)  Time [ms]\n")
    cat("========================================================\n")
    domcd(heart[, 1:2], data(heart), nrep)
    domcd(data.matrix(subset(phosphor, select = -plant)),
          data(phosphor), nrep)
    domcd(starsCYG, data(starsCYG), nrep)
    domcd(stack.x, data(stackloss), nrep)
    domcd(data.matrix(subset(coleman, select = -Y)), data(coleman), nrep)
    domcd(data.matrix(subset(salinity, select = -Y)), data(salinity), nrep)
    domcd(data.matrix(subset(wood, select = -y)), data(wood), nrep)
    domcd(data.matrix(subset(hbk,  select = -Y)), data(hbk), nrep)

    domcd(brain, "Animals", nrep)
    domcd(milk, data(milk), nrep)
    domcd(bushfire, data(bushfire), nrep)
    cat("========================================================\n")
    ##    domcd(x1000$X,data(x1000), nrep)
    ##    domcd(x5000$X,data(x5000), nrep)
}

if(FALSE){
    data(mortality, package = "riv")
    mm <- as.data.frame(lapply(mortality, signif, 3))
    for(j in c(1,2,6,7))
        mm[,j] <- mm[,j] * 10
    mm[,5] <- mm[,5] * 1000
    mm[,8] <- mm[,8] / 100
    mort3 <- mm
    dput(mort3)
}

mort3 <-
 data.frame(MO70 = c(140, 101, 86, 102, 115, 121, 118, 76.6,
            131, 112, 111, 112, 117, 118, 123, 122, 81.7, 108, 111, 109,
            92.5, 83.9, 93.8, 135, 124, 126, 122, 120, 127, 115, 156, 95.1,
            127, 129, 116, 82.3, 115, 106, 134, 94.9, 119, 111, 131, 85.6,
            135, 126, 141, 152, 137, 151, 93.6, 84.2, 78, 50.2, 81.3, 112,
            80.1, 125, 120, 143),

            MAGE = c(297, 277, 275, 268, 296, 327, 314, 258, 342, 278, 278,
            313, 284, 272, 296, 277, 271, 296, 286, 250, 280, 270, 246, 301,
            279, 287, 293, 271, 291, 295, 314, 267, 275, 307, 259, 251, 324,
            285, 288, 254, 278, 287, 316, 287, 326, 309, 334, 369, 321, 311,
            261, 272, 260, 244, 248, 277, 240, 295, 319, 346),

            CI68 = c(137, 137, 129, 129, 151, 157, 157, 157, 157, 202, 202, 202,
            138, 160, 190, 191, 191, 191, 159, 159, 146, 146, 203, 203, 182, 166,
            203, 203, 167, 167, 165, 153, 149, 149, 149, 157, 152, 183, 183, 183,
            183, 183, 183, 111, 171, 148, 148, 148, 192, 160, 160, 172, 172,
            172, 172, 101, 173, 173, 144, 181),

            MDOC = c(142, 80.4, 148, 167, 230, 187, 240, 149, 240, 195, 327,
            377, 203, 160, 161, 68.7, 141, 120, 176, 105, 128, 112, 98.9, 160,
            209, 200, 153, 126, 157, 157, 145, 160, 158, 102, 195, 188, 250,
            143, 157, 186, 114, 129, 129, 143, 186, 207, 144, 112, 157, 121,
            168, 155, 144, 144, 120, 194, 93.6, 231, 185, 89.7),

            DENS = c(37, 37, 27, 32, 17, 13, 23, 19, 27, 29, 15, 15, 48, 34,
            26, 47, 17, 10, 10, 18, 11, 13, 26, 19, 55, 17, 16, 7, 10, 17,
            44, 13, 18, 26, 40, 22, 29, 7, 28, 10, 15, 1, 11, 10, 8, 13, 13,
            6, 10, 26, 49, 28, 32, 18, 62, 15, 21, 18, 10, 12),

            NONW = c(4.22, 3.36, 0.67, 0.52, 2.51, 0.82, 4.07, 1.11,
            2.86, 2.92, 2.74, 1.05, 7.23, 5.16, 3.44, 2.84, 1.84,
            1.47, 0.62, 0.03, 0.96, 1.07, 1.74, 2.41, 0.45, 4.7, 4.45,
            1.2, 0.64, 2.28, 4.13, 1.06, 4.02, 2.22, 5.6, 0.43, 2.34,
            1.78, 2.81, 1.9, 3.09, 1.43, 2.58, 1.34, 0.78, 3.44, 2.07,
            0.68, 1, 3.6, 3.92, 2.58, 2.66, 0.05, 0.86, 0.32, 3.02,
            4.24, 1.26, 1.08),

            EDUC = c(454, 516, 601, 631, 565, 620, 661, 653, 661, 591,
            568, 499, 685, 534, 539, 536, 560, 542, 680, 546, 648,
            632, 601, 469, 458, 446, 521, 540, 661, 601, 480, 627,
            506, 363, 551, 662, 518, 556, 484, 607, 562, 517, 521,
            582, 629, 506, 534, 433, 459, 476, 492, 548, 517, 517,
            468, 685, 483, 471, 678, 528),

            IN69 = c(86.9, 99.3, 113, 99.2, 104, 118, 113, 117, 125,
            100, 104, 115, 122, 107, 135, 101, 123, 114, 114, 113,
            108, 109, 100, 99.8, 102, 100, 110, 112, 111, 113, 92.7,
            116, 86.3, 103, 86.4, 109, 116, 112, 104, 108, 103, 116,
            99.3, 116, 114, 104, 105, 97, 102, 83.4, 101, 125, 117,
            118, 90.3, 108, 92.4, 106, 126, 109))
