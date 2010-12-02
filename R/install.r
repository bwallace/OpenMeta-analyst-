install.openmetar <- function() {
    # installs package openmetar and dependent packages igraph and metafor, if necessary
    
    R.version <- packageDescription("base")["Version"]
    valid.versions <- c("2.9.2", "2.10.0", "2.10.1", "2.11.0", "2.11.1")
    if (is.element(R.version, valid.versions)) {
        start_dir <- getwd()
        packages <- installed.packages()
        package.names <- names(packages[,1])
        type <- getOption("pkgType")
        local.pkgs <- NULL
        repo.pkgs <- NULL
        
        if (!is.element("igraph", package.names)) {
            repo.pkgs <- c("igraph")
        }
        if (!is.element("metafor", package.names)) {
            if (is.element(R.version, c("2.9.2", "2.10.0", "2.10.1", "2.11.0", "2.11.1"))) {
                repo.pkgs <- c(repo.pkgs, "metafor")
            }
            else {
                if (type == "win.binary") {
                    local.pkgs <- c(local.pkgs, "metafor_0.5-7.zip")
                }
                if (type == "source") {
                    local.pkgs <- c(local.pkgs, "metafor_0.5-7.tar.gz")
                }
            }
        } 
        if (!is.element("openmetar", package.names)) {
            if (type == "win.binary") {
                local.pkgs <- c(local.pkgs, "openmetar_1.0.zip")
            }
            if (type == "source") {
                local.pkgs <- c(local.pkgs, "openmetar_1.0.tar.gz")
            }
        }    
        if (length(repo.pkgs) > 0) {
            # install packages from repository, if necessary
            install.packages(repo.pkgs)
        }
        setwd(start_dir)
        if (length(local.pkgs > 0)) {
            install.packages(local.pkgs, repo=NULL)
        }
    }
    else {
        cat("Your version of R is not compatible with openmetar.\n")
        cat("Compatible versions are 2.9.2, 2.10.0, 2.10.1, 2.11.0, and 2.11.1.\n")
    }
}
install.openmetar()

