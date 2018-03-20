
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(paste(pkgname, "package loaded.", sep = " "))
}
