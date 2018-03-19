#' @title Reads Roper Center datasets
#'
#' @description Reads fixed-width ASCII roper files.
#'

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(paste(pkgname, "package loaded.", sep = " "))
}

#' Takes arguments in the format a Roper Center survey codebook provides
#' @param col_positions
#' @param widths
#' @param col_names
#' @param filepath
#' @return a dataframe with \code{len(col_names)} number of columns, assigned to the values of \code{col_names}
#'
#' @examples read_rpr(c('1','2','4'), c('1','2','1'), c('V1','V2','V3'), './', 1)
#' @examples read_rpr(c('1','2','4'), c('1','2','1'), c('V1','V2','V3'), './')
#' @examples read_rpr(c('2','1','4'), c('2','1','1'), c('V2','V1','V3'), './', 2)


#' @export read_rpr
read_rpr <- function(col_positions, widths, col_names, filepath, card_read, cards) {
    
    if (length(unique(list(col_names, col_positions, widths))) == 1) {
        print("The lengths of the vectors of column names, widths, and positions must be the same")
        break
    }
    end_positions <- col_positions + widths - 1
    
    if (is.NaN(cards)) {
        return(read_fwf(filepath, fwf_positions(col_positions, end_positions, col_names)))
    } else {
        card_v <- rep(FALSE, cards)
        card_v(c(card_read)) <- FALSE
        return(read_fwf(filepath, fwf_positions(col_positions, end_positions, col_names)))[card_v, ]
    }
}
