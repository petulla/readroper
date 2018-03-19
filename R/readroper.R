#' @title Reads Roper Center datasets
#'
#' @description Reads fixed-width ASCII roper files.
#'

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(pkgname,"package loaded.",sep=" "))
}

#' Takes arguments in the format a Roper Center survey codebook provides
#' @param col_positions
#' @param widths
#' @param col_names
#' @param filepath
#' @return a dataframe with \code{len(col_names)} number of columns, assigned to the values of \code{col_names}
#' @examples read_rpr(c(), c(), c(), './', 1)
#' @examples read_rpr(c(), c(), c(), './')
#' @examples read_rpr(c(), c(), c(), './', 3)


#' @export read_rpr
read_rpr <- function(col_positions, widths, col_names, filepath, cards) {

    if (len(col_names) != len(col_positions) != len(widths)) {
      print('The lengths of the vectors of column names, widths, and positions must be the same')
      break
    }
    end_positions <- col_positions + widths - 1
    print(end_positions)
    if (is.NaN(cards) || cards == 1) {
      return(read_fwf(filepath, fwf_positions(col_positions, end_positions, col_names)))
    } else {

    }
}
