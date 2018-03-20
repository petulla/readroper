#' @title Reads Roper Center datasets
#'
#' @description Reads fixed-width ASCII roper files.

#' Takes arguments in the format a Roper Center survey codebook provides
#' @param col_positions
#' @param widths
#' @param col_names
#' @param filepath
#' @param card_read
#' @param cards
#' @return a dataframe with \code{len(col_names)} number of columns, assigned to the values of \code{col_names}
#' @export
#' @examples
#' fwf_sample <- readr_example('testMultiCard.txt')
#' cat(read_lines(fwf_sample))
#' fwf_sample2 <- readr_example('testSingleCard.txt')
#' cat(read_lines(fwf_sample2))
#' # 1. Fixed width file, first card, multi-card
#' read_rpr(c(1,2,4), c(1,2,1), c('V1','V2','V3'), fwf_sample, 1, 2)
#' # 2 .Fixed width file, first card, single card
#' read_rpr(c(1,2,4), c(1,2,1), c('V1','V2','V3'), fwf_sample2)
#' # 3. Fixed width file, second card, multi-card
#' read_rpr(c(2,1,4), c(2,1,1), c('V2','V1','V3'), fwf_sample, 2, 2)
#'
read_rpr <- function(col_positions, widths, col_names, filepath, card_read, cards) {

    if (is.nan(filepath)) {
        print("File must be provided as path or object")
        break
    } else if (is.nan(col_positions) || is.nan(widths) || is.nan(col_names)) {
        print("Col positions, widths and names must be given")
        break
    } else if (length(unique(list(col_names, col_positions, widths))) == 1) {
        print("The lengths of the vectors of column names, widths, and positions must be the same")
        break
    } else if ((is.nan(cards) && !is.nan(card_read)) || (!is.nan(cards) && is.nan(card_read))) {
        print("If reading a multi-card dataset, the number of cards and the card number to read from must be provided.")
        break
    }

    end_positions <- col_positions + widths - 1

    if (is.nan(cards) || is.nan(card_read)) {
        return(readr::read_fwf(filepath, readr::fwf_positions(col_positions, end_positions, col_names)))
    } else {
        card_v <- rep(FALSE, cards)
        card_v[card_read] <- TRUE
        return(readr::read_fwf(filepath, readr::fwf_positions(col_positions, end_positions, col_names)))[card_v, ]
    }
}
