#' @title Reads Roper Center datasets
#'
#' @description Reads fixed-width ASCII roper files.

#' Takes arguments in the format a Roper Center survey codebook provides
#'
#' @param col_positions starting position(s) to read
#' @param widths width of value(s) to read
#' @param col_names names of columns to return
#' @param filepath data obj or filepath to read
#' @param card_read card to read (if blank, reads a single card dataset)
#' @param cards number of cards in dataset (if null, assumes single card)
#' @return a dataframe with \code{len(col_names)} number of columns, assigned to the values of \code{col_names}
#' @export
#' @examples
#' fwf_sample <- readroper_example('testMultiCard.txt')
#' cat(readr::read_lines(fwf_sample))
#' fwf_sample2 <- readroper_example('testSingleCard.txt')
#' cat(readr::read_lines(fwf_sample2))
#' # 1. Fixed width file, first card, multi-card
#' read_rpr(col_positions=c(1,2,4), widths=c(1,2,1),
#' col_names=c('V1','V2','V3'), filepath=fwf_sample, card_read=1, cards=2)
#' # 2 .Fixed width file, first card, single card
#' read_rpr(col_positions=c(1,2,4), widths=c(1,2,1),
#' col_names=c('V1','V2','V3'), filepath=fwf_sample2)
#'  # 3. Fixed width file, second card, multi-card
#' read_rpr(col_positions=c(1,2,4), widths=c(1,2,1),
#' col_names=c('V1','V2','V3'), filepath=fwf_sample, card_read=2, cards=2)
#'
read_rpr <- function(col_positions = NaN, widths = NaN, col_names = NaN, filepath = NaN, card_read = NaN, cards = NaN) {

    if (is.nan(filepath)) {
        stop("File must be provided as path or object", call. = FALSE)
    } else if (is.nan(col_positions) || is.nan(widths) || is.nan(col_names)) {
        stop("Col positions, widths and names must be given", call. = FALSE)
    } else if (length(unique(sapply(list(widths, col_positions, col_names), length))) > 1) {
        stop("The lengths of the vectors of column names, widths, and positions must be the same", call. = FALSE)
    } else if ((is.nan(cards) & !is.nan(card_read)) || (!is.nan(cards) & is.nan(card_read) & as.numeric(card_read) !=
        1)) {
        stop("If reading a multi-card dataset, the number of cards and the card number to read from must be provided.",
            call. = FALSE)
    }

    widths <- as.numeric(widths)
    col_positions <- as.numeric(col_positions)

    end_positions <- col_positions + widths - 1

    if (!is.nan(cards)) {
        cards = as.numeric(cards)
    }

    if (!is.nan(card_read)) {
        card_read <- as.numeric(card_read)
    }

    if ((cards == 1) || (is.nan(cards) & is.nan(card_read))) {
        return(readr::read_fwf(filepath, readr::fwf_positions(col_positions, end_positions, col_names)))
    } else {
        card_v <- rep(FALSE, cards)
        card_v[card_read] <- TRUE
        return(readr::read_fwf(filepath, readr::fwf_positions(col_positions, end_positions, col_names))[card_v, ])
    }
}
