#' @title Reads Roper Center datasets
#'
#' @description Reads fixed-width ASCII roper files.

#' Takes arguments in the format a Roper Center survey codebook provides
#'
#' @param col_positions starting position(s) to read
#' @param widths width of value(s) to read
#' @param col_names names of columns to return
#' @param filepath data obj or filepath to read
#' @param card_read a vector of card(s) to read (if blank, reads a single card dataset)
#' @param cards number of cards in dataset (if null, assumes single card)
#' @return A data frame containing the variables specified in the \code{col_names} argument, plus a numeric \code{respondent} identifier and as many string \code{card} variables (\code{card1}, \code{card2}, ...) as specified by the \code{cards} argument.
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
read_rpr <- function(col_positions = NaN, widths = NaN, col_names = NaN, filepath = NaN, card_read = 1, cards = 1) {

  if (is.nan(filepath)) {
    stop("File must be provided as path or object", call. = FALSE)
  } else if (is.nan(col_positions) || is.nan(widths) || is.nan(col_names)) {
    stop("Col positions, widths and names must be given", call. = FALSE)
  } else if (length(unique(sapply(list(widths, col_positions, col_names), length))) > 1) {
    stop("The lengths of the vectors of column names, widths, and positions must be the same", call. = FALSE)
  } else if ((max(card_read) > max(cards))) {
    stop("If reading a multi-card dataset, the numbers of the cards to be read must not exceed the total number of cards.",
         call. = FALSE)
  }

  x <- read_ascii(file = filepath,
                  total_cards = cards,
                  var_names = col_names,
                  var_cards = card_read,
                  var_positions = col_positions,
                  var_widths = widths)
  return(x)
}
