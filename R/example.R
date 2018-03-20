#' Get path to readroper example
#'
#' readroper comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' readroper_example()
#' readroper_example('testMultiCard.txt')
#' readroper_example('testSingleCard.txt')
readroper_example <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "readroper"))
    } else {
        system.file("extdata", path, package = "readroper", mustWork = TRUE)
    }
}
