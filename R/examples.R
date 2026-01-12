#' Get path to example files
#'
#' @param file Character. File name or path within extdata
#' @export
#' @examples
#' momit_example() # all examples
#' # a text like file would return path and be good for from_tps etc.
#' momit_example("tps/AE/data1.tps")
#'
#' # note that .rda example must be either embraced with load()
#' # or be piped with %>% load(envir = .GlobalEnv)
#' # this will work:
#' load(momit_example("Momocs/bot.rda"))
#' # this will also work:
#' momit_example("Momocs/bot.rda") %>% load(envir = .GlobalEnv)
#' # but this will NOT work:
#' momit_example("Momocs/bot.rda") %>% load()
#'
momit_example <- function(file = NULL) {
  if (is.null(file)) {
    # List all examples
    dir(system.file("extdata", package = "Momit"), recursive = TRUE)
  } else {
    system.file("extdata", file, package = "Momit", mustWork = TRUE)
  }
}
